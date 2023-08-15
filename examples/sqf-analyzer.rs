use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use clap::{arg, value_parser, Command};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::ColorChoice;
use codespan_reporting::term::termcolor::StandardStream;

use sqf::cpp::Functions;
use sqf::get_path;
use sqf::preprocessor::Configuration;
use sqf::{cpp, error::Error};

fn main() {
    let matches = Command::new("sqf-analyzer")
        .author("LordGolias <lord.golias1@gmail.com>")
        .about("Linter of SQF (Arma 3)") // requires `cargo` feature
        .arg(
            arg!(--addon <directory>)
                .id("addon")
                .value_parser(value_parser!(PathBuf))
                .help("Path of directory containing a config.cpp"),
        )
        .arg(
            arg!(--sqf <file_path>)
                .id("sqf")
                .value_parser(value_parser!(PathBuf))
                .help("Path of an sqf file"),
        )
        .arg(
            arg!(--config <file_path>)
                .id("config")
                .value_parser(value_parser!(PathBuf))
                .help("Path of a directory containing description.ext"),
        )
        .arg(
            arg!(--mission <file_path>)
                .id("mission")
                .value_parser(value_parser!(PathBuf))
                .help("Path of a directory containing a description.ext"),
        )
        .get_matches();

    if let Some(path) = matches.get_one::<PathBuf>("sqf") {
        match sqf::check(path, Default::default()) {
            Ok(state) => {
                if !state.errors.is_empty() {
                    print_errors(state.errors, path)
                }
            }
            Err(e) => {
                println!("{}", e.inner);
            }
        }
    }

    if let Some(directory) = matches.get_one::<PathBuf>("mission") {
        let mission_path = directory.join("description.ext");
        let configuration = Configuration::with_path(mission_path.clone());

        let (functions, errors) = match cpp::analyze_file(configuration) {
            Ok((functions, errors)) => (functions, errors),
            Err(error) => {
                println!("{error}");
                return;
            }
        };

        println!("{} functions found and being analyzed", functions.len());
        if !errors.is_empty() {
            print_errors(errors, &mission_path)
        }
        process(&mission_path, &functions)
    }

    if let Some(path) = matches.get_one::<PathBuf>("config") {
        let configuration = Configuration::with_path(path.clone());

        let (_, errors) = match cpp::analyze_file(configuration) {
            Ok((functions, errors)) => (functions, errors),
            Err(error) => {
                println!("{error}");
                return;
            }
        };
        if !errors.is_empty() {
            print_errors(errors, path)
        }
    }

    if let Some(directory) = matches.get_one::<PathBuf>("addon") {
        let addon_path = directory.join("config.cpp");
        let configuration = Configuration::with_path(addon_path.clone());

        let (functions, errors) = match cpp::analyze_file(configuration) {
            Ok((functions, errors)) => (functions, errors),
            Err(error) => {
                println!("{error}");
                return;
            }
        };

        println!("{} functions found and being analyzed", functions.len());
        if !errors.is_empty() {
            print_errors(errors, &addon_path)
        }
        process(&addon_path, &functions)
    }
}

fn process(addon_path: &Path, functions: &Functions) {
    let configuration = Configuration::with_path(addon_path.to_owned());

    // first pass to get the global states
    let states = functions
        .iter()
        .filter_map(|(function_name, sqf_path)| {
            let path = get_path(&sqf_path.inner, &configuration).ok()?;

            sqf::check(&path, Default::default())
                .map(|state| (function_name, state))
                .ok()
        })
        .collect::<HashMap<_, _>>();

    // second pass to get errors
    for (function_name, path) in functions {
        let Ok(path) = get_path(&path.inner, &configuration) else {
            println!("Could not find path \"{}\" of function declared in addon", path.inner);
            continue
        };

        let mission = states
            .iter()
            .filter(|x| x.0.as_ref() != function_name.as_ref())
            .flat_map(|(function_name, state)| state.globals((*function_name).clone()))
            .collect();

        match sqf::check(&path, mission) {
            Ok(state) => {
                if !state.errors.is_empty() {
                    print_errors(state.errors, &path)
                }
            }
            Err(e) => {
                println!("{}", e.inner);
            }
        }
    }
}

fn print_errors(errors: Vec<Error>, path: &Path) {
    let Ok(content) = std::fs::read_to_string(path) else {
        println!("File {} not found", path.display());
        return;
    };
    if !errors.is_empty() {
        println!("{} has {} errors", path.display(), errors.len());
    } else {
        return;
    }

    let mut files = SimpleFiles::new();

    let file_id = files.add(path.display().to_string(), content);

    let diagnostics = &[Diagnostic::error().with_labels(
        errors
            .into_iter()
            .map(|error| {
                Label::primary(file_id, error.span.0..error.span.1).with_message(error.inner)
            })
            .collect(),
    )];

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();
    for diagnostic in diagnostics {
        term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
    }
}
