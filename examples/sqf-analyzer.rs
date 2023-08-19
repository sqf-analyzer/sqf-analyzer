use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;
use std::sync::Arc;

use clap::{arg, value_parser, Command};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use codespan_reporting::files::SimpleFiles;
use codespan_reporting::term;
use codespan_reporting::term::termcolor::ColorChoice;
use codespan_reporting::term::termcolor::StandardStream;

use sqf::analyzer;
use sqf::analyzer::MissionNamespace;
use sqf::analyzer::Settings;
use sqf::cpp::Functions;
use sqf::error::ErrorType;
use sqf::get_path;
use sqf::preprocessor;
use sqf::UncasedStr;
use sqf::MISSION_INIT_SCRIPTS;
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
        let configuration = analyzer::Configuration {
            file_path: path.clone().into(),
            base_path: "".into(),
            ..Default::default()
        };

        match sqf::check(configuration, Default::default(), Default::default()) {
            Ok(state) => {
                if !state.errors.is_empty() {
                    print_errors(state.errors, path)
                }
            }
            Err(e) => {
                println!("{}", e.type_.to_string());
            }
        }
    }

    if let Some(directory) = matches.get_one::<PathBuf>("mission") {
        let mission_path = directory.join("description.ext");
        let configuration = preprocessor::Configuration::with_path(mission_path.clone());

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
        let configuration = preprocessor::Configuration::with_path(path.clone());

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
        let configuration = preprocessor::Configuration::with_path(addon_path.clone());

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

enum Either {
    Original(String),
    Path(Arc<Path>),
}

fn process(addon_path: &Path, functions: &Functions) {
    let functions = functions.iter().map(|(function_name, sqf_path)| {
        let path = get_path(&sqf_path.inner, addon_path, &Default::default()).ok();
        (
            Some(function_name.clone()),
            path.map(Either::Path)
                .unwrap_or(Either::Original(sqf_path.inner.clone())),
        )
    });
    // iterator over default files analyze
    let defaults = MISSION_INIT_SCRIPTS.into_iter().map(|file| {
        let mut directory = addon_path.to_owned();
        directory.pop();
        let path: Arc<Path> = directory.join(file).into();
        (None::<Arc<UncasedStr>>, Either::Path(path))
    });

    // iterator over all relevant files to analyze
    let files = functions.chain(defaults);

    // vector of all (path, states)
    let states = files
        .clone()
        .filter_map(|(function_name, path)| {
            let path = match path {
                Either::Original(_) => {
                    return None;
                }
                Either::Path(path) => path,
            };
            let configuration = analyzer::Configuration {
                file_path: path.clone(),
                base_path: addon_path.to_owned(),
                ..Default::default()
            };

            // todo: add origins from functions that we know it is of type "Code"
            sqf::check(configuration, Default::default(), Default::default())
                .map(|state| (path, (function_name, state)))
                .ok()
        })
        .collect::<Vec<_>>();

    let mission: MissionNamespace = states
        .iter()
        .flat_map(|(_, (function_name, state))| state.globals(function_name.clone()))
        .collect();

    // second pass to get errors
    let errors = files
        .flat_map(|(_, path)| {
            let path = match path {
                Either::Original(original) => {
                    println!(
                        "Could not find path \"{}\" of function declared in addon",
                        original
                    );
                    return vec![];
                }
                Either::Path(path) => path,
            };

            let configuration = analyzer::Configuration {
                file_path: path,
                base_path: addon_path.to_owned(),
                ..Default::default()
            };

            let state = sqf::check(configuration, mission.clone(), Settings {})
                .map_err(|e| println!("{}", e.type_.to_string()));
            state.map(|s| s.errors).unwrap_or_default()
        })
        .fold(HashMap::<_, Vec<_>>::default(), |mut acc, error| {
            acc.entry(error.origin.clone().unwrap())
                .or_default()
                .push(error);
            acc
        });

    for (file_path, errors) in errors {
        print_errors(errors, &file_path)
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
                if error.type_ == ErrorType::PrivateAssignedToMission {
                    Label::secondary(file_id, error.span.0..error.span.1)
                } else {
                    Label::primary(file_id, error.span.0..error.span.1)
                }
                .with_message(error.type_.to_string())
            })
            .collect(),
    )];

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();
    for diagnostic in diagnostics {
        term::emit(&mut writer.lock(), &config, &files, diagnostic).unwrap();
    }
}
