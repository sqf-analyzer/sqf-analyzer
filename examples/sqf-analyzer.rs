use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use clap::{arg, value_parser, Command};

use source_span::DEFAULT_METRICS;
use source_span::{Position, Span};
use sqf::cpp::Functions;
use sqf::get_path;
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

        let (functions, errors) = match cpp::analyze_file(mission_path.clone()) {
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
        let (_, errors) = match cpp::analyze_file(path.clone()) {
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
        let (functions, errors) = match cpp::analyze_file(addon_path.clone()) {
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
    // first pass to get the global states
    let states = functions
        .iter()
        .filter_map(|(function_name, sqf_path)| {
            let path = get_path(&sqf_path.inner, addon_path.to_owned()).ok()?;

            sqf::check(&path, Default::default())
                .map(|state| (function_name, state))
                .ok()
        })
        .collect::<HashMap<_, _>>();

    // second pass to get errors
    for (function_name, path) in functions {
        let Ok(path) = get_path(&path.inner, addon_path.to_owned()) else {
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

fn print_errors(mut errors: Vec<Error>, path: &Path) {
    let Ok(content) = std::fs::read_to_string(path) else {
        println!("File {} not found", path.display());
        return;
    };
    if !errors.is_empty() {
        println!("{} has {} errors", path.display(), errors.len());
    } else {
        return;
    }
    let chars = content.chars().map(Result::<char, String>::Ok);

    let metrics = DEFAULT_METRICS;
    let mut fmt = source_span::fmt::Formatter::with_margin_color(source_span::fmt::Color::Blue);
    let buffer = source_span::SourceBuffer::new(chars, source_span::Position::default(), metrics);

    errors.sort_by_key(|x| x.span);
    errors.dedup_by_key(|x| x.span);

    let mut current: Span = Default::default();

    let mut opened = HashMap::<sqf::span::Span, Position>::default();

    let mut bytes = 0;
    for c in buffer.iter() {
        let c = c.unwrap();
        bytes += c.len_utf8();
        current.push(c, &metrics);
        for error in &errors {
            if bytes == error.span.0.saturating_add(1) {
                opened.insert(error.span, current.last());
            }
            if bytes == error.span.1 {
                let start = opened.remove(&error.span).unwrap();
                let span = Span::new(start, current.last(), current.end());
                fmt.add(
                    span,
                    Some(error.inner.clone()),
                    source_span::fmt::Style::Error,
                );
            }
        }
    }

    let formatted = fmt.render(buffer.iter(), buffer.span(), &metrics).unwrap();
    println!("{}", formatted);
}
