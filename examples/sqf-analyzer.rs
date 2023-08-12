use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use clap::{arg, value_parser, Command};

use source_span::DEFAULT_METRICS;
use source_span::{Position, Span};
use sqf::{cpp, error::Error};

fn main() {
    let matches = Command::new("sqf-analyzer")
        .author("LordGolias <lord.golias1@gmail.com>")
        .about("Linter of SQF (Arma 3)") // requires `cargo` feature
        .arg(
            arg!(--addon <directory>)
                .id("directory")
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
        let errors = sqf::check(path);
        if !errors.is_empty() {
            print_errors(errors, path)
        }
    }

    if let Some(directory) = matches.get_one::<PathBuf>("mission") {
        let (functions, errors) = match cpp::analyze_mission(directory.into()) {
            Ok((functions, errors)) => (functions, errors),
            Err(error) => {
                println!("{error}");
                return;
            }
        };
        if !errors.is_empty() {
            print_errors(errors, &directory.join("description.ext"))
        }
        for (_, path) in functions {
            let errors = sqf::check(&path.inner);
            if !errors.is_empty() {
                print_errors(errors, &path.inner)
            }
        }
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

    if let Some(directory) = matches.get_one::<PathBuf>("directory") {
        let (functions, errors) = match cpp::analyze_addon(directory.into()) {
            Ok((functions, errors)) => (functions, errors),
            Err(error) => {
                println!("{error}");
                return;
            }
        };
        if !errors.is_empty() {
            print_errors(errors, &directory.join("config.cpp"))
        }
        for (_, path) in functions {
            let errors = sqf::check(&path.inner);
            if !errors.is_empty() {
                print_errors(errors, &path.inner)
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

    for (i, c) in buffer.iter().enumerate() {
        let c = c.unwrap();
        current.push(c, &metrics);
        for error in &errors {
            if i == error.span.0 {
                opened.insert(error.span, current.last());
            }
            if i == error.span.1.saturating_sub(1) {
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
