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
                .value_parser(value_parser!(PathBuf)),
        )
        .arg(
            arg!(--file <file_path>)
                .id("file")
                .value_parser(value_parser!(PathBuf)),
        )
        .get_matches();

    if let Some(path) = matches.get_one::<PathBuf>("file") {
        let errors = sqf::check(path);
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
        for (_, mut path) in functions {
            path = path.replace('\\', "/");
            let mut base = directory.clone();
            base.pop();

            let path: PathBuf = base.join(path);
            let errors = sqf::check(&path);
            if !errors.is_empty() {
                print_errors(errors, &path)
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

    errors.sort_by_key(|x| x.span.0);

    let mut current: Span = Default::default();

    let mut opened = HashMap::<&str, Position>::default();

    for (i, c) in buffer.iter().enumerate() {
        let c = c.unwrap();
        current.push(c, &metrics);
        for error in &errors {
            if i == error.span.0 - 1 {
                opened.insert(error.inner.as_str(), current.start());
            }
            if i == error.span.1 - 1 {
                let start = opened.remove(error.inner.as_str()).unwrap();
                let span = Span::new(start, current.last(), current.end());
                fmt.add(
                    span,
                    Some(errors[0].inner.clone()),
                    source_span::fmt::Style::Error,
                );
            }
        }
    }

    let formatted = fmt.render(buffer.iter(), buffer.span(), &metrics).unwrap();
    println!("{}", formatted);
}
