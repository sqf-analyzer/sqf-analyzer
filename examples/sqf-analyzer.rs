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

fn main() -> Result<(), String> {
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
        .arg(
            arg!(--addons <kv>)
                .id("addons")
                .value_parser(parse_key_val::<String, String>)
                .help("key=value representing the pboprefix of paths. For example, \"/x/cba/addons\"=\"include/x/cba/addons\""),
        )
        .get_matches();

    let addons = matches
        .get_many::<(String, String)>("addons")
        .unwrap_or_default()
        .cloned()
        .map(|(k, v)| (k.into(), PathBuf::from(v).into()))
        .collect::<HashMap<_, _>>();

    if let Some(path) = matches.get_one::<PathBuf>("sqf") {
        let configuration = analyzer::Configuration {
            file_path: path.clone().into(),
            base_path: "".into(),
            addons: addons.clone(),
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
        let configuration = preprocessor::Configuration {
            path: mission_path.clone().into(),
            addons: addons.clone(),
            ..Default::default()
        };

        let (functions, errors) = match cpp::analyze_file(configuration) {
            Ok((functions, errors)) => (functions, errors),
            Err(error) => {
                return Err(error);
            }
        };

        println!("{} functions found and being analyzed", functions.len());
        if !errors.is_empty() {
            print_errors(errors, &mission_path)
        }
        return process(&mission_path, addons.clone(), &functions);
    }

    if let Some(path) = matches.get_one::<PathBuf>("config") {
        let configuration = preprocessor::Configuration {
            path: path.clone().into(),
            addons: addons.clone(),
            ..Default::default()
        };

        let (_, errors) = match cpp::analyze_file(configuration) {
            Ok((functions, errors)) => (functions, errors),
            Err(error) => {
                return Err(error);
            }
        };
        if !errors.is_empty() {
            print_errors(errors, path)
        }
    }

    if let Some(directory) = matches.get_one::<PathBuf>("addon") {
        let addon_path = directory.join("config.cpp");
        let configuration = preprocessor::Configuration {
            path: addon_path.clone().into(),
            addons: addons.clone(),
            ..Default::default()
        };

        let (functions, errors) = match cpp::analyze_file(configuration) {
            Ok((functions, errors)) => (functions, errors),
            Err(error) => {
                return Err(error);
            }
        };

        println!("{} functions found and being analyzed", functions.len());
        if !errors.is_empty() {
            print_errors(errors, &addon_path)
        }
        return process(&addon_path, addons, &functions);
    } else {
        Ok(())
    }
}

enum Either {
    Original(String),
    Path(Arc<Path>),
}

fn process(
    addon_path: &Path,
    addons: HashMap<Arc<str>, PathBuf>,
    functions: &Functions,
) -> Result<(), String> {
    let functions = functions.iter().map(|(function_name, sqf_path)| {
        let path = get_path(&sqf_path.inner, addon_path, &addons).ok();
        (
            Some(function_name.clone()),
            path.map(Either::Path)
                .unwrap_or(Either::Original(sqf_path.inner.clone())),
        )
    });
    // iterator over default files analyze
    let defaults = MISSION_INIT_SCRIPTS
        .into_iter()
        .chain(["XEH_preInit.sqf", "XEH_preStart.sqf", "XEH_postInit.sqf"])
        .map(|file| {
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
                addons: addons.clone(),
                ..Default::default()
            };

            // todo: add origins from functions that we know it is of type "Code"
            let text = std::fs::read_to_string(&configuration.file_path).ok()?;
            sqf::check_content(&text, configuration, Default::default(), Default::default())
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
        .flat_map(|(function_name, path)| {
            let path = match path {
                Either::Original(original) => {
                    let error = Error::new(
                        format!(
                            "Could not find path \"{}\" of function declared in addon",
                            original
                        ),
                        (1, 1),
                    );
                    return vec![error];
                }
                Either::Path(path) => path,
            };
            let Ok(text) = std::fs::read_to_string(&path) else {
                if function_name.is_some() {
                    let error = Error::new(format!(
                        "Could not open path \"{}\"",
                        path.display()
                    ), (1, 1));
                    return vec![error];
                }
                // optional files are not error if not found
                return vec![]
            };

            let configuration = analyzer::Configuration {
                file_path: path,
                base_path: addon_path.to_owned(),
                addons: addons.clone(),
                ..Default::default()
            };

            sqf::check_content(&text, configuration, mission.clone(), Settings {})
                .map(|s| s.errors)
                .unwrap_or_else(|e| vec![e])
        })
        .fold(HashMap::<_, Vec<_>>::default(), |mut acc, error| {
            if let Some(origin) = &error.origin {
                acc.entry(origin.clone()).or_default().push(error);
            } else {
                println!("{:?}", error);
            }
            acc
        });

    let len_files = errors.len();
    let len_errors = errors.values().map(|x| x.len()).sum::<usize>();
    for (file_path, errors) in errors {
        print_errors(errors, &file_path)
    }
    if len_errors > 0 {
        Err(format!(
            "{len_errors} errors found across {len_files} files"
        ))
    } else {
        Ok(())
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

    let diagnostics = errors.into_iter().map(|error| {
        let d = match error.type_ {
            ErrorType::PrivateAssignedToMission => Diagnostic::warning(),
            ErrorType::UnusedVariable => Diagnostic::note(),
            _ => Diagnostic::bug(),
        };
        d.with_labels(vec![Label::primary(file_id, error.span.0..error.span.1)
            .with_message(error.type_.to_string())])
    });

    let writer = StandardStream::stderr(ColorChoice::Auto);
    let config = codespan_reporting::term::Config::default();
    for diagnostic in diagnostics {
        term::emit(&mut writer.lock(), &config, &files, &diagnostic).unwrap();
    }
}

use std::error::Error as StdError;

/// Parse a single key-value pair
fn parse_key_val<T, U>(s: &str) -> Result<(T, U), Box<dyn StdError + Send + Sync + 'static>>
where
    T: std::str::FromStr,
    T::Err: StdError + Send + Sync + 'static,
    U: std::str::FromStr,
    U::Err: StdError + Send + Sync + 'static,
{
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid KEY=value: no `=` found in `{s}`"))?;
    Ok((s[..pos].parse()?, s[pos + 1..].parse()?))
}
