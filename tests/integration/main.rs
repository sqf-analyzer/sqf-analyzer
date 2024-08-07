use std::path::PathBuf;

use sqf::{analyzer::Configuration, cpp::analyze_file};

mod analyser;
mod call;
mod cpp;
mod params;
mod parser;
mod preprocessor;

#[test]
fn check_is_ok() {
    let path = std::path::PathBuf::from("./tests/integration/examples/basic_if.sqf");
    let configuration = Configuration {
        file_path: path.into(),
        base_path: "./tests/integration/examples".into(),
        ..Default::default()
    };
    sqf::check(configuration, Default::default(), Default::default()).unwrap();
}

#[test]
fn check_addon() {
    let path: PathBuf = "tests/integration/case_sensitive/addons/main/config.cpp".into();
    let configuration = sqf::preprocessor::Configuration::with_path(path.clone());

    let (functions, e) = analyze_file(configuration).unwrap();
    assert_eq!(e, vec![]);

    for (_, function_path) in functions {
        let Ok(path) = sqf::get_path(&function_path.inner, &path, &Default::default()) else {
            panic!()
        };

        let configuration = Configuration {
            file_path: path,
            base_path: "tests/integration/case_sensitive".into(),
            ..Default::default()
        };

        let e = sqf::check(configuration, Default::default(), Default::default()).unwrap();
        assert_eq!(e.errors, vec![]);
    }
}
