use std::path::PathBuf;

use sqf::cpp::analyze_file;

mod analyser;
mod call;
mod cpp;
mod params;
mod parser;
mod preprocessor;

#[test]
fn check_is_ok() {
    let path = std::path::PathBuf::from("./tests/integration/examples/basic_if.sqf");
    sqf::check(&path, Default::default()).unwrap();
}

#[test]
fn check_addon() {
    let path: PathBuf = "tests/integration/case_sensitive/addons/main/config.cpp".into();

    let (functions, e) = analyze_file(path.clone()).unwrap();
    assert_eq!(e, vec![]);

    for (_, function_path) in functions {
        let Ok(path) = sqf::get_path(&function_path.inner,path.clone()) else {
            panic!()
        };
        let e = sqf::check(&path, Default::default()).unwrap();
        assert_eq!(e.errors, vec![]);
    }
}
