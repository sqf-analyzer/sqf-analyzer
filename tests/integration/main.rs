use std::path::PathBuf;

use sqf::cpp::analyze_addon;

mod analyser;
mod call;
mod cpp;
mod params;
mod parser;
mod preprocessor;

#[test]
fn check_is_ok() {
    let path = std::path::PathBuf::from("./tests/integration/examples/basic_if.sqf");
    sqf::check(&path);
}

#[test]
fn check_addon() {
    let path: PathBuf = "tests/integration/case_sensitive/addons/main".into();

    let (functions, e) = analyze_addon(path.clone()).unwrap();
    assert_eq!(e, vec![]);

    for (_, function_path) in functions {
        let Ok(path) = sqf::get_path(&function_path.inner,path.join("config.cpp").clone()) else {
            panic!()
        };
        let e = sqf::check(&path);
        assert_eq!(e, vec![]);
    }
}
