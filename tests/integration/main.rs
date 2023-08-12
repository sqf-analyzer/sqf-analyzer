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
    let (functions, e) =
        analyze_addon("tests/integration/case_sensitive/addons/main".into()).unwrap();
    assert_eq!(e, vec![]);

    for (_, path) in functions {
        let e = sqf::check(&path.inner);
        assert_eq!(e, vec![]);
    }
}
