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
