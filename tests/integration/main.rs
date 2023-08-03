mod analyser;
mod params;
mod parser;

#[test]
fn pairs() {
    use std::fs;
    let path = "tests/integration/examples/antistasi.cpp";
    let case = fs::read_to_string(path).unwrap();

    sqf::preprocessor::parse(sqf::preprocessor::pairs(&case).unwrap());
}

#[test]
fn tokens() {
    use std::fs;
    let path = "tests/integration/examples/basic.cpp";
    let case = fs::read_to_string(path).unwrap();

    sqf::preprocessor::parse(sqf::preprocessor::pairs(&case).unwrap());
}

#[test]
fn tokens1() {
    use std::fs;
    let path = "tests/integration/examples/basic_if.sqf";
    let case = fs::read_to_string(path).unwrap();

    sqf::preprocessor::parse(sqf::preprocessor::pairs(&case).unwrap());
}

#[test]
fn tokens2() {
    use std::fs;
    let path = "tests/integration/examples/basic.cpp";
    let case = fs::read_to_string(path).unwrap();
    let expected = fs::read_to_string("tests/integration/examples/expected_basic.txt").unwrap();

    let ast = sqf::preprocessor::parse(sqf::preprocessor::pairs(&case).unwrap());

    assert_eq!(format!("{ast:#?}"), expected);
}

#[test]
fn define() {
    use std::fs;
    let path = "tests/integration/examples/basic_def.sqf";
    let case = fs::read_to_string(path).unwrap();
    let expected = fs::read_to_string("tests/integration/examples/expected_basic_def.txt").unwrap();

    let ast = sqf::preprocessor::parse(sqf::preprocessor::pairs(&case).unwrap());

    assert_eq!(format!("{ast:#?}"), expected);
}
