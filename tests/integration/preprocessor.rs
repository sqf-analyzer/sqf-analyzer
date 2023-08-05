use sqf::preprocessor::tokens;

#[test]
fn pairs_() {
    use std::fs;
    let path = "tests/integration/examples/antistasi.cpp";
    let case = fs::read_to_string(path).unwrap();

    tokens(&case, Default::default(), Default::default()).unwrap();
}

#[test]
fn basic_functionality() {
    use std::fs;
    let path = "tests/integration/examples/basic.cpp";
    let case = fs::read_to_string(path).unwrap();

    tokens(&case, Default::default(), Default::default()).unwrap();
}

#[test]
fn tokens1() {
    use std::fs;
    let path = "tests/integration/examples/basic_if.sqf";
    let case = fs::read_to_string(path).unwrap();

    let mut ast = tokens(&case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(
        r,
        vec!["if", "a", "then", "{", "b", "}", "else", "{", "c", "}", ";", "d"]
    );
}

#[test]
fn tokens2() {
    use std::fs;
    let path = "tests/integration/examples/basic.cpp";
    let case = fs::read_to_string(path).unwrap();

    let mut ast = tokens(&case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    let expected: Vec<&str> = vec![];

    assert_eq!(r, expected);
}

#[test]
fn define() {
    let case = r#"#define KEY_PARAM(KEY,NAME,DEF_VALUE) \
private #NAME; \
NAME = [toLower KEY, toUpper KEY, DEF_VALUE, RETNIL(_this)] call CBA_fnc_getArg; \
TRACE_3("KEY_PARAM",KEY,NAME,DEF_VALUE)
"#;

    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    let expected: Vec<&str> = vec![];

    assert_eq!(r, expected);
}

#[test]
fn define_use_with_args() {
    let case = "#define A(B) var##B\n1 + A(1);";

    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, vec!["1", "+", "var1", ";"]);
}

#[test]
fn define_use() {
    let case = "#define A (1 + 1)\n1 + A";

    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, vec!["1", "+", "(", "1", "+", "1", ")"]);
}

#[test]
fn antistasi() {
    use std::fs;
    let path = "tests/integration/examples/antistasi.cpp";
    let case = fs::read_to_string(path).unwrap();

    let mut ast = tokens(&case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    let expected: Vec<&str> = vec![];

    assert_eq!(r, expected);
}
