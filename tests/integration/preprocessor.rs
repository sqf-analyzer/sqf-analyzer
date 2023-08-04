use sqf::preprocessor::{pairs, parse, AstIterator};

#[test]
fn pairs_() {
    use std::fs;
    let path = "tests/integration/examples/antistasi.cpp";
    let case = fs::read_to_string(path).unwrap();

    parse(pairs(&case).unwrap());
}

#[test]
fn tokens() {
    use std::fs;
    let path = "tests/integration/examples/basic.cpp";
    let case = fs::read_to_string(path).unwrap();

    parse(pairs(&case).unwrap());
}

#[test]
fn tokens1() {
    use std::fs;
    let path = "tests/integration/examples/basic_if.sqf";
    let case = fs::read_to_string(path).unwrap();
    let expected = fs::read_to_string("tests/integration/examples/expected_basic_if.txt").unwrap();

    let ast = parse(pairs(&case).unwrap());

    assert_eq!(format!("{ast:#?}"), expected);

    let r = AstIterator::new(ast, Default::default(), Default::default())
        .map(|x| x.inner)
        .collect::<Vec<_>>();

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
    let expected = fs::read_to_string("tests/integration/examples/expected_basic.txt").unwrap();

    let ast = parse(pairs(&case).unwrap());

    assert_eq!(format!("{ast:#?}"), expected);

    let r = AstIterator::new(ast, Default::default(), Default::default())
        .map(|x| x.inner)
        .collect::<Vec<_>>();
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
    parse(pairs(case).unwrap());
}

#[test]
fn define_use_with_args() {
    let case = "#define A(B) var##B\n1 + A(1);";

    let ast = parse(pairs(case).unwrap());

    let r = AstIterator::new(ast, Default::default(), Default::default())
        .map(|x| x.inner)
        .collect::<Vec<_>>();

    assert_eq!(r, vec!["1", "+", "var1", ";"]);
}

#[test]
fn define_use() {
    let case = "#define A (1 + 1)\n1 + A";

    let ast = parse(pairs(case).unwrap());

    let r = AstIterator::new(ast, Default::default(), Default::default())
        .map(|x| x.inner)
        .collect::<Vec<_>>();

    assert_eq!(r, vec!["1", "+", "(", "1", "+", "1", ")"]);
}

#[test]
fn antistasi() {
    use std::fs;
    let path = "tests/integration/examples/antistasi.cpp";
    let case = fs::read_to_string(path).unwrap();

    let ast = parse(pairs(&case).unwrap());

    let r = AstIterator::new(ast, Default::default(), Default::default())
        .map(|x| x.inner)
        .collect::<Vec<_>>();
    let expected: Vec<&str> = vec![];

    assert_eq!(r, expected);
}
