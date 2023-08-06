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
fn define_no_args() {
    let case = "#define B a\nB";

    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, vec!["a"]);
}

#[test]
fn macro_nested_no_args() {
    let case = "#define B b\n#define A a\\B\nA";

    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, vec!["a", "\\", "b"]);
}

#[test]
fn macro_with_concat() {
    let case = "#define B ##b\n#define A a\\B\nA";

    let mut iter: sqf::preprocessor::AstIterator<'_> =
        tokens(case, Default::default(), Default::default()).unwrap();

    let r = iter.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["a", "\\b"]);
}

#[test]
fn macro_with_arg() {
    let case = "#define B ##b\n#define A(c) c\\B\nA(a)";

    let mut iter: sqf::preprocessor::AstIterator<'_> =
        tokens(case, Default::default(), Default::default()).unwrap();

    let r = iter.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["a", "\\b"]);
}

#[test]
fn macro_with_arg_nested() {
    let case = "#define B(b) ##b\n#define A(a) a\\B(b)\nA(a)";

    let mut iter: sqf::preprocessor::AstIterator<'_> =
        tokens(case, Default::default(), Default::default()).unwrap();

    let r = iter.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["a", "\\b"]);
}

#[test]
fn define_double() {
    let case = r#"#define DOUBLES(var1,var2) ##var1##_##var2
#define QUOTE(var1) #var1
#define FNC_FILE_BASE(func) QUOTE(dictionary\DOUBLES(fnc,func).sqf)

FNC_FILE_BASE(a)
"#;
    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, vec!["\"dictionary\\fnc_a.sqf\""]);
}

#[test]
fn define_quote_in_middle() {
    let case = r#"#define A(a) #a
#define B(a) b = A(a)

B(a)
"#;
    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, vec!["b", "=", "\"a\""]);
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

#[test]
fn macro_with_composite_arg() {
    let case = r#"#define A(a) a

A(call b)
"#;
    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, vec!["call", "b"]);
}
