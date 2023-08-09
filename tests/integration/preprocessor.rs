use sqf::{preprocessor::tokens, span::Spanned};

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
fn if_comment() {
    let case = r#"#ifndef A
// comment
    #define A
#endif"#;

    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let _ = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);
    assert_eq!(ast.state.defines.len(), 1);
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
fn define_eval_double_end() {
    let case = r#"#define DOUBLES(var1,var2) ##var1##_##var2

DOUBLES(a, b)
"#;
    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, vec!["a_b"]);
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

#[test]
fn origin_define() {
    let case = r#"#define A _a
_a = 1;
_b = A;
"#;

    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast
        .by_ref()
        .map(|x| x.map(|x| x.to_string()))
        .collect::<Vec<_>>();

    let r = r
        .iter()
        .map(|x| x.as_ref().map(|x| x.as_str()))
        .collect::<Vec<_>>();

    assert_eq!(
        r,
        vec![
            Spanned {
                inner: "_a",
                span: (13, 15)
            },
            Spanned {
                inner: "=",
                span: (16, 17)
            },
            Spanned {
                inner: "1",
                span: (18, 19)
            },
            Spanned {
                inner: ";",
                span: (19, 20)
            },
            Spanned {
                inner: "_b",
                span: (21, 23)
            },
            Spanned {
                inner: "=",
                span: (24, 25)
            },
            Spanned {
                inner: "_a",
                span: (26, 27)
            },
            Spanned {
                inner: ";",
                span: (27, 28)
            }
        ]
    );
}

#[test]
fn define_empty_args() {
    let case = r#"#define A() 1

A()
"#;
    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, vec!["1"]);
}

#[test]
fn line() {
    let case = r#"#define FIX_LINE_NUMBERS2(sharp) sharp##line __LINE__ __FILE__
#define FIX_LINE_NUMBERS() FIX_LINE_NUMBERS2(#)

FIX_LINE_NUMBERS(a)
"#;
    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, Vec::<String>::new());
}

#[test]
fn other() {
    let case = r#"#define A(a, b)
A(a, b)"#;

    let mut ast = tokens(case, Default::default(), Default::default()).unwrap();

    let r = ast.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(ast.state.errors, vec![]);

    assert_eq!(r, Vec::<String>::new());
}
