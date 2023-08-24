use std::collections::VecDeque;

use sqf::{
    preprocessor::parse,
    preprocessor::{tokens, Ast},
    span::Spanned,
};

fn assert(case: &str, expected: Vec<&str>) {
    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, expected);
}

#[test]
fn pairs_() {
    use std::fs;
    let path = "tests/integration/examples/antistasi.cpp";
    let case = fs::read_to_string(path).unwrap();

    tokens(&case, Default::default()).unwrap();
}

#[test]
fn basic_functionality() {
    use std::fs;
    let path = "tests/integration/examples/basic.cpp";
    let case = fs::read_to_string(path).unwrap();

    tokens(&case, Default::default()).unwrap();
}

#[test]
fn if_across_code() {
    let case = r#"
if a then {
    b
#ifdef A
};
#else
} else {
    c
};
#endif
d
"#;

    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

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

    let mut iter = tokens(case, Default::default()).unwrap();

    let _ = iter.by_ref().map(|x| x.inner).collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);
    assert_eq!(iter.state.configuration.defines.len(), 1);
}

#[test]
fn tokens2() {
    use std::fs;
    let path = "tests/integration/examples/basic.cpp";
    let case = fs::read_to_string(path).unwrap();

    let mut iter = tokens(&case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

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

    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    let expected: Vec<&str> = vec![];

    assert_eq!(r, expected);
}

#[test]
fn define_carrier_end() {
    let case = "#define A \\\r\n1";
    assert(case, vec![]);
}

#[test]
fn define_use_with_args() {
    let case = "#define A(B) var##B\n1 + A(1);";

    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["1", "+", "var1", ";"]);
}

#[test]
fn define_use() {
    let case = "#define A (1 + 1)\n1 + A";

    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["1", "+", "(", "1", "+", "1", ")"]);
}

#[test]
fn define_no_args() {
    let case = "#define B a\nB";

    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["a"]);
}

#[test]
fn macro_nested_no_args() {
    let case = "#define B b\n#define A a\\B\nA";

    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["a", "\\", "b"]);
}

#[test]
fn macro_with_concat() {
    let case = "#define B ##b\n#define A a\\B\nA";

    let mut iter: sqf::preprocessor::AstIterator<'_> = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["a", "\\b"]);
}

#[test]
fn macro_with_arg() {
    let case = "#define B ##b\n#define A(c) c\\B\nA(a)";

    let mut iter: sqf::preprocessor::AstIterator<'_> = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["a", "\\b"]);
}

#[test]
fn macro_with_arg_nested() {
    let case = "#define B(b) ##b\n#define A(a) a\\B(b)\nA(a)";

    let mut iter: sqf::preprocessor::AstIterator<'_> = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
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
    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["\"dictionary\\fnc_a.sqf\""]);
}

#[test]
fn define_eval_double_end() {
    let case = r#"#define DOUBLES(var1,var2) ##var1##_##var2

DOUBLES(a, b)
"#;
    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["a_b"]);
}

#[test]
fn antistasi() {
    use std::fs;
    let path = "tests/integration/examples/antistasi.cpp";
    let case = fs::read_to_string(path).unwrap();

    let mut iter = tokens(&case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    let expected: Vec<&str> = vec![];

    assert_eq!(r, expected);
}

#[test]
fn macro_with_composite_arg() {
    let case = r#"#define A(a) a

A(call b)
"#;
    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
        .by_ref()
        .map(|x| x.inner.to_string())
        .collect::<Vec<_>>();
    assert_eq!(iter.state.errors, vec![]);

    assert_eq!(r, vec!["call", "b"]);
}

#[test]
fn undef() {
    assert("#define B 1\n#undef B\nB", vec!["B"]);
}

#[test]
fn origin_define() {
    let case = r#"#define A _a
_a = 1;
_b = A;
"#;

    let mut iter = tokens(case, Default::default()).unwrap();

    let r = iter
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
    assert(case, vec!["1"]);
}

#[test]
fn line() {
    let case = r#"#define FIX_LINE_NUMBERS2(sharp) sharp##line __LINE__ __FILE__
#define FIX_LINE_NUMBERS() FIX_LINE_NUMBERS2(#)

FIX_LINE_NUMBERS(a)
"#;
    assert(case, vec![]);
}

#[test]
fn other() {
    let case = r#"#define A(a, b)
A(a, b)"#;
    assert(case, vec![]);
}

#[test]
fn define_quote_in_middle() {
    let case = r#"#define A(a) #a
#define B(a) b = A(a)

B(a)
"#;

    assert(case, vec!["b", "=", "\"a\""]);
}

#[test]
fn quoted_in_middle_with_break() {
    let case = r#"
    #define A(a) (isNil #a)
    A(a)"#;
    assert(case, vec!["(", "isNil", "\"a\"", ")"]);
}

#[test]
fn exponent() {
    assert("1e6", vec!["1e6"]);
}

#[test]
fn number1() {
    assert("10", vec!["10"]);
    assert("10.0", vec!["10.0"]);
    assert("1.0", vec!["1.0"]);
    assert(".1", vec![".1"]);
    assert("0.1", vec!["0.1"]);
    assert(".65", vec![".65"]);
    assert("4.76837e-007", vec!["4.76837e-007"]);
    assert("3.", vec!["3."]);
}

#[test]
fn single_quoted() {
    assert("'a'", vec!["'a'"]);
}

#[test]
fn hash_op() {
    assert("#define A(x) x\nA(_this#0)", vec!["_this", "#", "0"]);
}

#[test]
fn nested_macro_call() {
    let case = r#"#define A(a, b) a + b
#define B(a) a
A(B(1), 2)"#;
    assert(case, vec!["1", "+", "2"]);
}

#[test]
fn macro_arg_with_parenthesis() {
    let case = r#"
#define A(a)
{A(((a) b))}
"#;
    assert(case, vec!["{", "}"]);
}

#[test]
fn include_a3() {
    let case = r#"#include "\A3\Ui_f\hpp\defineResinclDesign.inc""#;
    assert(case, vec![]);
}

#[test]
fn include_slash_file() {
    let case = r#"#include ".\tests\integration\examples\basic.cpp""#;
    assert(case, vec![]);
}

#[test]
fn include_forward_slash_file() {
    let case = r#"#include "./tests/integration/examples/basic.cpp""#;
    assert(case, vec![]);
}

#[test]
fn if_has_include() {
    let case = r#"
#if __has_include("something")
#endif
"#;
    assert(case, vec![]);
}

#[test]
fn if_else() {
    let case = r#"
#if 0
#else
1
#endif
"#;
    assert(case, vec!["1"]);
}

#[test]
fn ifndef_def() {
    let case = r#"
#ifndef A
#define A
#define B 1 + 2
#endif

B
"#;
    assert(case, vec!["1", "+", "2"]);
}

#[test]
fn comment_span() {
    let case = r#"/*
aaa  
*/"#;
    assert_eq!(
        parse(case).unwrap(),
        VecDeque::from([Ast::Comment(Spanned {
            inner: "/*\naaa  \n*/",
            span: (0, 11),
        })])
    );
}

#[test]
fn macro_recursion() {
    let case = r#"
#define A A
A
"#;
    assert(case, vec!["A"]);
}

#[test]
fn define_with_comment_in_body() {
    let case = r#"
#define A 1 // a
A
"#;
    assert(case, vec!["1"]);
}

#[test]
fn argument_with_array() {
    let case = r#"
#define A(name,value) (a call [name,value,true])

A("a", [_a, _b] call b);
"#;
    assert(
        case,
        vec![
            "(", "a", "call", "[", "\"a\"", ",", "[", "_a", ",", "_b", "]", "call", "b", ",",
            "true", "]", ")", ";",
        ],
    );
}

#[test]
fn crlf() {
    assert("#if A\r\n#endif", vec![]);
}

#[test]
fn bom_at_start() {
    assert("\u{feff}//------\r\n", vec![]);
}

#[test]
fn define_other() {
    assert(
        "#define bool                //boolean\nbool a = true;",
        vec!["a", "=", "true", ";"],
    );
}

#[test]
fn concat_with_space_before() {
    let case = r#"
    #define TRIPLES(var1,var2,var3) ##var1##_##var2##_##var3
    #define FNC_UI_PREFIX(prefix,func) class TRIPLES(UI,prefix,func)

    FNC_UI_PREFIX(a,b)
    "#;
    assert(case, vec!["class", "UI_a_b"]);
}

#[test]
fn define_single_quoted() {
    let case = r#"
    #define DOUBLES(var1,var2) var1##_##var2
    #define A(var1) 1 + 'DOUBLES(fnc,var1)' + 1

    A(a)
    "#;
    assert(case, vec!["1", "+", "'fnc_a'", "+", "1"]);
}

#[test]
fn recursive_define() {
    let case = r#"
#define LOG_SYS(LEVEL,MESSAGE) [LEVEL, MESSAGE]
#define ERROR(MESSAGE) LOG_SYS('ERROR',MESSAGE)
ERROR("no display");
"#;

    assert(case, vec!["[", "'ERROR'", ",", "\"no display\"", "]", ";"]);
}

#[test]
fn recursion_does_not_crash() {
    let case = r#"
#define LOG ERROR
#define ERROR LOG
ERROR
"#;

    assert(case, vec!["ERROR"]);
}

#[test]
fn define_with_tabs() {
    let case = "#define\tA  \t3";
    assert(case, vec![]);
}
