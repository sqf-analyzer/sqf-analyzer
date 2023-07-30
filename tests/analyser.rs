use std::collections::HashMap;

use sqf::analyzer::*;
use sqf::parser::*;
use sqf::types::*;

fn check_infer(case: &str, expected: HashMap<Span<String>, Option<Type>>) {
    let a = analyze(&parse(tokens(case).unwrap()), "tests/dictionary/".into()).unwrap();
    assert_eq!(a, expected);
}

#[test]
fn infer_number() {
    let case = "private _a = 1";

    let expected = HashMap::from([(
        Span {
            inner: "_a".to_string(),
            span: (8, 10),
        },
        Some(Type::Number),
    )]);
    check_infer(case, expected);
}

#[test]
fn infer_string() {
    let case = "private _a = \"1\"";

    let expected = HashMap::from([(
        Span {
            inner: "_a".to_string(),
            span: (8, 10),
        },
        Some(Type::String),
    )]);
    check_infer(case, expected);
}

#[test]
fn infer_variable() {
    let case = "private _a = 1";

    let expected = HashMap::from([(
        Span {
            inner: "_a".to_string(),
            span: (8, 10),
        },
        Some(Type::Number),
    )]);

    check_infer(case, expected);
}

#[test]
fn infer_unary() {
    let case = "private _a = params [\"_a\"]";

    let expected = HashMap::from([(
        Span {
            inner: "_a".to_string(),
            span: (8, 10),
        },
        Some(Type::Boolean),
    )]);

    check_infer(case, expected);
}

#[test]
fn infer_binary() {
    let case = "private _a = 1 + 1";

    let expected = HashMap::from([(
        Span {
            inner: "_a".to_string(),
            span: (8, 10),
        },
        Some(Type::Number),
    )]);

    check_infer(case, expected);

    let case = "_a select _i";
    check_infer(case, HashMap::new());
}

#[test]
fn infer_nullary() {
    let case = "private _a = west";

    let expected = HashMap::from([(
        Span {
            inner: "_a".to_string(),
            span: (8, 10),
        },
        Some(Type::Side),
    )]);

    check_infer(case, expected);
}

#[test]
fn infer_example() {
    use std::fs;
    let path = "tests/dictionary/addons/dictionary/fnc__set.sqf";
    let case = fs::read_to_string(path).unwrap();

    let expected = HashMap::from([
        (
            Span {
                inner: "_dictionary".to_string(),
                span: (274, 285),
            },
            None,
        ),
        (
            Span {
                inner: "_dictionary".to_string(),
                span: (477, 488),
            },
            None,
        ),
        (
            Span {
                inner: "_key".to_string(),
                span: (317, 321),
            },
            None,
        ),
        (
            Span {
                inner: "_value".to_string(),
                span: (374, 380),
            },
            None,
        ),
        (
            Span {
                inner: "_i".to_string(),
                span: (430, 434),
            },
            Some(Type::Number),
        ),
    ]);

    let r = analyze(&parse(tokens(&case).unwrap()), path.into()).unwrap();
    assert_eq!(r, expected);
}

#[test]
fn infer_example2() {
    use std::fs;
    use std::path::PathBuf;
    let directory: PathBuf = "tests/dictionary/addons/dictionary/".into();

    let paths = fs::read_dir(directory.clone()).unwrap();
    for path in paths {
        let path = path.unwrap().path();
        if path.extension().is_none() {
            continue;
        }
        if !path.extension().unwrap().to_str().unwrap().contains(".sqf") {
            continue;
        }
        let path = directory.clone().join(path);
        let case = fs::read_to_string(path.clone()).unwrap();
        analyze(&parse(tokens(&case).unwrap()), path).unwrap();
    }
}
