use std::collections::HashMap;

use sqf::analyzer::*;
use sqf::parser::*;
use sqf::types::*;

fn parse_analyze(case: &str) -> State {
    let (ast, errors) = parse(tokens(case).unwrap());
    assert_eq!(errors, vec![]);
    analyze(&ast, "tests/dictionary/".into())
}

fn check_infer(case: &str, expected: HashMap<Spanned<String>, Option<Type>>) {
    let result = parse_analyze(case);
    assert_eq!(result.errors, vec![]);
    assert_eq!(result.types, expected);
}

#[test]
fn infer_number() {
    let case = "private _a = 1";

    let expected = HashMap::from([(
        Spanned {
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
        Spanned {
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
        Spanned {
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
        Spanned {
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
        Spanned {
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
        Spanned {
            inner: "_a".to_string(),
            span: (8, 10),
        },
        Some(Type::Side),
    )]);

    check_infer(case, expected);
}

/// when not private, variables are assigned to the previous stacks (or namespace if not defined)
#[test]
fn namespace() {
    let case = "private _a = west; call {private _a = 2}";

    let expected = vec![HashMap::from([(
        "_a".to_string(),
        ((8, 10), Some(Type::Side)),
    )])];

    let state = parse_analyze(case);
    assert_eq!(state.namespace.stack, expected);

    let case = "private _a = west; call {_a = 2}";

    let expected = vec![HashMap::from([(
        "_a".to_string(),
        ((25, 27), Some(Type::Number)),
    )])];

    let state = parse_analyze(case);
    assert_eq!(state.namespace.stack, expected);

    let case = "call {_a = 2}";

    let expected = HashMap::from([("_a".to_string(), ((6, 8), Some(Type::Number)))]);

    let state = parse_analyze(case);
    assert_eq!(state.namespace.mission, expected);
}

#[test]
fn infer_example1() {
    use std::fs;
    let path = "tests/integration/dictionary/addons/dictionary/fnc__set.sqf";
    let case = fs::read_to_string(path).unwrap();

    let expected = HashMap::from([
        (
            Spanned {
                inner: "_dictionary".to_string(),
                span: (274, 285),
            },
            None,
        ),
        (
            Spanned {
                inner: "_dictionary".to_string(),
                span: (477, 488),
            },
            Some(Type::Anything),
        ),
        (
            Spanned {
                inner: "_key".to_string(),
                span: (317, 321),
            },
            None,
        ),
        (
            Spanned {
                inner: "_value".to_string(),
                span: (374, 380),
            },
            None,
        ),
        (
            Spanned {
                inner: "_i".to_string(),
                span: (430, 434),
            },
            Some(Type::Number),
        ),
    ]);

    let (ast, errors) = parse(tokens(&case).unwrap());
    assert_eq!(errors, vec![]);

    let r = analyze(&ast, path.into());
    assert_eq!(r.errors, vec![]);
    assert_eq!(r.types, expected);
}

#[test]
fn infer_example2() {
    use std::fs;
    use std::path::PathBuf;
    let directory: PathBuf = "tests/integration/dictionary/addons/dictionary/".into();

    let paths = fs::read_dir(directory).unwrap();
    for path in paths {
        let path = path.unwrap().path();
        if path.extension().is_none() {
            continue;
        }
        let extension = path.extension().unwrap().to_str().unwrap();
        if !extension.contains("sqf") {
            continue;
        }
        println!("{path:?}");
        let case = fs::read_to_string(path.clone()).unwrap();

        let (ast, errors) = parse(tokens(&case).unwrap());
        assert_eq!(errors, vec![]);

        let r = analyze(&ast, path);
        assert!(r.errors.is_empty())
    }
}
