use std::collections::HashMap;
use std::path::PathBuf;

use sqf::analyzer::*;
use sqf::parser;
use sqf::preprocessor;
use sqf::span::Spanned;
use sqf::types::Type;

pub fn parse_analyze_s(case: &str, state: &mut State) {
    let iter = preprocessor::tokens(case, Default::default(), Default::default()).unwrap();
    let (expr, errors) = parser::parse(iter);
    assert_eq!(errors, vec![]);
    analyze(&expr, state);
}

pub fn parse_analyze(case: &str) -> State {
    let mut state = Default::default();
    parse_analyze_s(case, &mut state);
    state
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

#[test]
fn infer_ok() {
    let case = "if true exitWith {_result\n};";

    let expected = Default::default();

    check_infer(case, expected);
}

#[test]
fn namespace_origin() {
    let case = "call A_fn_a";

    let mut state = State::default();
    state.namespace.mission.insert(
        "A_fn_a".to_string().into(),
        (
            Origin::External("A_fn_a".to_string().into()),
            Some(Type::Code),
        ),
    );
    parse_analyze_s(case, &mut state);
    assert_eq!(
        state.origins,
        HashMap::from([((5, 11), Origin::External("A_fn_a".to_string().into()))])
    );
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

    let expected = HashMap::from([(
        "_a".to_string().into(),
        (Origin::File((6, 8)), Some(Type::Number)),
    )]);

    let state = parse_analyze(case);
    assert_eq!(state.namespace.mission, expected);
}

#[test]
fn infer_example1() {
    use std::fs;
    let path: PathBuf = "tests/integration/dictionary/addons/dictionary/fnc__set.sqf".into();
    let case = fs::read_to_string(path.clone()).unwrap();

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

    let iter = preprocessor::tokens(&case, Default::default(), path).unwrap();
    let (expr, errors) = parser::parse(iter);
    assert_eq!(errors, vec![]);

    let mut state = Default::default();
    analyze(&expr, &mut state);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.types, expected);
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

        let iter = preprocessor::tokens(&case, Default::default(), path.clone()).unwrap();
        let (expr, errors) = parser::parse(iter);
        assert_eq!(errors, vec![]);

        let mut state = Default::default();
        analyze(&expr, &mut state);
        assert_eq!(state.errors, vec![]);
    }
}
