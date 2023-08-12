use std::collections::HashMap;
use std::path::PathBuf;

use sqf::analyzer::*;
use sqf::parser;
use sqf::preprocessor;
use sqf::span::Span;
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

fn check_infer(case: &str, expected: HashMap<Span, Option<Type>>) {
    let result = parse_analyze(case);
    assert_eq!(result.errors, vec![]);
    assert_eq!(result.types, expected);
}

#[test]
fn infer_number() {
    let case = "private _a = 1";

    let expected = HashMap::from([((8, 10), Some(Type::Number))]);
    check_infer(case, expected);
}

#[test]
fn infer_string() {
    let case = "private _a = \"1\"";

    let expected = HashMap::from([((8, 10), Some(Type::String))]);
    check_infer(case, expected);
}

#[test]
fn infer_variable() {
    let case = "private _a = 1";

    let expected = HashMap::from([((8, 10), Some(Type::Number))]);

    check_infer(case, expected);
}

#[test]
fn infer_unary() {
    let case = "private _a = params [\"_a\"]";

    let expected = HashMap::from([((8, 10), Some(Type::Boolean))]);

    check_infer(case, expected);
}

#[test]
fn infer_binary() {
    let case = "private _a = 1 + 1";

    let expected = HashMap::from([((8, 10), Some(Type::Number))]);

    check_infer(case, expected);

    let case = "_a select _i";
    check_infer(case, HashMap::new());
}

#[test]
fn infer_hexa() {
    let case = "private _a = 0xa";

    let expected = HashMap::from([((8, 10), Some(Type::Number))]);

    check_infer(case, expected);
}

#[test]
fn infer_nullary() {
    let case = "private _a = west";

    let expected = HashMap::from([((8, 10), Some(Type::Side))]);

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
            Some(Type::Code.into()),
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

    let expected = HashMap::from([("_a".into(), ((8, 10), Some(Type::Side.into())))]);

    let state = parse_analyze(case);
    assert_eq!(state.namespace.stack[0].variables, expected);

    let case = "private _a = west; call {_a = 2}";

    let expected = HashMap::from([("_a".into(), ((25, 27), Some(Type::Number.into())))]);

    let state = parse_analyze(case);
    assert_eq!(state.namespace.stack[0].variables, expected);

    let case = "call {_a = 2}";

    let expected = HashMap::from([(
        "_a".into(),
        (Origin::File((6, 8)), Some(Type::Number.into())),
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
        ((274, 285), None),
        ((477, 488), Some(Type::Anything)),
        ((317, 321), None),
        ((374, 380), None),
        ((430, 434), Some(Type::Number)),
    ]);

    let iter = preprocessor::tokens(&case, Default::default(), path).unwrap();
    let (expr, errors) = parser::parse(iter);
    assert_eq!(errors, vec![]);

    let mut state = State::default();
    analyze(&expr, &mut state);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.types, expected);
    assert_eq!(
        state.signature(),
        Some(&vec![
            Parameter {
                name: "_arguments".into(),
                type_: Type::Anything,
                has_default: false,
            },
            Parameter {
                name: "_isGlobal".into(),
                type_: Type::Anything,
                has_default: false,
            }
        ])
    );
}

#[test]
fn infer_example3() {
    use std::fs;
    let path: PathBuf = "tests/integration/dictionary/addons/dictionary/fnc_set.sqf".into();
    let case = fs::read_to_string(path.clone()).unwrap();

    let iter = preprocessor::tokens(&case, Default::default(), path).unwrap();
    let (expr, errors) = parser::parse(iter);
    assert_eq!(errors, vec![]);

    let mut state = State::default();
    state.namespace.mission.insert(
        "DICT_fnc__set".to_string().into(),
        (
            Origin::External("DICT_fnc__set".to_string().into()),
            Some(Output::Code(
                vec![
                    Parameter {
                        name: "_arguments".into(),
                        type_: Type::Anything,
                        has_default: false,
                    },
                    Parameter {
                        name: "_isGlobal".into(),
                        type_: Type::Anything,
                        has_default: false,
                    },
                ],
                Some(Type::Boolean),
            )),
        ),
    );

    analyze(&expr, &mut state);
    assert_eq!(state.errors, vec![]);
    assert_eq!(
        state.parameters,
        HashMap::from([
            ((119, 124), "_arguments".into()),
            ((126, 131), "_isGlobal".into())
        ])
    );
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

#[test]
fn errors() {
    let cases = [
        "call + 1",
        "{} + a",
        "a + {}",
        "for 1",
        "call _a = 1",
        "1 = 1",
        "private 1 = 1",
    ];
    for case in cases {
        let state = parse_analyze(case);
        assert!(!state.errors.is_empty());
    }
}

#[test]
fn reassign_evaluted_after() {
    let case = r#"
    private _a = 1;
    if(_a == -1) then
    {
        _a = "1";
    };
    "#;
    let errors = parse_analyze(case).errors;
    assert_eq!(errors, vec![]);
}

#[test]
fn reassign_evaluted_then() {
    let case = r#"
    params ["_x"];
    if (_x isEqualType []) then {
        _x = [];
    } else {
        _x getVariable "a"
    };
    "#;
    let errors = parse_analyze(case).errors;
    assert_eq!(errors, vec![]);
}
