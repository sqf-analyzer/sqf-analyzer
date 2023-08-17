use std::collections::HashMap;
use std::path::PathBuf;

use sqf::analyzer::*;
use sqf::parser;
use sqf::preprocessor;
use sqf::preprocessor::Configuration;
use sqf::span::Span;
use sqf::types::Type;
use sqf::uncased;

pub fn parse_analyze_s(case: &str, state: &mut State) {
    let iter = preprocessor::tokens(case, Default::default()).unwrap();
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
        uncased("A_fn_a"),
        (
            Origin::External(uncased("A_fn_a"), None),
            Some(Type::Code.into()),
        ),
    );
    parse_analyze_s(case, &mut state);
    assert_eq!(
        state.origins,
        HashMap::from([((5, 11), Origin::External(uncased("A_fn_a"), None))])
    );
}

/// when not private, variables are assigned to the previous stacks (or namespace if not defined)
#[test]
fn namespace() {
    let case = "private _a = west; call {private _a = 2}";

    let expected = HashMap::from([(uncased("_a"), ((8, 10), Some(Type::Side.into())))]);

    let state = parse_analyze(case);
    assert_eq!(state.namespace.stack[0].variables, expected);

    let case = "private _a = west; call {_a = 2}";

    let expected = HashMap::from([(uncased("_a"), ((25, 27), Some(Type::Number.into())))]);

    let state = parse_analyze(case);
    assert_eq!(state.namespace.stack[0].variables, expected);

    let case = "call {_a = 2}";

    let expected = HashMap::from([(
        uncased("_a"),
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
        ((274, 285), Some(Type::Anything)),
        ((477, 488), Some(Type::Anything)),
        ((317, 321), Some(Type::Anything)),
        ((374, 380), Some(Type::Anything)),
        ((430, 434), Some(Type::Number)),
    ]);

    let iter = preprocessor::tokens(&case, Configuration::with_path(path)).unwrap();
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

    let iter = preprocessor::tokens(&case, Configuration::with_path(path)).unwrap();
    let (expr, errors) = parser::parse(iter);
    assert_eq!(errors, vec![]);

    let mut state = State::default();
    state.namespace.mission.insert(
        uncased("DICT_fnc__set"),
        (
            Origin::External(uncased("DICT_fnc__set"), None),
            Some(Output::Code(
                Some(vec![
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
                ]),
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

        let iter = preprocessor::tokens(&case, Configuration::with_path(path.clone())).unwrap();
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

#[test]
fn return_type_simple() {
    assert_eq!(parse_analyze("1").return_type(), Some(Type::Number));
}

#[test]
fn case_insensitive() {
    // binary; unary; nullary
    assert_eq!(
        parse_analyze("true And false; Call {}; True").errors,
        vec![]
    );
}

#[test]
fn return_type() {
    let case = r#"
    private _aa = {
        params ["_x"];
    
        if (_x) then {
            [1];
        } else {
            [2];
        };
    };

    private _x = [1] call _aa;
    _x
    "#;
    let state = parse_analyze(case);

    assert_eq!(
        state.namespace.stack[0].variables,
        HashMap::from([
            (
                uncased("_aa"),
                (
                    (13, 16),
                    Some(Output::Code(
                        Some(vec![Parameter {
                            name: "_x".into(),
                            type_: Type::Anything,
                            has_default: false,
                        }]),
                        Some(Type::Array)
                    ))
                )
            ),
            (uncased("_x"), ((154, 156), Some(Type::Array.into())))
        ])
    );
    assert_eq!(state.return_type(), Some(Type::Array));
}

#[test]
fn no_signature() {
    let case = r#"private _aa = {_this}"#;
    let state = parse_analyze(case);

    assert_eq!(
        state.namespace.stack[0].variables,
        HashMap::from([(
            uncased("_aa"),
            ((8, 11), Some(Output::Code(None, Some(Type::Anything))))
        ),])
    );
    assert_eq!(state.return_type(), Some(Type::Nothing));
}

#[test]
fn return_type_then() {
    let case = r#"private _aa = if(_side == c) then {a} else {b};"#;
    let state = parse_analyze(case);

    assert_eq!(
        state.namespace.stack[0].variables,
        HashMap::from([(uncased("_aa"), ((8, 11), None))])
    );
    assert_eq!(state.return_type(), Some(Type::Nothing));
}

#[test]
fn debug() {
    let case = r#"private _aa = _a select [1]"#;
    let state = parse_analyze(case);

    assert_eq!(
        state.namespace.stack[0].variables,
        HashMap::from([(uncased("_aa"), ((8, 11), Some(Type::Anything.into())))])
    );
    assert_eq!(state.return_type(), Some(Type::Nothing));
}

#[test]
fn origin_global() {
    let case = r#"1 + a"#;

    let mut state = State::default();
    state.namespace.mission.insert(
        uncased("a"),
        (
            Origin::External(uncased("a"), Some((10, 11))),
            Some(Output::Type(Type::Number)),
        ),
    );
    parse_analyze_s(case, &mut state);
    assert_eq!(
        state.origins,
        HashMap::from([((4, 5), Origin::External(uncased("a"), Some((10, 11))))])
    );
}

#[test]
fn origin_global_from() {
    let case = r#"a = 1"#;

    let state = parse_analyze(case);
    assert_eq!(
        state.namespace.mission,
        HashMap::from([(
            uncased("a"),
            (Origin::File((0, 1)), Some(Type::Number.into()))
        )])
    );
}

#[test]
fn for_each() {
    let case = r#"
private _x = 1;
{"a" + _x} forEach []
"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
}

#[test]
fn exit_with() {
    let case = r#"
if _a exitWith {1};
"a";
"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.return_type(), Some(Type::Anything))
}

#[test]
fn else_return_union() {
    let case = r#"
if _a then {2} else {true}
"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.return_type(), Some(Type::Anything))
}

#[test]
fn then_return() {
    let case = r#"
if _a then {2}
"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.return_type(), Some(Type::Number))
}

#[test]
fn if_else_assign_val() {
    let case = r#"
private _result = 0;
if(true) then {
    _result = 1;
} else {
    _result = [];
};
_result;
"#;
    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.return_type(), Some(Type::Anything))
}

#[test]
fn for_each_return() {
    let case = "{true} forEach []";
    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.return_type(), Some(Type::Boolean))
}
