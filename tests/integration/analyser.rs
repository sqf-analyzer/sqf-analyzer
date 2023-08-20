use std::collections::HashMap;
use std::path::Path;
use std::path::PathBuf;

use sqf::analyzer::*;
use sqf::error::Error;
use sqf::error::ErrorType;
use sqf::parser;
use sqf::preprocessor;
use sqf::span::Span;
use sqf::types::Type;
use sqf::uncased;

pub fn parse_analyze_s(case: &str, state: &mut State) {
    let iter = preprocessor::tokens(case, Default::default()).unwrap();
    let (expr, errors) = parser::parse(iter);
    assert_eq!(errors, vec![]);
    analyze(&expr, state);
}

pub fn file(start: usize, end: usize) -> Origin {
    Origin(PathBuf::from("").into(), Some((start, end)))
}

pub fn function(path: PathBuf) -> Origin {
    Origin(path.into(), None)
}

pub fn global(path: PathBuf, span: Span) -> Origin {
    Origin(path.into(), Some(span))
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
    let case = "private _a = 1.1";

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

    let case = "private _a = [] call {1}";
    let expected = HashMap::from([((8, 10), Some(Type::Number))]);
    check_infer(case, expected);
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
    let case = "if true exitWith {1\n};";

    let expected = Default::default();

    check_infer(case, expected);
}

#[test]
fn namespace_origin() {
    let case = "call A_fn_a";

    let mut state = State::default();
    state.namespace.mission.insert(
        uncased("A_fn_a"),
        (function(PathBuf::from("a")), Some(Type::Code.into())),
    );
    parse_analyze_s(case, &mut state);
    assert_eq!(
        state.origins,
        HashMap::from([((5, 11), function(PathBuf::from("a")))])
    );
}

/// when not private, variables are assigned to the previous stacks (or namespace if not defined)
#[test]
fn namespace() {
    let case = "private _a = west; call {private _a = 2}";

    let expected = HashMap::from([(uncased("_a"), (file(8, 10), Some(Type::Side.into())))]);

    let state = parse_analyze(case);
    assert_eq!(state.namespace.stack[0].variables, expected);

    let case = "private _a = west; call {_a = 2}";

    let expected = HashMap::from([(uncased("_a"), (file(25, 27), Some(Type::Number.into())))]);

    let state = parse_analyze(case);
    assert_eq!(state.namespace.stack[0].variables, expected);

    let case = "call {_a = 2}";

    let expected = HashMap::from([(uncased("_a"), (file(6, 8), Some(Type::Number.into())))]);

    let state = parse_analyze(case);
    assert_eq!(state.namespace.mission, expected);
}

#[test]
fn infer_example1() {
    let configuration = Configuration {
        file_path: PathBuf::from("tests/integration/dictionary/addons/dictionary/fnc__set.sqf")
            .into(),
        base_path: "./tests/integration/examples".into(),
        ..Default::default()
    };

    let state = sqf::check(configuration, Default::default(), Default::default()).unwrap();

    let expected = HashMap::from([
        ((274, 285), Some(Type::Anything)),
        ((477, 488), Some(Type::Anything)),
        ((317, 321), Some(Type::Anything)),
        ((374, 380), Some(Type::Anything)),
        ((430, 434), Some(Type::Number)),
    ]);

    assert_eq!(
        state.errors,
        vec![
            Error {
                type_: ErrorType::UndefinedVariable,
                span: (532, 542),
                origin: Some(
                    Path::new("tests/integration/dictionary/addons/dictionary/fnc__set.sqf").into()
                )
            },
            Error {
                type_: ErrorType::UndefinedVariable,
                span: (532, 542),
                origin: Some(
                    Path::new("tests/integration/dictionary/addons/dictionary/fnc__set.sqf").into()
                )
            }
        ]
    );
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
    let configuration = Configuration {
        file_path: PathBuf::from("tests/integration/dictionary/addons/dictionary/fnc_set.sqf")
            .into(),
        base_path: "./tests/integration/examples".into(),
        ..Default::default()
    };

    let mission = HashMap::from([(
        uncased("DICT_fnc__set"),
        (
            function(PathBuf::from("a")),
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
    )]);
    let state = sqf::check(configuration, mission, Default::default()).unwrap();

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

    let paths = fs::read_dir(&directory).unwrap();
    for path in paths {
        let path = path.unwrap().path();
        if path.extension().is_none() {
            continue;
        }
        let extension = path.extension().unwrap().to_str().unwrap();
        if !extension.contains("sqf") {
            continue;
        }

        let configuration = Configuration {
            file_path: path.into(),
            base_path: directory.clone(),
            ..Default::default()
        };
        let mut state = sqf::check(configuration, Default::default(), Default::default()).unwrap();
        state
            .errors
            .retain(|e| e.type_ != ErrorType::UndefinedVariable);
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
                    file(13, 16),
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
            (uncased("_x"), (file(154, 156), Some(Type::Array.into())))
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
            (file(8, 11), Some(Output::Code(None, Some(Type::Anything))))
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
        HashMap::from([(uncased("_aa"), (file(8, 11), None))])
    );
    assert_eq!(state.return_type(), Some(Type::Nothing));
}

#[test]
fn private_assign_type() {
    let case = r#"private _aa = _a select [1]"#;
    let state = parse_analyze(case);

    assert_eq!(
        state.namespace.stack[0].variables,
        HashMap::from([(uncased("_aa"), (file(8, 11), Some(Type::Anything.into())))])
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
            global(PathBuf::from("a"), (10, 11)),
            Some(Output::Type(Type::Number)),
        ),
    );
    parse_analyze_s(case, &mut state);
    assert_eq!(
        state.origins,
        HashMap::from([((4, 5), global(PathBuf::from("a"), (10, 11)))])
    );
}

#[test]
fn origin_global_from() {
    let case = r#"a = 1"#;

    let state = parse_analyze(case);
    assert_eq!(
        state.namespace.mission,
        HashMap::from([(uncased("a"), (file(0, 1), Some(Type::Number.into())))])
    );
}

#[test]
fn for_each() {
    let case = r#"
private _x = 1;
{"a" + _x + _foreachindex} forEach []
"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
}

#[test]
fn exit_with() {
    let case = r#"
if true exitWith {1};
"a";
"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.return_type(), Some(Type::Anything))
}

#[test]
fn else_return_union() {
    let case = r#"
if true then {2} else {true}
"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.return_type(), Some(Type::Anything))
}

#[test]
fn then_return() {
    let case = r#"
if true then {2}
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

#[test]
fn params_good() {
    let case = "[1] params [\"_x\"]";
    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(
        state.namespace.stack[0].variables,
        HashMap::from([(uncased("_x"), (file(12, 16), Some(Type::Number.into())))])
    );
}

#[test]
fn count() {
    let case = "{_x == 4} count []";
    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.origins, HashMap::from([((1, 3), file(10, 15))]));
}

#[test]
fn private_unary() {
    let case = "private [\"_a\"]; _a = 1";
    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.namespace.mission.len(), 0);
}

#[test]
fn select_a() {
    let case = "[1, 2] select {_x == 1}";
    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.namespace.mission.len(), 0);
}

#[test]
fn compile() {
    let case = "call compile preprocessFileLineNumbers \"error.sqf\"";

    let configuration = Configuration {
        file_path: PathBuf::from("./tests/integration/examples/bla.txt").into(),
        base_path: "./tests/integration/examples/config.cpp".into(),
        ..Default::default()
    };

    let mut state = State {
        configuration,
        ..Default::default()
    };
    parse_analyze_s(case, &mut state);

    assert_eq!(state.errors, vec![
        Error {
            type_: "\"+\" does not support left side of type \"Array\" and right side of type \"Number\"".to_string().into(),
            span: (10, 11),
            origin: Some(PathBuf::from("tests/integration/examples/error.sqf").into())
        },
        Error {
            type_: "\"+\" does not support left side of type \"Number\" and right side of type \"Array\"".to_string().into(),
            span: (9, 10),
            origin: Some(PathBuf::from("tests/integration/examples/other.sqf").into())
        },
    ]);
    assert_eq!(state.namespace.mission.len(), 2); // a and b
}

#[test]
fn exec_vm() {
    let case = "execVM \"other.sqf\"";

    let configuration = Configuration {
        file_path: PathBuf::from("./tests/integration/examples/error.txt").into(),
        base_path: "./tests/integration/examples/description.cpp".into(),
        ..Default::default()
    };

    let mut state = State {
        configuration,
        ..Default::default()
    };
    parse_analyze_s(case, &mut state);

    assert_eq!(state.errors, vec![
        Error {
            type_: "\"+\" does not support left side of type \"Number\" and right side of type \"Array\"".to_string().into(),
            span: (9, 10),
            origin: Some(PathBuf::from("tests/integration/examples/other.sqf").into())
        },
    ]);
    assert_eq!(state.namespace.mission.len(), 1); // b
}

#[test]
fn binary_exec_vm() {
    let case = "[] execVM \"other.sqf\"; b";

    let configuration = Configuration {
        file_path: PathBuf::from("./tests/integration/examples/error.txt").into(),
        base_path: "./tests/integration/examples/description.cpp".into(),
        ..Default::default()
    };

    let mut state = State {
        configuration,
        ..Default::default()
    };
    parse_analyze_s(case, &mut state);

    assert_eq!(state.errors, vec![
        Error {
            type_: "\"+\" does not support left side of type \"Number\" and right side of type \"Array\"".to_string().into(),
            span: (9, 10),
            origin: Some(PathBuf::from("tests/integration/examples/other.sqf").into())
        },
    ]);
    assert_eq!(state.namespace.mission.len(), 1); // b
    assert_eq!(
        state.origins,
        HashMap::from([(
            (23, 24),
            global(
                PathBuf::from("tests/integration/examples/other.sqf"),
                (0, 1)
            )
        )])
    );
}

#[test]
fn debug() {
    let case = "params [\"_a\"]; a = 1; true";
    let state = parse_analyze(case);
    state.globals(Some(uncased("f")));
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.return_type(), Some(Type::Boolean))
}
