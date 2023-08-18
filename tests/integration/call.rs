use std::{collections::HashMap, path::PathBuf};

use sqf::{
    analyzer::{Origin, Output, Parameter, State},
    error::{Error, ErrorType},
    types::Type,
    uncased,
};

use super::analyser::{parse_analyze, parse_analyze_s};

#[test]
fn call_len_args() {
    let case = r#"private _a = {params ["_a"]}; [1, 2] call _a;"#;

    let state = parse_analyze(case);
    assert_eq!(
        state.errors,
        vec![Error::new(
            ErrorType::InsufficientArguments {
                expected: 1,
                passed: 2
            },
            (30, 36)
        )]
    );
}

#[test]
fn call_annotate() {
    let case = r#"private _a = {params ["_a"]}; [1] call _a;"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
    assert_eq!(state.parameters, HashMap::from([((31, 32), "_a".into())]));
}

#[test]
fn call_annotate_ext() {
    let case = r#"private _a = [1] call A_fn_a;"#;

    let mut state = State::default();
    state.namespace.mission.insert(
        uncased("A_fn_a"),
        (
            Origin::External(PathBuf::from("a").into(), None),
            Some(Output::Code(
                Some(vec![Parameter {
                    name: "_a".into(),
                    type_: Type::Anything,
                    has_default: false,
                }]),
                Some(Type::Boolean),
            )),
        ),
    );
    parse_analyze_s(case, &mut state);
    assert_eq!(
        state.namespace.stack[0].variables,
        HashMap::from([(uncased("_a"), ((8, 10), Some(Type::Boolean.into())))])
    );
    assert_eq!(
        state.origins,
        HashMap::from([((22, 28), Origin::External(PathBuf::from("a").into(), None))])
    );
    assert_eq!(state.parameters, HashMap::from([((14, 15), "_a".into())]));
}

#[test]
fn used_default_arg() {
    let case = r#"private _a = {params ["_a", ["_b", []]]}; [1] call _a;"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
}

#[test]
fn execute_annotate_ext() {
    let case = r#"[1] remoteExec ["A_fn_a"];"#;

    let mut state = State::default();
    state.namespace.mission.insert(
        uncased("A_fn_a"),
        (
            Origin::External(PathBuf::from("a").into(), None),
            Some(Output::Code(
                Some(vec![Parameter {
                    name: "_a".into(),
                    type_: Type::Number,
                    has_default: false,
                }]),
                None,
            )),
        ),
    );
    parse_analyze_s(case, &mut state);
    assert_eq!(
        state.origins,
        HashMap::from([((16, 24), Origin::External(PathBuf::from("a").into(), None))])
    );
    assert_eq!(state.parameters, HashMap::from([((1, 2), "_a".into())]));
}

#[test]
fn spawn_annotate_ext() {
    let case = r#"private _a = 1; [] spawn {_a};"#;

    let mut state = State::default();
    parse_analyze_s(case, &mut state);
    assert_eq!(state.origins, HashMap::default());
}
