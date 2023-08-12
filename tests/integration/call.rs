use std::collections::HashMap;

use sqf::{
    analyzer::{Origin, Output, Parameter, State},
    span::Spanned,
    types::Type,
};

use super::analyser::{parse_analyze, parse_analyze_s};

#[test]
fn call_len_args() {
    let case = r#"private _a = {params ["_a"]}; [1, 2] call _a;"#;

    let state = parse_analyze(case);
    assert_eq!(
        state.errors,
        vec![Spanned {
            inner: "Function expects 1 parameters, but received 2".to_string(),
            span: (30, 36)
        }]
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
        "A_fn_a".to_string().into(),
        (
            Origin::External("A_fn_a".to_string().into()),
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
        HashMap::from([("_a".into(), ((8, 10), Some(Type::Boolean.into())))])
    );
    assert_eq!(
        state.origins,
        HashMap::from([((22, 28), Origin::External("A_fn_a".to_string().into()))])
    );
    assert_eq!(state.parameters, HashMap::from([((14, 15), "_a".into())]));
}

#[test]
fn used_default_arg() {
    let case = r#"private _a = {params ["_a", ["_b", []]]}; [1] call _a;"#;

    let state = parse_analyze(case);
    assert_eq!(state.errors, vec![]);
}
