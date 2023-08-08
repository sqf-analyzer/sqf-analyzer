use std::collections::HashMap;

use sqf::span::Spanned;

use super::analyser::parse_analyze;

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
