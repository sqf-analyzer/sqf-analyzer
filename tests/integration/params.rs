use sqf::{
    analyzer::{Parameter, State},
    span::Spanned,
    types::Type,
};

use crate::analyser::parse_analyze;

#[test]
fn basic_no_errors() {
    let case = "params [\"_a\"]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        Spanned {
            span: (8, 12),
            inner: "_a".to_string(),
        },
        Some(Type::Anything.into()),
        true,
    );
    expected.parameters = Some(vec![Parameter {
        name: "_a".to_string(),
        type_: Type::Anything,
    }]);

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn basic_error() {
    let case = "params [\"a\"]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.errors.push(Spanned {
        inner: "Argument must begin with _".to_string(),
        span: (8, 11),
    });
    expected.parameters = Some(vec![]);

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn with_default() {
    let case = "params [[\"_a\", true]]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        Spanned {
            span: (9, 13),
            inner: "_a".to_string(),
        },
        Some(Type::Anything.into()),
        true,
    );
    expected.parameters = Some(vec![Parameter {
        name: "_a".to_string(),
        type_: Type::Anything,
    }]);

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn with_default_and_type() {
    let case = "params [[\"_a\", true, [true]]]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        Spanned {
            span: (9, 13),
            inner: "_a".to_string(),
        },
        Some(Type::Boolean.into()),
        true,
    );
    expected.parameters = Some(vec![Parameter {
        name: "_a".to_string(),
        type_: Type::Boolean,
    }]);

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn with_default_and_type_invalid_default() {
    let case = "params [[\"_a\", objNull, [true]]]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        Spanned {
            span: (9, 13),
            inner: "_a".to_string(),
        },
        Some(Type::Boolean.into()),
        true,
    );
    expected.errors.push(Spanned {
        inner: "params' default argument type \"Object\" is inconsistent with expected type \"Boolean\"".to_string(),
        span: (8, 31),
    });
    expected.parameters = Some(vec![Parameter {
        name: "_a".to_string(),
        type_: Type::Boolean,
    }]);

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn with_two_types() {
    let case = "params [[\"_a\", objNull, [true, objNull]]]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        Spanned {
            span: (9, 13),
            inner: "_a".to_string(),
        },
        Some(Type::Anything.into()),
        true,
    );
    expected.parameters = Some(vec![Parameter {
        name: "_a".to_string(),
        type_: Type::Anything,
    }]);

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn basic_with_unknown_type() {
    let case = "params [[\"_a\", objNull, [_b]]]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        Spanned {
            span: (9, 13),
            inner: "_a".to_string(),
        },
        Some(Type::Anything.into()),
        true,
    );
    expected.parameters = Some(vec![Parameter {
        name: "_a".to_string(),
        type_: Type::Anything,
    }]);
    expected.errors.push(Spanned {
        inner: "params' third argument's elements must be typed".to_string(),
        span: (24, 28),
    });

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}
