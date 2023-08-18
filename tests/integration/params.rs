use sqf::{
    analyzer::{Parameter, State},
    error::{Error, ErrorType},
    span::Spanned,
    types::Type,
    uncased,
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
            inner: uncased("_a"),
        },
        Some(Type::Anything.into()),
        true,
    );
    expected.namespace.stack.last_mut().unwrap().signature = Some(vec![Parameter {
        name: "_a".into(),
        type_: Type::Anything,
        has_default: false,
    }]);
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn basic_error() {
    let case = "params [\"a\"]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected
        .errors
        .push(Error::new(ErrorType::GlobalVariableParam, (8, 11)));
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );

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
            inner: uncased("_a"),
        },
        Some(Type::Boolean.into()),
        true,
    );
    expected.namespace.stack.last_mut().unwrap().signature = Some(vec![Parameter {
        name: "_a".into(),
        type_: Type::Boolean,
        has_default: true,
    }]);
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );
    expected.explanations.insert((15, 19), "Always true.");

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
            inner: uncased("_a"),
        },
        Some(Type::Boolean.into()),
        true,
    );
    expected.namespace.stack.last_mut().unwrap().signature = Some(vec![Parameter {
        name: "_a".into(),
        type_: Type::Boolean,
        has_default: true,
    }]);
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );
    expected.explanations.insert((15, 19), "Always true.");
    expected.explanations.insert((22, 26), "Always true.");

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
            inner: uncased("_a"),
        },
        Some(Type::Boolean.into()),
        true,
    );
    expected.errors.push(Error::new(
        ErrorType::IncompatibleParamArgument(Type::Object, Type::Boolean),
        (8, 31),
    ));
    expected.namespace.stack.last_mut().unwrap().signature = Some(vec![Parameter {
        name: "_a".into(),
        type_: Type::Boolean,
        has_default: true,
    }]);
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );
    expected.explanations.insert(
        (15, 22),
        "A non-existing object. This value is not equal to anything, including itself.",
    );
    expected.explanations.insert((25, 29), "Always true.");

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
            inner: uncased("_a"),
        },
        Some(Type::Anything.into()),
        true,
    );
    expected.namespace.stack.last_mut().unwrap().signature = Some(vec![Parameter {
        name: "_a".into(),
        type_: Type::Anything,
        has_default: true,
    }]);
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );
    expected.explanations.insert(
        (15, 22),
        "A non-existing object. This value is not equal to anything, including itself.",
    );
    expected.explanations.insert((25, 29), "Always true.");
    expected.explanations.insert(
        (31, 38),
        "A non-existing object. This value is not equal to anything, including itself.",
    );

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
            inner: uncased("_a"),
        },
        Some(Type::Anything.into()),
        true,
    );
    expected.namespace.stack.last_mut().unwrap().signature = Some(vec![Parameter {
        name: "_a".into(),
        type_: Type::Anything,
        has_default: true,
    }]);
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.errors.push(Error::new(
        "params' third argument's elements must be typed".to_string(),
        (24, 28),
    ));
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );
    expected.explanations.insert(
        (15, 22),
        "A non-existing object. This value is not equal to anything, including itself.",
    );

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn basic_with_invalid_type_param() {
    let case = "params [[\"_a\", objNull, 1]]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected
        .errors
        .push(Error::new(ErrorType::ExpectedType(Type::Array), (24, 25)));
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );
    expected.explanations.insert(
        (15, 22),
        "A non-existing object. This value is not equal to anything, including itself.",
    );

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn basic_with_invalid_param() {
    let case = "params [1]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.errors.push(Error::new(
        "params' argument must be either a string or array".to_string(),
        (8, 9),
    ));
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn error_too_many_params() {
    let case = "params [['_a', 1, [1], 5, 6]]";

    let state = parse_analyze(case);
    assert_eq!(
        state.errors,
        vec![Error::new(
            "params' arguments only accept up to 4 arguments".to_string(),
            (26, 27),
        )]
    );
}

#[test]
fn basic_with_invalid_param_array() {
    let case = "params [[1]]";

    let state = parse_analyze(case);
    assert_eq!(
        state.errors,
        vec![Error::new(ErrorType::ExpectedType(Type::String), (8, 11),)]
    );
}

/// signatures are correct when used
#[test]
fn with_code() {
    let case = r#"params [["_callback", {}, [{}]]];"#;

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        Spanned {
            span: (9, 20),
            inner: uncased("_callback"),
        },
        Some(Type::Code.into()),
        true,
    );
    expected.namespace.stack.last_mut().unwrap().signature = Some(vec![Parameter {
        name: "_callback".into(),
        type_: Type::Code,
        has_default: true,
    }]);
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);
    expected.explanations.insert(
        (0, 6),
        "Parses _this param inside a script into array of private variables",
    );

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}
