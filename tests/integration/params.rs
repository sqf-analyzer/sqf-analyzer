use sqf::{
    analyzer::{Parameter, State},
    error::{Error, ErrorType},
    types::Type,
    uncased,
};

use crate::analyser::{file, parse_analyze};

#[test]
fn basic_no_errors() {
    let case = "params [\"_a\"]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        uncased("_a"),
        Some(Type::Anything.into()),
        file(8, 12),
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
    expected.errors = vec![Error::new(ErrorType::UnusedVariable, (8, 12))];

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn basic_error() {
    let case = "params [\"a\"]";

    let mut expected = State::default();
    expected.namespace.push_stack();

    expected.errors = vec![Error::new(ErrorType::GlobalVariableParam, (8, 11))];
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
        uncased("_a"),
        Some(Type::Anything.into()),
        file(9, 13),
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
    expected.explanations.insert((15, 19), "Always true.");
    expected.errors = vec![Error::new(ErrorType::UnusedVariable, (9, 13))];

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn with_default_and_type() {
    let case = "params [[\"_a\", true, [true]]]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected
        .namespace
        .insert(uncased("_a"), Some(Type::Boolean.into()), file(9, 13), true);
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

    expected.errors = vec![Error::new(ErrorType::UnusedVariable, (9, 13))];

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn with_default_and_type_invalid_default() {
    let case = "params [[\"_a\", objNull, [true]]]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected
        .namespace
        .insert(uncased("_a"), Some(Type::Boolean.into()), file(9, 13), true);

    expected.errors = vec![
        Error::new(
            ErrorType::IncompatibleParamArgument(Type::Object, Type::Boolean),
            (8, 31),
        ),
        Error::new(ErrorType::UnusedVariable, (9, 13)),
    ];
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
        uncased("_a"),
        Some(Type::Anything.into()),
        file(9, 13),
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
    expected.errors = vec![Error::new(ErrorType::UnusedVariable, (9, 13))];

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}

#[test]
fn basic_with_unknown_type() {
    let case = "params [[\"_a\", objNull, [_b]]]";

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        uncased("_a"),
        Some(Type::Anything.into()),
        file(9, 13),
        true,
    );
    expected.namespace.stack.last_mut().unwrap().signature = Some(vec![Parameter {
        name: "_a".into(),
        type_: Type::Anything,
        has_default: true,
    }]);
    expected.namespace.stack.last_mut().unwrap().return_type = Some(Type::Boolean);

    expected.errors = vec![
        Error::new(ErrorType::UnusedVariable, (9, 13)),
        Error::new(
            "params' third argument's elements must be typed".to_string(),
            (24, 28),
        ),
        Error::new(ErrorType::UndefinedVariable("_b".into()), (25, 27)),
        Error::new(ErrorType::UndefinedVariable("_b".into()), (25, 27)),
    ];
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
    expected.errors = vec![Error::new(ErrorType::ExpectedType(Type::Array), (24, 25))];
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
    expected.errors = vec![Error::new(
        "params' argument must be either a string or array".to_string(),
        (8, 9),
    )];
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
        vec![
            Error::new(ErrorType::UnusedVariable, (9, 13)),
            Error::new(
                "params' arguments only accept up to 4 arguments".to_string(),
                (26, 27),
            ),
        ]
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

#[test]
fn single_array_arg() {
    let case = "params [[\"_a\"]]";

    let state = parse_analyze(case);
    assert_eq!(
        state.errors,
        vec![Error::new(ErrorType::UnusedVariable, (9, 13))]
    );
}

/// signatures are correct when used
#[test]
fn with_code() {
    let case = r#"params [["_callback", {}, [{}]]];"#;

    let mut expected = State::default();
    expected.namespace.push_stack();
    expected.namespace.insert(
        uncased("_callback"),
        Some(Type::Code.into()),
        file(9, 20),
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
    expected.errors = vec![Error::new(ErrorType::UnusedVariable, (9, 20))];

    let state = parse_analyze(case);
    assert_eq!(state, expected);
}
