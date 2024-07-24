use std::{path::Path, sync::Arc};

use crate::{span::Span, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
    GlobalVariableParam,
    IncompatibleParamArgument(Type, Type),

    InvalidType(Arc<str>, Type),

    /// an undefined varible has been used
    UndefinedVariable(Arc<str>),
    /// a defined varible that is not being used
    UnusedVariable,
    /// an underscored variable was assigned to the mission namespace
    PrivateAssignedToMission,
    InsufficientArguments {
        expected: usize,
        passed: usize,
    },
    InvalidBinaryOperator,
    ExpectedType(Type),
    UnexpectedEndOfFile,
    UnclosedParenthesis,
    Other(Arc<str>),
}

impl From<String> for ErrorType {
    fn from(value: String) -> Self {
        Self::Other(value.into())
    }
}

impl std::fmt::Display for ErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ErrorType::GlobalVariableParam => f.write_str("Parameter variable must start with _"),
            ErrorType::IncompatibleParamArgument(t1, t2) => f.write_fmt(format_args!(
                "Param has a default of type {t1:?} but expects type {t2:?}"
            )),
            ErrorType::InvalidType(expected, got) => {
                f.write_fmt(format_args!("Expected type {expected:?}, but got {got:?}"))
            }
            ErrorType::UndefinedVariable(name) => {
                f.write_fmt(format_args!("undefined variable \"{name}\""))
            }
            ErrorType::UnusedVariable => f.write_str("Unused variable"),
            ErrorType::PrivateAssignedToMission => {
                f.write_str("variable with underscore assigned to global space")
            }
            ErrorType::InsufficientArguments { expected, passed } => f.write_fmt(format_args!(
                "function expected {expected} arguments, but got {passed}"
            )),
            ErrorType::InvalidBinaryOperator => f.write_str("invalid binary operator"),
            ErrorType::ExpectedType(t) => f.write_fmt(format_args!("Expected type {t:?}")),
            ErrorType::UnexpectedEndOfFile => {
                f.write_str("unclosed parenthesis before end of file")
            }
            ErrorType::UnclosedParenthesis => f.write_str("unclosed parenthesis"),
            ErrorType::Other(e) => f.write_str(e),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub type_: ErrorType,
    pub span: Span,
    pub origin: Option<Arc<Path>>,
}

impl Error {
    pub fn new<R: Into<ErrorType>>(type_: R, span: Span) -> Self {
        Self {
            type_: type_.into(),
            span,
            origin: None,
        }
    }
}
