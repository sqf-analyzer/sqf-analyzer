use std::{path::Path, sync::Arc};

use crate::{span::Span, types::Type};

#[derive(Debug, Clone, PartialEq)]
pub enum ErrorType {
    GlobalVariableParam,
    IncompatibleParamArgument(Type, Type),

    InvalidType(Arc<str>, Type),

    /// an undefined varible has been used
    UndefinedVariable,
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

impl ToString for ErrorType {
    fn to_string(&self) -> String {
        match self {
            ErrorType::GlobalVariableParam => "Parameter variable must start with _".to_string(),
            ErrorType::IncompatibleParamArgument(t1, t2) => {
                format!("Param has a default of type {t1:?} but expects type {t2:?}")
            }
            ErrorType::InvalidType(expected, got) => {
                format!("Expected type {expected:?}, but got {got:?}")
            }
            ErrorType::UndefinedVariable => "undefined variable".to_string(),
            ErrorType::PrivateAssignedToMission => {
                "variable with underscore assigned to global space".to_string()
            }
            ErrorType::InsufficientArguments { expected, passed } => {
                format!("function expected {expected} arguments, but got {passed}")
            }
            ErrorType::InvalidBinaryOperator => "invalid binary operator".to_string(),
            ErrorType::ExpectedType(t) => format!("Expected type {t:?}"),
            ErrorType::UnexpectedEndOfFile => "unclosed parenthesis before end of file".to_string(),
            ErrorType::UnclosedParenthesis => "unclosed parenthesis".to_string(),
            ErrorType::Other(e) => e.to_string(),
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
