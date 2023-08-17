use std::path::PathBuf;

use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub inner: String,
    pub span: Span,
    pub origin: Option<PathBuf>,
}

impl Error {
    pub fn new(inner: String, span: Span) -> Self {
        Self {
            inner,
            span,
            origin: None,
        }
    }
}
