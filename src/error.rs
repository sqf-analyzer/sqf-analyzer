use std::{path::Path, sync::Arc};

use crate::span::Span;

#[derive(Debug, Clone, PartialEq)]
pub struct Error {
    pub inner: String,
    pub span: Span,
    pub origin: Option<Arc<Path>>,
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
