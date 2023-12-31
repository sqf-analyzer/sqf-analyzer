use std::collections::VecDeque;
use std::sync::Arc;

use pest::iterators::Pair;

use crate::error::Error;
use crate::span::Spanned;

mod ast;
mod define;
mod include;
mod iterator;
mod parser;

pub use ast::{Ast, Define, Defines};
pub use iterator::AstIterator;

pub use self::iterator::Configuration;

pub(crate) fn parse_hexadecimal(v: &str) -> Option<usize> {
    v.starts_with("0x")
        .then(|| usize::from_str_radix(v.trim_start_matches("0x"), 16).ok())
        .flatten()
}

impl<'a> From<Pair<'a, parser::Rule>> for Spanned<Arc<str>> {
    fn from(value: Pair<'a, parser::Rule>) -> Self {
        Self {
            span: (value.as_span().start(), value.as_span().end()),
            inner: value.as_str().into(),
        }
    }
}

pub fn parse(text: &str) -> Result<VecDeque<Ast<'_>>, Error> {
    parser::pairs(text).map(parser::parse)
}

#[allow(clippy::result_large_err)]
pub fn tokens(
    text: &str,
    configuration: Configuration,
) -> Result<AstIterator, (Configuration, Error)> {
    match parse(text) {
        Ok(ast) => Ok(AstIterator::new(ast, configuration)),
        Err(e) => Err((configuration, e)),
    }
}
