use std::borrow::Cow;
use std::path::PathBuf;

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

pub type SpannedRef<'a> = Spanned<Cow<'a, str>>;

impl<'a> From<Pair<'a, parser::Rule>> for SpannedRef<'a> {
    fn from(value: Pair<'a, parser::Rule>) -> Self {
        Self {
            span: (value.as_span().start(), value.as_span().end()),
            inner: value.as_str().into(),
        }
    }
}

pub fn parse(data: &str) -> Result<Ast, Error> {
    parser::pairs(data).map(parser::parse)
}

pub fn tokens(data: &str, defines: Defines, path: PathBuf) -> Result<AstIterator, Error> {
    parse(data).map(|ast| AstIterator::new(ast, defines, path))
}
