use std::sync::Arc;

use crate::span::{Span, Spanned};

#[derive(Debug, Clone)]
pub enum Expr {
    String(Spanned<Arc<str>>),
    Number(Spanned<f64>),
    Nullary(Spanned<Arc<str>>),
    Variable(Spanned<Arc<str>>),
    Unary(Spanned<Arc<str>>, Box<Expr>),
    Binary(Box<Expr>, Spanned<Arc<str>>, Box<Expr>),
    Code(Spanned<Vec<Expr>>),
    Array(Spanned<Vec<Expr>>),
    Nil(Span), // returned on error
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Self::String(a) => a.span,
            Self::Number(a) => a.span,
            Self::Nullary(a) => a.span,
            Self::Variable(a) => a.span,
            Self::Unary(a, expr) => {
                let (start, _) = a.span;
                let (_, end) = expr.span();
                (start, end)
            }
            Self::Binary(lhs, _, rhs) => {
                let (start, _) = lhs.span();
                let (_, end) = rhs.span();
                (start, end)
            }
            Self::Code(expr) | Self::Array(expr) => expr.span,
            Self::Nil(span) => *span,
        }
    }
}
