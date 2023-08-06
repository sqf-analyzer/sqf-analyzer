use crate::{
    preprocessor::SpannedRef,
    span::{Span, Spanned},
};

#[derive(Debug, Clone)]
pub enum Expr<'a> {
    String(SpannedRef<'a>),
    Number(Spanned<i64>),
    Nullary(SpannedRef<'a>),
    Boolean(Spanned<bool>),
    Variable(SpannedRef<'a>),
    Unary(SpannedRef<'a>, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, SpannedRef<'a>, Box<Expr<'a>>),
    Code(Spanned<Vec<Expr<'a>>>),
    Array(Spanned<Vec<Expr<'a>>>),
    Nil(Span), // returned on error
}

impl<'a> Expr<'a> {
    pub fn span(&self) -> Span {
        match self {
            Self::String(a) => a.span,
            Self::Number(a) => a.span,
            Self::Nullary(a) => a.span,
            Self::Boolean(a) => a.span,
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
