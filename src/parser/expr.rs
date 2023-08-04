use std::fmt;

use crate::span::Spanned;

#[derive(Clone)]
pub enum Expr<'a> {
    String(Spanned<&'a str>),
    Number(Spanned<i64>),
    Nullary(Spanned<&'a str>),
    Boolean(Spanned<bool>),
    Variable(Spanned<&'a str>),
    Unary(Spanned<&'a str>, Box<Expr<'a>>),
    Binary(Box<Expr<'a>>, Spanned<&'a str>, Box<Expr<'a>>),
    Code(Vec<Expr<'a>>),
    Array(Vec<Expr<'a>>),
    Nil, // returned on error
}

impl<'a> fmt::Debug for Expr<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Expr::Nullary(i) | Expr::String(i) | Expr::Variable(i) => {
                write!(f, "{}", i.inner)
            }
            Expr::Boolean(i) => {
                write!(f, "{}", i.inner)
            }
            Expr::Number(i) => {
                write!(f, "{}", i.inner)
            }
            Expr::Unary(head, rhs) => {
                write!(f, "({} {:?})", head.inner, rhs)
            }
            Expr::Binary(lhs, head, rhs) => {
                write!(f, "({:?} {} {:?})", lhs, head.inner, rhs)
            }
            Expr::Code(rest) => {
                write!(f, "{{")?;
                for s in rest {
                    write!(f, "{:?};", s)?
                }
                write!(f, "}}")
            }
            Expr::Array(rest) => {
                write!(f, "[")?;
                for s in rest {
                    write!(f, "{:?},", s)?
                }
                write!(f, "]")
            }
            Expr::Nil => {
                write!(f, "nil")
            }
        }
    }
}
