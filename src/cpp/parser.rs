use std::collections::VecDeque;
use std::iter::Peekable;
use std::sync::Arc;

use crate::preprocessor::parse_hexadecimal;
use crate::{
    error::Error,
    preprocessor::AstIterator,
    span::{Span, Spanned},
};

#[derive(Clone, Debug)]
pub enum Expr {
    Number(Spanned<f32>),
    Boolean(Spanned<bool>),
    GameType(Spanned<Arc<str>>),
    String(Spanned<Arc<str>>),
    Token(Spanned<Arc<str>>),
    Code(Spanned<VecDeque<Expr>>),
    Array(Span),
    Expr(Spanned<VecDeque<Expr>>),
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::GameType(o) | Expr::Token(o) | Expr::String(o) => o.span,
            Expr::Code(o) => o.span,
            Expr::Number(o) => o.span,
            Expr::Boolean(o) => o.span,
            Expr::Array(s) => *s,
            Expr::Expr(s) => s.span,
        }
    }
}

#[inline]
fn matches(token: Option<&Spanned<Arc<str>>>, v: &str) -> bool {
    if let Some(Spanned { inner, .. }) = token {
        inner.as_ref() == v
    } else {
        false
    }
}

fn expression<I: Iterator<Item = Spanned<Arc<str>>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> VecDeque<Expr> {
    let mut expressions = Default::default();
    if matches(iter.peek(), ")") {
        return expressions;
    };

    while iter.peek().is_some() {
        let expression = expr(iter, errors);
        expressions.push_back(expression);

        if matches(iter.peek(), ")") {
            break;
        }
    }
    expressions
}

fn code<I: Iterator<Item = Spanned<Arc<str>>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> VecDeque<Expr> {
    let mut expressions = Default::default();
    if matches(iter.peek(), "}") {
        return expressions;
    };

    while iter.peek().is_some() {
        let expression = expr(iter, errors);
        expressions.push_back(expression);

        if matches(iter.peek(), ";") {
            iter.next().unwrap();
        }
        if matches(iter.peek(), "}") {
            break;
        }
    }
    expressions
}

fn expr<I: Iterator<Item = Spanned<Arc<str>>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> Expr {
    let Spanned { inner, span } = iter.next().expect("function invariant");
    match inner.as_ref() {
        ";" => Expr::Code(Spanned {
            inner: Default::default(),
            span,
        }),
        "{" => {
            let expr = code(iter, errors);
            let last = iter.next();
            if !matches(last.as_ref(), "}") {
                errors.push(Error {
                    inner: "\"{\" is not closed".to_string(),
                    span,
                })
            }
            let start = span.0;
            let end = last.map(|x| x.span.1).unwrap_or(start);

            Expr::Code(Spanned {
                inner: expr,
                span: (start, end),
            })
        }
        "[" => {
            let last = iter.next();
            if !matches(last.as_ref(), "]") {
                errors.push(Error {
                    inner: "\"[\" is not closed".to_string(),
                    span,
                })
            }
            let start = span.0;
            let end = last.map(|x| x.span.1).unwrap_or(start);

            Expr::Array((start, end))
        }
        "(" => {
            let expr = expression(iter, errors);
            let last = iter.next();
            if !matches(last.as_ref(), ")") {
                errors.push(Error {
                    inner: "\"(\" is not closed".to_string(),
                    span,
                })
            }
            let start = span.0;
            let end = last.map(|x| x.span.1).unwrap_or(start);

            Expr::Expr(Spanned {
                inner: expr,
                span: (start, end),
            })
        }
        _ => token_to_expr(Spanned { inner, span }),
    }
}

pub fn parse(mut iter: AstIterator) -> (VecDeque<Expr>, Vec<Error>) {
    let mut peekable = iter.by_ref().peekable();
    let mut errors = vec![];
    let expr = code(&mut peekable, &mut errors);
    errors.extend(iter.state.errors);
    (expr, errors)
}

fn token_to_expr(token: Spanned<Arc<str>>) -> Expr {
    let bytes = token.inner.as_bytes();
    if bytes.first() == Some(&b'"') && bytes.last() == Some(&b'"') {
        Expr::String(
            token
                .as_ref()
                .map(|x| x.get(1..x.len() - 1).unwrap().to_owned().into()),
        )
    } else if bytes == b"true" {
        Expr::Boolean(token.map(|_| true))
    } else if bytes == b"false" {
        Expr::Boolean(token.map(|_| false))
    } else if bytes.eq_ignore_ascii_case(b"coop") {
        Expr::GameType(token)
    } else if let Ok(number) = token.inner.parse::<f32>() {
        Expr::Number(Spanned {
            inner: number,
            span: token.span,
        })
    } else if let Some(int) = parse_hexadecimal(&token.inner) {
        Expr::Number(Spanned {
            inner: int as f32,
            span: token.span,
        })
    } else {
        Expr::Token(token)
    }
}
