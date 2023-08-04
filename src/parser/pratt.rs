/// Inspired by https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
use std::{collections::HashSet, iter::Peekable};

use super::Expr;

use crate::{
    database::SIGNATURES, error::Error, preprocessor::AstIterator, span::Spanned, types::Signature,
};

lazy_static::lazy_static! {

    pub static ref BINARY: HashSet<&'static str> = SIGNATURES
        .iter()
        .filter_map(|x| {
            if let Signature::Binary(_, name, _, _) = x {
                Some(*name)
            } else {
                None
            }
        })
        .collect::<HashSet<_, _>>();

    pub static ref UNARY: HashSet<&'static str> = SIGNATURES
        .iter()
        .filter_map(|x| {
            if let Signature::Unary(name, _, _) = x {
                Some(*name)
            } else {
                None
            }
        })
        .collect::<HashSet<_, _>>();

    pub static ref NULLARY: HashSet<&'static str> = SIGNATURES
        .iter()
        .filter_map(|x| {
            if let Signature::Nullary(name, _) = x {
                Some(*name)
            } else {
                None
            }
        })
        .collect::<HashSet<_, _>>();
}

fn atom_to_expr(token: Spanned<&str>) -> Expr {
    NULLARY
        .contains(token.inner)
        .then(|| Expr::Nullary(token))
        .or_else(|| {
            // bool
            if token.inner == "true" {
                Some(Expr::Boolean(Spanned {
                    inner: true,
                    span: token.span,
                }))
            } else if token.inner == "false" {
                Some(Expr::Boolean(Spanned {
                    inner: false,
                    span: token.span,
                }))
            } else {
                None
            }
        })
        .or_else(|| {
            // number
            token
                .inner
                .parse::<i64>()
                .map(|x| Spanned {
                    inner: x,
                    span: token.span,
                })
                .map(Expr::Number)
                .ok()
        })
        .or_else(|| {
            // string
            let bytes = token.inner.as_bytes();
            if bytes.first() == Some(&b'"') && bytes.last() == Some(&b'"') {
                Some(Expr::String(token))
            } else {
                None
            }
        })
        .unwrap_or_else(|| Expr::Variable(token))
}

fn _is_op(token: &str) -> bool {
    let token = token.to_owned().to_ascii_lowercase();
    UNARY.contains(token.as_str())
        || BINARY.contains(token.as_str())
        || matches!(
            token.as_str(),
            "}" | ")" | "]" | "{" | "(" | "[" | ";" | "=" | ","
        )
}

fn code<'a, I: Iterator<Item = Spanned<&'a str>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> Vec<Expr<'a>> {
    let mut expressions = vec![];
    if matches(iter.peek(), "}") {
        return expressions;
    };

    while iter.peek().is_some() {
        let expression = expr_bp(iter, 0, errors);
        expressions.push(expression);

        if matches(iter.peek(), ";") {
            iter.next().unwrap();
        }
        if matches(iter.peek(), "}") {
            break;
        }
    }
    expressions
}

fn array<'a, I: Iterator<Item = Spanned<&'a str>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> Vec<Expr<'a>> {
    let mut expressions = vec![];
    if matches(iter.peek(), "]") {
        return expressions;
    };
    while iter.peek().is_some() {
        let expression = expr_bp(iter, 0, errors);
        expressions.push(expression);

        if matches(iter.peek(), ",") {
            iter.next().unwrap();
        }
        if matches(iter.peek(), "]") {
            break;
        }
    }
    expressions
}

pub fn parse(iter: AstIterator) -> (Vec<Expr<'_>>, Vec<Error>) {
    let mut iter = iter.peekable();
    let mut errors = vec![];
    (code(&mut iter, &mut errors), errors)
}

#[inline]
fn matches(token: Option<&Spanned<&str>>, v: &str) -> bool {
    if let Some(Spanned { inner, .. }) = token {
        *inner == v
    } else {
        false
    }
}

/// This function returns under 3 conditions
/// 1. Reached end of file
/// 2. Reached an operator with a binding power < min_bp
/// 3. Reached a ";" or ","
fn expr_bp<'a, I: Iterator<Item = Spanned<&'a str>>>(
    lexer: &mut Peekable<I>,
    min_bp: u8,
    errors: &mut Vec<Error>,
) -> Expr<'a> {
    let mut lhs = match lexer.next() {
        Some(Spanned { inner: "(", span }) => {
            let lhs = expr_bp(lexer, 0, errors);

            if !matches(lexer.next().as_ref(), ")") {
                errors.push(Error {
                    inner: "\"(\" is not closed".to_string(),
                    span,
                })
            }

            lhs
        }
        Some(Spanned { inner: ";", .. }) => Expr::Code(vec![]),
        Some(Spanned { inner: "{", span }) => {
            let expr = code(lexer, errors);

            if !matches(lexer.next().as_ref(), "}") {
                errors.push(Error {
                    inner: "\"{\" is not closed".to_string(),
                    span,
                })
            }

            Expr::Code(expr)
        }
        Some(Spanned { inner: "[", span }) => {
            let expr = array(lexer, errors);

            if !matches(lexer.next().as_ref(), "]") {
                errors.push(Error {
                    inner: "\"[\" is not closed".to_string(),
                    span,
                })
            }

            Expr::Array(expr)
        }
        Some(op) => {
            if let Some(((), r_bp)) = prefix_binding_power(op.inner) {
                let rhs = expr_bp(lexer, r_bp, errors);
                Expr::Unary(op, Box::new(rhs))
            } else {
                atom_to_expr(op)
            }
        }
        None => {
            errors.push(Error {
                inner: "Un-expected end of file".to_string(),
                span: (0, 0),
            });
            return Expr::Nil;
        }
    };

    loop {
        let op = match lexer.peek() {
            None => break,
            Some(Spanned { inner: ";", .. }) => break,
            Some(Spanned { inner: ",", .. }) => break,
            Some(op) => *op,
        };

        if let Some((l_bp, r_bp)) = infix_binding_power(op.inner) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();

            let rhs = expr_bp(lexer, r_bp, errors);
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
            continue;
        } else if op.inner == "}" || op.inner == "]" || op.inner == ")" {
        } else {
            errors.push(Error {
                inner: format!("\"{}\" is not a valid binary operator", op.inner),
                span: op.span,
            });
        }

        break;
    }
    lhs
}

fn prefix_binding_power(op: &str) -> Option<((), u8)> {
    let op = op.to_owned().to_lowercase();
    let op = op.as_str();

    if UNARY.contains(op) {
        // https://foxhound.international/precedence-arma-3-sqf.html
        Some(((), 50))
    } else {
        None
    }
}

fn infix_binding_power(op: &str) -> Option<(u8, u8)> {
    let op = op.to_owned().to_lowercase();
    let op = op.as_str();

    // https://foxhound.international/precedence-arma-3-sqf.html
    let res = match op {
        ";" => (1, 2),
        "=" => (19, 20), // assign op has the least binding power
        "or" | "||" => (21, 22),
        "and" | "&&" => (23, 24),
        "==" | "!=" | ">" | "<" | ">=" | "<=" | ">>" => (25, 26),
        // binary op below => (27, 28)
        "else" => (29, 30),
        "+" | "-" | "max" | "min" => (31, 32),
        "*" | "/" | "%" | "mod" | "atan2" => (33, 34),
        "^" => (34, 35),
        _ => {
            if !BINARY.contains(op) {
                return None;
            }
            (27, 28)
        }
    };
    Some(res)
}
