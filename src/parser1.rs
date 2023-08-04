use std::{collections::HashSet, fmt, iter::Peekable};

use crate::{
    database::SIGNATURES,
    error::Error,
    preprocessor::AstIterator,
    types::{Signature, Spanned},
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token<'a> {
    Atom(Spanned<&'a str>),
    Op(Spanned<&'a str>),
    Eof,
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

fn code<'a, I: Iterator<Item = Token<'a>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> Vec<Expr<'a>> {
    let mut expressions = vec![];
    if matches(iter.peek(), "}") {
        return expressions;
    };

    while iter.peek().unwrap_or(&Token::Eof) != &Token::Eof {
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

fn array<'a, I: Iterator<Item = Token<'a>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> Vec<Expr<'a>> {
    let mut expressions = vec![];
    if matches(iter.peek(), "]") {
        return expressions;
    };
    while iter.peek().unwrap_or(&Token::Eof) != &Token::Eof {
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

// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html#Introduction
pub fn parse(iter: AstIterator) -> (Vec<Expr<'_>>, Vec<Error>) {
    let mut iter = iter
        .map(|x| {
            if _is_op(x.inner) {
                Token::Op(x)
            } else {
                Token::Atom(x)
            }
        })
        .chain(std::iter::once(Token::Eof))
        .peekable();
    let mut errors = vec![];
    (code(&mut iter, &mut errors), errors)
}

#[inline]
fn matches(token: Option<&Token>, v: &str) -> bool {
    if let Some(Token::Op(Spanned { inner, .. })) = token {
        *inner == v
    } else {
        false
    }
}

/// This function returns under 3 conditions
/// 1. Reached end of file
/// 2. Reached an operator with a binding power < min_bp
/// 3. Reached a ";" or ","
fn expr_bp<'a, I: Iterator<Item = Token<'a>>>(
    lexer: &mut Peekable<I>,
    min_bp: u8,
    errors: &mut Vec<Error>,
) -> Expr<'a> {
    let mut lhs = match lexer.next().unwrap_or(Token::Eof) {
        Token::Atom(it) => atom_to_expr(it),
        Token::Op(Spanned { inner: "(", span }) => {
            let lhs = expr_bp(lexer, 0, errors);

            if !matches(lexer.next().as_ref(), ")") {
                errors.push(Error {
                    inner: "\"(\" is not closed".to_string(),
                    span,
                })
            }

            lhs
        }
        Token::Op(Spanned { inner: ";", .. }) => Expr::Code(vec![]),
        Token::Op(Spanned { inner: "{", span }) => {
            let expr = code(lexer, errors);

            if !matches(lexer.next().as_ref(), "}") {
                errors.push(Error {
                    inner: "\"{\" is not closed".to_string(),
                    span,
                })
            }

            Expr::Code(expr)
        }
        Token::Op(Spanned { inner: "[", span }) => {
            let expr = array(lexer, errors);

            if !matches(lexer.next().as_ref(), "]") {
                errors.push(Error {
                    inner: "\"[\" is not closed".to_string(),
                    span,
                })
            }

            Expr::Array(expr)
        }
        Token::Op(op) => {
            let ((), r_bp) = prefix_binding_power(op.inner).unwrap_or_else(|| {
                errors.push(Error {
                    inner: format!("\"{}\" is not a valid unary operator", op.inner),
                    span: op.span,
                });
                ((), 50)
            });
            let rhs = expr_bp(lexer, r_bp, errors);
            Expr::Unary(op, Box::new(rhs))
        }
        Token::Eof => {
            errors.push(Error {
                inner: "Un-expected end of file".to_string(),
                span: (0, 0),
            });
            return Expr::Nil;
        }
    };

    loop {
        let op = match lexer.peek().unwrap_or(&Token::Eof) {
            Token::Eof => break,
            Token::Op(Spanned { inner: ";", .. }) => break,
            Token::Op(Spanned { inner: ",", .. }) => break,
            Token::Op(op) => *op,
            Token::Atom(primary) => {
                errors.push(Error {
                    inner: format!("Un-expected value \"{}\"", primary.inner),
                    span: primary.span,
                });
                // provide _something_
                return Expr::Unary(*primary, Box::new(lhs));
            }
        };

        if let Some((l_bp, ())) = postfix_binding_power(op.inner) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();

            lhs = Expr::Unary(op, Box::new(lhs));
            continue;
        }

        if let Some((l_bp, r_bp)) = infix_binding_power(op.inner) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();

            let rhs = expr_bp(lexer, r_bp, errors);
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
            continue;
        }

        break;
    }
    lhs
}

fn prefix_binding_power(op: &str) -> Option<((), u8)> {
    if UNARY.contains(op) {
        // https://foxhound.international/precedence-arma-3-sqf.html
        Some(((), 50))
    } else {
        None
    }
}

fn postfix_binding_power(_: &str) -> Option<(u8, ())> {
    None
}

fn infix_binding_power(op: &str) -> Option<(u8, u8)> {
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
