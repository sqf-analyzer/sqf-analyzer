/// Inspired by https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
use std::{collections::HashSet, iter::Peekable, sync::Arc};

use uncased::UncasedStr;

use crate::{
    database,
    error::{Error, ErrorType},
    preprocessor::{parse_hexadecimal, AstIterator},
    span::Spanned,
    types::Signature,
};

use super::Expr;

lazy_static::lazy_static! {

    pub static ref BINARY: HashSet<&'static UncasedStr> = database::BINARY
        .iter()
        .filter_map(|x| {
            if let Signature::Binary(_, name, _, _, _) = x {
                Some(UncasedStr::new(name))
            } else {
                None
            }
        })
        .collect::<HashSet<_, _>>();

    pub static ref UNARY: HashSet<&'static UncasedStr> = database::UNARY
        .iter()
        .filter_map(|x| {
            if let Signature::Unary(name, _, _, _) = x {
                Some(UncasedStr::new(name))
            } else {
                None
            }
        })
        .collect::<HashSet<_, _>>();

    pub static ref NULLARY: HashSet<&'static UncasedStr> = database::NULLARY
        .iter()
        .filter_map(|x| {
            if let Signature::Nullary(name, _, _) = x {
                Some(UncasedStr::new(name))
            } else {
                None
            }
        })
        .collect::<HashSet<_, _>>();
}

fn atom_to_expr(token: Spanned<Arc<str>>) -> Expr {
    NULLARY
        .contains(&UncasedStr::new(token.inner.as_ref()))
        .then(|| Expr::Nullary(token.clone()))
        .or_else(|| {
            // number
            token
                .inner
                .parse::<f64>()
                .map(|x| Spanned {
                    inner: x,
                    span: token.span,
                })
                .map(Expr::Number)
                .ok()
        })
        .or_else(|| {
            parse_hexadecimal(&token.inner)
                .map(|x| Spanned {
                    inner: x as f64,
                    span: token.span,
                })
                .map(Expr::Number)
        })
        .or_else(|| {
            // string
            let bytes = token.inner.as_bytes();
            let quoted = (bytes.first() == Some(&b'"') && bytes.last() == Some(&b'"'))
                | (bytes.first() == Some(&b'\'') && bytes.last() == Some(&b'\''));
            if quoted && bytes.len() >= 2 {
                Some(Expr::String(
                    token
                        .as_ref()
                        .map(|x| x.get(1..x.len() - 1).unwrap().to_owned().into()),
                ))
            } else {
                None
            }
        })
        .unwrap_or_else(|| Expr::Variable(token))
}

fn code<I: Iterator<Item = Spanned<Arc<str>>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> Vec<Expr> {
    let mut expressions = vec![];
    if matches(iter.peek(), "}") {
        return expressions;
    };

    while iter.peek().is_some() {
        while matches!(iter.peek().map(|x| x.inner.as_ref()), Some(";" | ",")) {
            iter.next().unwrap();
        }
        if matches(iter.peek(), "}") {
            break;
        }

        let expression = expr_bp(iter, 0, errors);
        expressions.push(expression);

        while matches!(iter.peek().map(|x| x.inner.as_ref()), Some(";" | ",")) {
            iter.next().unwrap();
        }

        if matches(iter.peek(), "}") {
            break;
        }
    }
    expressions
}

fn array<I: Iterator<Item = Spanned<Arc<str>>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> Vec<Expr> {
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

pub fn parse(mut iter: AstIterator) -> (Vec<Expr>, Vec<Error>) {
    let mut peekable = iter.by_ref().peekable();
    let mut errors = vec![];
    let expr = code(&mut peekable, &mut errors);
    errors.extend(iter.state.errors);
    (expr, errors)
}

#[inline]
fn matches(token: Option<&Spanned<Arc<str>>>, v: &str) -> bool {
    if let Some(Spanned { inner, .. }) = token {
        inner.as_ref() == v
    } else {
        false
    }
}

/// This function returns under 3 conditions
/// 1. Reached end of file
/// 2. Reached an operator with a binding power < min_bp
/// 3. Reached a ";" or ","
fn expr_bp<I: Iterator<Item = Spanned<Arc<str>>>>(
    iter: &mut Peekable<I>,
    min_bp: u8,
    errors: &mut Vec<Error>,
) -> Expr {
    let mut lhs = match iter.next() {
        None => {
            // todo: get the correct span
            errors.push(Error::new(ErrorType::UnexpectedEndOfFile, (0, 0)));
            return Expr::Nil((0, 0));
        }
        Some(Spanned { inner, span }) => match inner.as_ref() {
            "(" => {
                let lhs = expr_bp(iter, 0, errors);

                if !matches(iter.next().as_ref(), ")") {
                    errors.push(Error::new(ErrorType::UnclosedParenthesis, span))
                }

                lhs
            }
            "{" => {
                let expr = code(iter, errors);

                let last = iter.next();
                if !matches(last.as_ref(), "}") {
                    errors.push(Error::new(ErrorType::UnclosedParenthesis, span))
                }
                let start = span.0;
                let end = last.map(|x| x.span.1).unwrap_or(start);

                Expr::Code(Spanned {
                    inner: expr,
                    span: (start, end),
                })
            }
            "[" => {
                let expr = array(iter, errors);

                let last = iter.next();
                if !matches(last.as_ref(), "]") {
                    errors.push(Error::new(ErrorType::UnclosedParenthesis, span))
                }
                let start = span.0;
                let end = last.map(|x| x.span.1).unwrap_or(start);

                Expr::Array(Spanned {
                    inner: expr,
                    span: (start, end),
                })
            }
            op => {
                if let Some(((), r_bp)) = prefix_binding_power(op) {
                    let rhs = expr_bp(iter, r_bp, errors);
                    Expr::Unary(Spanned { inner, span }, Box::new(rhs))
                } else {
                    atom_to_expr(Spanned { inner, span })
                }
            }
        },
    };

    loop {
        let op = match iter.peek() {
            None => break,
            Some(op) => {
                if matches!(op.inner.as_ref(), ";" | ",") {
                    break;
                }
                op.clone()
            }
        };

        if let Some((l_bp, r_bp)) = infix_binding_power(&op.inner) {
            if l_bp < min_bp {
                break;
            }
            iter.next();

            let rhs = expr_bp(iter, r_bp, errors);
            lhs = Expr::Binary(Box::new(lhs), op, Box::new(rhs));
            continue;
        } else if matches!(op.inner.as_ref(), "}" | "]" | ")") {
        } else {
            let error = match (op.inner.as_ref(), &lhs) {
                ("(", Expr::Variable(v)) => Error::new(
                    ErrorType::Other(format!("Macro \"{}\" undefined", v.inner.as_ref()).into()),
                    v.span,
                ),
                _ => Error::new(
                    ErrorType::Other(
                        format!("\"{}\" is not a valid binary operator", op.inner).into(),
                    ),
                    op.span,
                ),
            };
            errors.push(error);
            iter.next();
            return lhs;
        }

        break;
    }
    lhs
}

fn prefix_binding_power(op: &str) -> Option<((), u8)> {
    if UNARY.contains(&UncasedStr::new(op)) {
        // https://community.bistudio.com/wiki/Operators#Order_of_Precedence
        Some(((), 50))
    } else {
        None
    }
}

fn infix_binding_power(op: &str) -> Option<(u8, u8)> {
    if UncasedStr::new(op) == UncasedStr::new("or") {
        return Some((21, 22));
    } else if UncasedStr::new(op) == UncasedStr::new("and") {
        return Some((23, 24));
    } else if UncasedStr::new(op) == UncasedStr::new("else") {
        return Some((29, 30));
    } else if UncasedStr::new(op) == UncasedStr::new("max")
        || UncasedStr::new(op) == UncasedStr::new("min")
    {
        return Some((31, 32));
    } else if UncasedStr::new(op) == UncasedStr::new("mod")
        || UncasedStr::new(op) == UncasedStr::new("atan2")
    {
        return Some((33, 34));
    }

    // https://community.bistudio.com/wiki/Operators#Order_of_Precedence
    let res = match op {
        ";" | "," => (1, 2),
        "=" => (19, 20), // assign op has the least binding power
        "or" | "||" => (21, 22),
        "and" | "&&" => (23, 24),
        "==" | "!=" | ">" | "<" | ">=" | "<=" | ">>" => (25, 26),
        // binary op below => (27, 28)
        "else" => (29, 30),
        "+" | "-" | "max" | "min" => (31, 32),
        "*" | "/" | "%" | "mod" | "atan2" => (33, 34),
        "^" => (34, 35),
        "#" => (36, 37),
        _ => {
            if !BINARY.contains(&UncasedStr::new(op)) {
                return None;
            }
            (27, 28)
        }
    };
    Some(res)
}
