use sqf::{
    database::SIGNATURES,
    error::Error,
    preprocessor::AstIterator,
    types::{Signature, Spanned},
};

use std::{collections::HashSet, fmt, iter::Peekable};

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
enum S<'a> {
    Atom(Spanned<&'a str>),
    Cons(Spanned<&'a str>, Vec<S<'a>>),
    Code(Vec<S<'a>>),
    Array(Vec<S<'a>>),
    Nil,
}

impl<'a> fmt::Debug for S<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            S::Atom(i) => write!(f, "{}", i.inner),
            S::Cons(head, rest) => {
                write!(f, "({}", head.inner)?;
                for s in rest {
                    write!(f, " {:?}", s)?
                }
                write!(f, ")")
            }
            S::Code(rest) => {
                write!(f, "{{")?;
                for s in rest {
                    write!(f, "{:?};", s)?
                }
                write!(f, "}}")
            }
            S::Array(rest) => {
                write!(f, "[")?;
                for s in rest {
                    write!(f, "{:?},", s)?
                }
                write!(f, "]")
            }
            S::Nil => {
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
) -> Vec<S<'a>> {
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
) -> Vec<S<'a>> {
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
fn expr(iter: AstIterator) -> (Vec<S<'_>>, Vec<Error>) {
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
) -> S<'a> {
    let mut lhs = match lexer.next().unwrap_or(Token::Eof) {
        Token::Atom(it) => S::Atom(it),
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
        Token::Op(Spanned { inner: ";", .. }) => S::Code(vec![]),
        Token::Op(Spanned { inner: "{", span }) => {
            let expr = code(lexer, errors);

            if !matches(lexer.next().as_ref(), "}") {
                errors.push(Error {
                    inner: "\"{\" is not closed".to_string(),
                    span,
                })
            }

            S::Code(expr)
        }
        Token::Op(Spanned { inner: "[", span }) => {
            let expr = array(lexer, errors);

            if !matches(lexer.next().as_ref(), "]") {
                errors.push(Error {
                    inner: "\"[\" is not closed".to_string(),
                    span,
                })
            }

            S::Array(expr)
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
            S::Cons(op, vec![rhs])
        }
        Token::Eof => {
            errors.push(Error {
                inner: "Un-expected end of file".to_string(),
                span: (0, 0),
            });
            return S::Nil;
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
                return S::Cons(*primary, vec![lhs]);
            }
        };

        if let Some((l_bp, ())) = postfix_binding_power(op.inner) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();

            lhs = S::Cons(op, vec![lhs]);
            continue;
        }

        if let Some((l_bp, r_bp)) = infix_binding_power(op.inner) {
            if l_bp < min_bp {
                break;
            }
            lexer.next();

            let rhs = expr_bp(lexer, r_bp, errors);
            lhs = S::Cons(op, vec![lhs, rhs]);
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

#[test]
fn tokens() {
    use std::fs;
    let path = "tests/integration/examples/basic_parenthesis.sqf";
    let case = fs::read_to_string(path).unwrap();

    let a = sqf::preprocessor::parse(sqf::preprocessor::pairs(&case).unwrap());
    let iter = sqf::preprocessor::AstIterator::new(a, Default::default());

    let (result, errors) = expr(iter);
    assert!(errors.is_empty());
    println!("{:#?}", result);
    let expected = r#"[
    1,
    {},
    (+ 1 1),
    {},
    (then (if a) (else {1;2;} {1;2;})),
    (then (if a) (else {1;} {2;})),
    (= (private a) 1),
    (= a []),
    (= a [1,2,]),
    (* (+ a 1) 2),
]"#;
    assert_eq!(format!("{:#?}", result), expected);
}

#[test]
fn errors() {
    let case = vec![
        (
            "[",
            vec![Spanned {
                inner: "\"[\" is not closed".to_string(),
                span: (0, 1),
            }],
        ),
        (
            "{",
            vec![Spanned {
                inner: "\"{\" is not closed".to_string(),
                span: (0, 1),
            }],
        ),
        (
            "(",
            vec![
                Spanned {
                    inner: "Un-expected end of file".to_string(),
                    span: (0, 0),
                },
                Spanned {
                    inner: "\"(\" is not closed".to_string(),
                    span: (0, 1),
                },
            ],
        ),
    ];

    for (case, expected) in case {
        let a = sqf::preprocessor::parse(sqf::preprocessor::pairs(case).unwrap());
        let iter = sqf::preprocessor::AstIterator::new(a, Default::default());

        let (_, errors) = expr(iter);
        assert_eq!(errors, expected);
    }
}
