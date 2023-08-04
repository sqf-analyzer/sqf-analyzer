use sqf::{
    database::SIGNATURES,
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
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Token<'a> {
    Atom(Spanned<&'a str>),
    Op(Spanned<&'a str>),
    Eof,
}

// https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html#Introduction
fn expr(iter: AstIterator) -> S {
    let mut iter = iter
        .map(|x| match x.inner {
            "*" | "+" | "-" | "}" | ")" | "]" | "{" | "(" | "[" | "==" | "=<" | "=>" | "!="
            | "<" | ">" | "if" | "then" | "else" | ";" => Token::Op(x),
            _ => Token::Atom(x),
        })
        .chain(std::iter::once(Token::Eof))
        .peekable();
    expr_bp(&mut iter, 0)
}

fn expr_bp<'a, I: Iterator<Item = Token<'a>>>(lexer: &mut Peekable<I>, min_bp: u8) -> S<'a> {
    let mut lhs = match lexer.next().unwrap() {
        Token::Atom(it) => S::Atom(it),
        Token::Op(Spanned { inner: "(", .. }) => {
            let lhs = expr_bp(lexer, 0);
            matches!(lexer.next().unwrap(), Token::Op(Spanned { inner: ")", .. }));
            lhs
        }
        Token::Op(Spanned { inner: "{", .. }) => {
            let lhs = expr_bp(lexer, 0);
            matches!(lexer.next().unwrap(), Token::Op(Spanned { inner: "}", .. }));
            lhs
        }
        Token::Op(Spanned { inner: "[", .. }) => {
            let lhs = expr_bp(lexer, 0);
            matches!(lexer.next().unwrap(), Token::Op(Spanned { inner: "]", .. }));
            lhs
        }
        Token::Op(op) => {
            let ((), r_bp) = prefix_binding_power(op.inner);
            let rhs = expr_bp(lexer, r_bp);
            S::Cons(op, vec![rhs])
        }
        t => panic!("bad token: {:?}", t),
    };

    loop {
        let op = match lexer.peek().unwrap() {
            Token::Eof => break,
            Token::Op(op) => *op,
            t => panic!("bad token: {:?}", t),
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
            lexer.next().unwrap();

            lhs = {
                let rhs = expr_bp(lexer, r_bp);
                S::Cons(op, vec![lhs, rhs])
            };
            continue;
        }

        break;
    }
    lhs
}

fn prefix_binding_power(op: &str) -> ((), u8) {
    match op {
        "+" | "-" => ((), 9),
        "if" => ((), 4),
        _ => panic!("bad op: {:?}", op),
    }
}

fn postfix_binding_power(op: &str) -> Option<(u8, ())> {
    let res = match op {
        ";" => (1, ()),
        "{" => (11, ()),
        _ => return None,
    };
    Some(res)
}

fn infix_binding_power(op: &str) -> Option<(u8, u8)> {
    let res = match op {
        "=" => (2, 1),
        "+" | "-" => (5, 6),
        "*" | "/" => (7, 8),
        "then" | "else" => (3, 4),
        _ => return None,
    };
    Some(res)
}

/*
#[test]
fn tests() {
    let s = expr("1");
    assert_eq!(s.to_string(), "1");

    let s = expr("1 + 2 * 3");
    assert_eq!(s.to_string(), "(+ 1 (* 2 3))");

    let s = expr("a + b * c * d + e");
    assert_eq!(s.to_string(), "(+ (+ a (* (* b c) d)) e)");

    let s = expr("f . g . h");
    assert_eq!(s.to_string(), "(. f (. g h))");

    let s = expr("1 + 2 + f . g . h * 3 * 4");
    assert_eq!(s.to_string(), "(+ (+ 1 2) (* (* (. f (. g h)) 3) 4))");

    let s = expr("--1 * 2");
    assert_eq!(s.to_string(), "(* (- (- 1)) 2)");

    let s = expr("--f . g");
    assert_eq!(s.to_string(), "(- (- (. f g)))");

    let s = expr("-9!");
    assert_eq!(s.to_string(), "(- (! 9))");

    let s = expr("f . g !");
    assert_eq!(s.to_string(), "(! (. f g))");

    let s = expr("(((0)))");
    assert_eq!(s.to_string(), "0");

    let s = expr("x[0][1]");
    assert_eq!(s.to_string(), "([ ([ x 0) 1)");

    let s = expr(
        "a ? b :
         c ? d
         : e",
    );
    assert_eq!(s.to_string(), "(? a b (? c d e))");

    let s = expr("a = 0 ? b : c = d");
    assert_eq!(s.to_string(), "(= a (= (? 0 b c) d))")
}
 */

#[test]
fn tokens() {
    use std::fs;
    let path = "tests/integration/examples/basic_if.sqf";
    let case = fs::read_to_string(path).unwrap();

    let a = sqf::preprocessor::parse(sqf::preprocessor::pairs(&case).unwrap());
    let iter = sqf::preprocessor::AstIterator::new(a, Default::default());

    println!("{:?}", iter.clone().collect::<Vec<_>>());
    println!("{:#?}", expr(iter));
    // if (else (then (if a) b) (; c))
    panic!();
}
