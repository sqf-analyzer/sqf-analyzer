use std::collections::HashSet;
use std::marker::PhantomData;

use crate::database::SIGNATURES;
use crate::pratt_parser::{Affix, AsStr, Assoc, PrattParser, Precedence, ToSpan};
pub use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

use crate::error::Error;
use crate::types::*;

#[derive(Parser)]
#[grammar = "sqf.pest"]
pub struct SQFParser;

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

struct Pre<'i> {
    phantom: PhantomData<&'i u32>,
}

fn common_weight(token: &str) -> Option<(Affix, u32)> {
    // https://foxhound.international/precedence-arma-3-sqf.html
    match token {
        "or" | "||" => Some((Affix::Infix(Assoc::Left), 1)),
        "and" | "&&" => Some((Affix::Infix(Assoc::Left), 2)),
        "==" | "!=" | ">" | "<" | ">=" | "<=" | ">>" => Some((Affix::Infix(Assoc::Left), 3)),
        "else" => Some((Affix::Infix(Assoc::Left), 5)),
        "+" | "-" | "max" | "min" => Some((Affix::Infix(Assoc::Left), 6)),
        "*" | "/" | "%" | "mod" | "atan2" => Some((Affix::Infix(Assoc::Left), 7)),
        "^" => Some((Affix::Infix(Assoc::Left), 8)),
        _ => None,
    }
}

impl AsStr for Pair<'_, Rule> {
    fn as_str(&self) -> &str {
        self.as_str()
    }
}

impl ToSpan for Pair<'_, Rule> {
    fn to_span(&self) -> Span {
        (self.as_span().start(), self.as_span().end())
    }
}

impl<'i> Precedence for Pre<'i> {
    type Item = Pair<'i, Rule>;

    fn inflix_weight(item: &Self::Item) -> Option<(Affix, u32)> {
        let token = item.as_str().to_lowercase(); // SQF is case insensitive
        let token = token.as_ref();
        common_weight(token).or_else(|| {
            BINARY
                .contains(token)
                .then_some((Affix::Infix(Assoc::Left), 4))
        })
    }

    fn prefix_weight(item: &Self::Item) -> Option<(Affix, u32)> {
        let token = item.as_str().to_lowercase(); // SQF is case insensitive
        let token = token.as_ref();
        UNARY
            .contains(token)
            .then_some((Affix::Prefix, 9))
            .or_else(|| common_weight(token))
    }
}

fn to_span(pair: &Pair<'_, Rule>) -> Span {
    (pair.as_span().start(), pair.as_span().end())
}

impl From<Pair<'_, Rule>> for Spanned<String> {
    fn from(value: Pair<'_, Rule>) -> Self {
        Self {
            span: value.to_span(),
            inner: value.as_str().to_string(),
        }
    }
}

fn parse_assignment(pairs: Pairs<Rule>, errors: &mut Vec<Error>) -> Expr {
    let mut a = pairs.collect::<Vec<_>>();
    let expr = a.pop().expect("by pest definition");
    let variable = a.pop().expect("by pest definition");
    let is_private = !a.is_empty();
    Expr::Assignment {
        is_private,
        variable: variable.into(),
        expr: Box::new(parse_expr(expr, errors)),
    }
}

fn parse_macro(pairs: Pairs<Rule>) -> Expr {
    let mut a = pairs.collect::<Vec<_>>();
    let content = a.pop().expect("by pest definition");
    let name = a.pop().expect("by pest definition");
    Expr::Macro(name.into(), content.into())
}

fn no_quotes(v: &str) -> &str {
    &v[1..v.len() - 1]
}

pub fn quoted_span(pair: Pair<'_, Rule>) -> Spanned<String> {
    Spanned {
        inner: no_quotes(pair.as_str()).to_string(),
        span: to_span(&pair),
    }
}

fn not_comment(pair: &Pair<'_, Rule>) -> bool {
    !matches!(pair.as_rule(), Rule::COMMENT)
}

pub fn parse_expr(pair: Pair<Rule>, errors: &mut Vec<Error>) -> Spanned<Expr> {
    let mut a = PrattParser::new()
        .map_primary::<Pre, _, _>(|primary: _| match primary.as_rule() {
            Rule::number => Spanned {
                inner: Expr::Value(Value::Number(primary.as_str().to_string())),
                span: to_span(&primary),
            },
            Rule::string => quoted_span(primary).map(Value::String).map(Expr::Value),
            Rule::boolean => Spanned {
                inner: Expr::Value(Value::Boolean(primary.as_str().to_string())),
                span: to_span(&primary),
            },
            Rule::array => {
                let span = to_span(&primary);
                let inner = primary
                    .into_inner()
                    .filter(not_comment)
                    .map(|pair| parse_expr(pair, errors))
                    .collect::<Vec<_>>();
                Spanned {
                    inner: Expr::Value(Value::Array(inner)),
                    span,
                }
            }
            Rule::expr => parse_expr(primary, errors),
            Rule::variable => Spanned {
                span: to_span(&primary),
                inner: Expr::Variable(primary.into()),
            },
            Rule::assignment => Spanned {
                span: to_span(&primary),
                inner: parse_assignment(primary.into_inner(), errors),
            },
            Rule::code => {
                let span = to_span(&primary);
                let inner = primary
                    .into_inner()
                    .filter(not_comment)
                    .map(|pair| parse_expr(pair, errors))
                    .collect::<Vec<_>>();
                Spanned {
                    inner: Expr::Value(Value::Code(inner)),
                    span,
                }
            }
            Rule::macro_ => Spanned {
                span: to_span(&primary),
                inner: parse_macro(primary.into_inner()),
            },
            rule => unreachable!("Expr::parse expected atom, found {:?}", rule),
        })
        .map_prefix(|op, rhs| {
            let span = (to_span(&op).0, rhs.span.1);

            Spanned {
                inner: Expr::UnaryOp {
                    op: op.into(),
                    rhs: Box::new(rhs),
                },
                span,
            }
        })
        .map_infix(|lhs, op, rhs| {
            let span = (lhs.span.0, rhs.span.1);
            Spanned {
                inner: Expr::BinaryOp {
                    lhs: Box::new(lhs),
                    op: op.into(),
                    rhs: Box::new(rhs),
                },
                span,
            }
        });
    let span = pair.to_span();
    let result = a.parse(pair.into_inner());
    match result {
        Ok(r) => r,
        Err(e) => {
            errors.push(e);
            Spanned {
                span,
                inner: Expr::Error,
            }
        }
    }
}

pub fn tokens(data: &str) -> Result<Pairs<'_, Rule>, Error> {
    Ok(SQFParser::parse(Rule::program, data)?)
}

fn to_span_expr(pair: Pair<Rule>, errors: &mut Vec<Error>) -> Spanned<Expr> {
    match pair.as_rule() {
        Rule::include => {
            let mut pairs = pair.into_inner();
            let _ = pairs.next();
            let pair = pairs.next().expect("by pest definition");

            Spanned {
                span: to_span(&pair),
                inner: Expr::Include(quoted_span(pair)),
            }
        }
        Rule::define => {
            let mut pairs = pair.into_inner();
            let _ = pairs.next().expect("by pest definition");
            let name = pairs.next().expect("by pest definition");

            let mut arguments = vec![];
            let mut body = None;

            for pair in pairs {
                match pair.as_rule() {
                    Rule::define_argument => arguments.push(pair.as_str().to_string()),
                    Rule::define_body => body = Some(pair.as_str().to_string()),
                    _ => unreachable!(),
                }
            }

            Spanned {
                span: to_span(&name),
                inner: Expr::Define(Define {
                    name: name.as_str().to_string(),
                    arguments,
                    body,
                }),
            }
        }
        _ => parse_expr(pair, errors),
    }
}

pub fn parse<'a, I: Iterator<Item = Pair<'a, Rule>>>(iter: I) -> (Vec<Spanned<Expr>>, Vec<Error>) {
    let mut errors = vec![];
    let expr = iter
        .filter(|pair| !pair.as_str().is_empty())
        .filter(not_comment)
        .map(|pair| to_span_expr(pair, &mut errors))
        .collect();
    (expr, errors)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_positive() {
        let cases = ["\"a\"", "\"\"a\"\"", "\"\"ba\"aa\"\""];

        for case in cases {
            assert!(SQFParser::parse(Rule::string, case).is_ok());
        }
    }

    #[test]
    fn variable() {
        let cases = ["_a", "_args", "aa", "_arguments"];

        for case in cases {
            let r = SQFParser::parse(Rule::variable, case);
            if let Err(r) = r {
                println!("{r:?}");
                panic!();
            }
        }
    }

    #[test]
    fn array() {
        let cases = ["[1, 2]"];

        for case in cases {
            let r = SQFParser::parse(Rule::array, case);
            if let Err(r) = r {
                println!("{r:?}");
                panic!();
            }
        }
    }
}
