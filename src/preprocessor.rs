use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

use crate::error::Error;
use crate::pratt_parser::ToSpan;
use crate::types::{Span, Spanned};

impl<'a> From<Pair<'a, Rule>> for Spanned<&'a str> {
    fn from(value: Pair<'a, Rule>) -> Self {
        Self {
            span: value.to_span(),
            inner: value.as_str(),
        }
    }
}

impl ToSpan for Pair<'_, Rule> {
    fn to_span(&self) -> Span {
        (self.as_span().start(), self.as_span().end())
    }
}

#[derive(Parser)]
#[grammar = "preprocessor.pest"]
struct PreprocessorParser;

pub fn pairs(data: &str) -> Result<Pairs<'_, Rule>, Error> {
    Ok(PreprocessorParser::parse(Rule::program, data)?)
}

#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Ifdef(Pair<'a, Rule>, Vec<Ast<'a>>, Vec<Ast<'a>>),
    Ifndef(Pair<'a, Rule>, Vec<Ast<'a>>, Vec<Ast<'a>>),
    If(Pairs<'a, Rule>, Vec<Ast<'a>>, Vec<Ast<'a>>),
    Define(Pair<'a, Rule>, Vec<Spanned<&'a str>>, Vec<Spanned<&'a str>>),
    Undefine(Spanned<&'a str>),
    Include(Spanned<&'a str>),
    Body(Vec<Ast<'a>>),
    Comment(Spanned<&'a str>),
    Term(Spanned<&'a str>),
}

fn parse_if(pair: Pair<'_, Rule>) -> Ast<'_> {
    let mut if_ = pair.into_inner();
    let mut if_start = if_.next().unwrap().into_inner();
    let if_body = if_.next().unwrap().into_inner();
    let if_body = (if_body.len() != 0).then_some(if_body);
    let else_ = if if_.len() > 0 {
        let _ = if_.next(); // #else
        if_.next()
            .map(|else_| else_.into_inner())
            .and_then(|else_| (else_.len() != 0).then_some(else_))
    } else {
        None
    };

    let body = if_body.map(_parse).unwrap_or_default();
    let else_ = else_.map(_parse).unwrap_or_default();

    let if_type = if_start.next().unwrap();
    match if_type.as_str() {
        "#ifndef" => {
            let name = if_start.next().unwrap();
            Ast::Ifndef(name, body, else_)
        }
        "#ifdef" => {
            let name = if_start.next().unwrap();
            Ast::Ifdef(name, body, else_)
        }
        _ => {
            assert!(matches!(if_type.as_rule(), Rule::if_expr));
            let expr = if_type.into_inner();
            Ast::If(expr, body, else_)
        }
    }
}

fn parse_pair(pair: Pair<'_, Rule>) -> Ast<'_> {
    match pair.as_rule() {
        Rule::if_ => parse_if(pair),
        Rule::define => {
            let mut define = pair.into_inner();
            let name = define.next().unwrap();

            let Some(args_or_body) = define.next() else {
                return Ast::Define(name, vec![], vec![]);
            };
            let is_arguments = matches!(args_or_body.as_rule(), Rule::define_arguments);

            let (args, body) = if is_arguments {
                let body = define.map(|x| x.into()).collect::<Vec<_>>();

                let args = args_or_body.into_inner().map(|x| x.into()).collect();

                (args, body)
            } else {
                (
                    vec![],
                    std::iter::once(args_or_body.into())
                        .chain(define.map(|x| x.into()))
                        .collect(),
                )
            };

            Ast::Define(name, args, body)
        }
        Rule::undef => {
            let word = pair.into_inner().next().unwrap();
            Ast::Undefine(word.into())
        }
        Rule::include => {
            let word = pair.into_inner().next().unwrap();
            Ast::Include(word.into())
        }
        Rule::COMMENT => Ast::Comment(pair.into()),
        _ => Ast::Term(pair.into()),
    }
}

fn _parse(pairs: Pairs<'_, Rule>) -> Vec<Ast<'_>> {
    pairs
        .filter(|pair| pair.as_rule() != Rule::EOI)
        .map(parse_pair)
        .collect()
}

pub fn parse(pairs: Pairs<'_, Rule>) -> Ast<'_> {
    Ast::Body(_parse(pairs))
}
