use std::collections::VecDeque;

use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

use super::ast::*;
use crate::error::Error;
use crate::span::Spanned;

#[derive(Parser)]
#[grammar = "preprocessor/preprocessor.pest"]
struct PreprocessorParser;

pub fn pairs(data: &str) -> Result<Pairs<'_, Rule>, Error> {
    Ok(PreprocessorParser::parse(Rule::program, data)?)
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
            Ast::Ifndef(if_type.into(), name.into(), body, else_)
        }
        "#ifdef" => {
            let name = if_start.next().unwrap();
            Ast::Ifdef(if_type.into(), name.into(), body, else_)
        }
        _ => {
            assert!(matches!(if_type.as_rule(), Rule::if_expr));
            let expr = if_type.into_inner().map(|x| x.into()).collect();
            Ast::If(expr, body, else_)
        }
    }
}

fn parse_pair(pair: Pair<'_, Rule>) -> Ast<'_> {
    match pair.as_rule() {
        Rule::if_ => parse_if(pair),
        Rule::define => {
            let mut define = pair.into_inner();
            let keyword = define.next().unwrap().into();
            let name = define.next().unwrap().into();

            let Some(args_or_body) = define.next() else {
                return Ast::Define(Define {
                    keyword,
                    name, arguments: None, body: Default::default()});
            };
            let is_arguments = matches!(args_or_body.as_rule(), Rule::define_arguments);

            let (arguments, body) = if is_arguments {
                let body = define
                    .filter(|x| x.as_rule() != Rule::EOI)
                    .map(|x| x.into())
                    .collect();

                let args = Some(args_or_body.into_inner().map(|x| x.into()).collect());

                (args, body)
            } else {
                (
                    Default::default(),
                    std::iter::once(args_or_body.into())
                        .chain(
                            define
                                .filter(|x| x.as_rule() != Rule::EOI)
                                .map(|x| x.into()),
                        )
                        .collect(),
                )
            };

            Ast::Define(Define {
                keyword,
                name,
                arguments,
                body,
            })
        }
        Rule::undef => {
            let mut tokens = pair.into_inner();
            let keyword = tokens.next().unwrap();
            let word = tokens.next().unwrap();
            Ast::Undefine(keyword.into(), word.into())
        }
        Rule::include => {
            let mut tokens = pair.into_inner();
            let keyword = tokens.next().unwrap();
            let word = tokens.next().unwrap();
            Ast::Include(keyword.into(), word.into())
        }
        Rule::COMMENT => Ast::Comment(pair.into()),
        _ => Ast::Term(pair.into()),
    }
}

fn _parse(pairs: Pairs<'_, Rule>) -> VecDeque<Ast<'_>> {
    pairs
        .filter(|pair| pair.as_rule() != Rule::EOI)
        .map(parse_pair)
        .collect()
}

pub fn parse(pairs: Pairs<'_, Rule>) -> Ast<'_> {
    Ast::Body(_parse(pairs))
}

impl From<pest::error::Error<Rule>> for Error {
    fn from(value: pest::error::Error<Rule>) -> Self {
        let span = match value.location {
            pest::error::InputLocation::Pos(a) => (a, a),
            pest::error::InputLocation::Span(span) => span,
        };
        let message = match value.variant {
            pest::error::ErrorVariant::ParsingError { positives, .. } => {
                let positives = positives
                    .into_iter()
                    .filter(|rule| !matches!(rule, Rule::EOI))
                    .collect::<Vec<_>>();
                format!("expected {positives:?}")
            }
            pest::error::ErrorVariant::CustomError { message } => message,
        };

        Spanned {
            span,
            inner: message,
        }
    }
}

impl<'a> From<Pair<'a, Rule>> for Spanned<&'a str> {
    fn from(value: Pair<'a, Rule>) -> Self {
        Self {
            span: (value.as_span().start(), value.as_span().end()),
            inner: value.as_str(),
        }
    }
}

impl<'a> From<Pair<'a, Rule>> for Spanned<String> {
    fn from(value: Pair<'a, Rule>) -> Self {
        Self {
            span: (value.as_span().start(), value.as_span().end()),
            inner: value.as_str().to_string(),
        }
    }
}
