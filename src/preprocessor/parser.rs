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

    let mut else_keyword = None;
    let maybe_else = if_.next();
    let (endif_keyword, else_) =
        if matches!(maybe_else.as_ref().map(|x| x.as_rule()), Some(Rule::else_)) {
            else_keyword = maybe_else;
            let else_ = if_
                .next()
                .map(|else_| else_.into_inner())
                .and_then(|else_| (else_.len() != 0).then_some(else_));
            let endif_keyword = if_.next().unwrap();
            (endif_keyword, else_)
        } else {
            (maybe_else.unwrap(), None)
        };

    let then = if_body.map(_parse).unwrap_or_default();
    let else_ = else_.map(_parse).unwrap_or_default();

    let if_type = if_start.next().unwrap();
    match if_type.as_str() {
        "#ifndef" => {
            let name = if_start.next().unwrap();
            Ast::Ifndef(IfDefined {
                keyword: if_type.into(),
                term: name.into(),
                then,
                else_keyword: else_keyword.map(|x| x.into()),
                else_,
                endif_keyword: endif_keyword.into(),
            })
        }
        "#ifdef" => {
            let name = if_start.next().unwrap();
            Ast::Ifdef(IfDefined {
                keyword: if_type.into(),
                term: name.into(),
                then,
                else_keyword: else_keyword.map(|x| x.into()),
                else_,
                endif_keyword: endif_keyword.into(),
            })
        }
        "#if" => {
            let expr = _parse(if_start);
            Ast::If(If {
                keyword: if_type.into(),
                expr,
                then,
                else_keyword: else_keyword.map(|x| x.into()),
                else_,
                endif_keyword: endif_keyword.into(),
            })
        }
        _ => unreachable!(),
    }
}

fn parse_pair(pair: Pair<'_, Rule>) -> Ast<'_> {
    match pair.as_rule() {
        Rule::if_ => parse_if(pair),
        Rule::define => {
            let mut define = pair.into_inner();
            let keyword = define.next().unwrap().into();
            let name = define.next().unwrap().into();

            let Some(args_or_body) = define.peek() else {
                return Ast::Define(Define {
                    keyword,
                    name,
                    arguments: None,
                    body: Default::default(),
                });
            };
            let is_arguments = matches!(args_or_body.as_rule(), Rule::define_arguments);

            let (arguments, body) = if is_arguments {
                let args = Some(
                    define
                        .next()
                        .unwrap()
                        .into_inner()
                        .map(|x| x.into())
                        .collect(),
                );

                let body = define
                    .filter(|x| x.as_rule() != Rule::EOI)
                    .filter(|x| x.as_rule() != Rule::COMMENT)
                    .map(|x| x.into())
                    .collect();

                (args, body)
            } else {
                (
                    Default::default(),
                    define
                        .filter(|x| x.as_rule() != Rule::EOI)
                        .filter(|x| x.as_rule() != Rule::COMMENT)
                        .map(|x| x.into())
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

pub fn parse(pairs: Pairs<'_, Rule>) -> VecDeque<Ast<'_>> {
    _parse(pairs)
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

        Error::new(message, span)
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
