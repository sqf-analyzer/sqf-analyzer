use std::collections::{HashMap, VecDeque};

use pest::iterators::{Pair, Pairs};
use pest::Parser;
use pest_derive::Parser;

use crate::error::Error;
use crate::span::{Span, Spanned};

pub trait ToSpan {
    fn to_span(&self) -> Span;
}

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
pub struct Define<'a> {
    pub name: Spanned<&'a str>,
    pub arguments: Vec<Spanned<&'a str>>,
    pub body: Vec<Spanned<&'a str>>,
}

#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Ifdef(Spanned<&'a str>, VecDeque<Ast<'a>>, VecDeque<Ast<'a>>),
    Ifndef(Spanned<&'a str>, VecDeque<Ast<'a>>, VecDeque<Ast<'a>>),
    If(
        VecDeque<Spanned<&'a str>>,
        VecDeque<Ast<'a>>,
        VecDeque<Ast<'a>>,
    ),
    Define(Define<'a>),
    Undefine(Spanned<&'a str>),
    Include(Spanned<&'a str>),
    Body(VecDeque<Ast<'a>>),
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
            Ast::Ifndef(name.into(), body, else_)
        }
        "#ifdef" => {
            let name = if_start.next().unwrap();
            Ast::Ifdef(name.into(), body, else_)
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
            let name = define.next().unwrap().into();

            let Some(args_or_body) = define.next() else {
                return Ast::Define(Define {
                    name, arguments: vec![], body: vec![]});
            };
            let is_arguments = matches!(args_or_body.as_rule(), Rule::define_arguments);

            let (arguments, body) = if is_arguments {
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

            Ast::Define(Define {
                name,
                arguments,
                body,
            })
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

fn _parse(pairs: Pairs<'_, Rule>) -> VecDeque<Ast<'_>> {
    pairs
        .filter(|pair| pair.as_rule() != Rule::EOI)
        .map(parse_pair)
        .collect()
}

pub fn parse(pairs: Pairs<'_, Rule>) -> Ast<'_> {
    Ast::Body(_parse(pairs))
}

pub type Defines<'a> = HashMap<&'a str, Define<'a>>;

#[derive(Clone)]
pub struct AstIterator<'a> {
    base: VecDeque<Ast<'a>>,
    defines: Defines<'a>,
}

fn evaluate_terms<'a>(
    terms: &mut VecDeque<Ast<'a>>,
    defines: &mut Defines<'a>,
) -> (bool, Option<Spanned<&'a str>>) {
    // go to front, take (has_more, item); if not has_more; remove it
    match terms.front_mut() {
        Some(item) => {
            let (has_more, item) = take_last(item, defines);
            if has_more {
                (true, item)
            } else {
                terms.pop_front();
                (true, item)
            }
        }
        None => (false, None),
    }
}

fn evaluate_if(expr: &VecDeque<Spanned<&str>>, defines: &Defines) -> bool {
    match expr.len() {
        1 => {
            let def_name = expr.front().unwrap().inner;
            defines
                .get(def_name)
                .and_then(|_| todo!("compute bool from variable"))
                .unwrap_or_default()
        }
        3 => {
            todo!("compute op from variable")
        }
        _ => unreachable!(),
    }
}

fn take_last<'a>(ast: &mut Ast<'a>, defines: &mut Defines<'a>) -> (bool, Option<Spanned<&'a str>>) {
    match ast {
        Ast::Ifndef(term, else_, then) | Ast::Ifdef(term, then, else_) => {
            if defines.contains_key(term.inner) {
                evaluate_terms(then, defines)
            } else {
                evaluate_terms(else_, defines)
            }
        }
        Ast::If(expr, then, else_) => {
            if evaluate_if(expr, defines) {
                evaluate_terms(then, defines)
            } else {
                evaluate_terms(else_, defines)
            }
        }
        Ast::Define(define) => {
            defines.insert(define.name.inner, define.clone());
            (false, None)
        }
        Ast::Undefine(name) => {
            defines.remove(name.inner);
            (false, None)
        }
        Ast::Include(_) => (false, None), // todo => include
        Ast::Body(terms) => evaluate_terms(terms, defines),
        Ast::Term(term) => (false, Some(*term)),
        Ast::Comment(_) => (false, None),
    }
}

impl<'a> AstIterator<'a> {
    pub fn new(base: Ast<'a>, defines: Defines<'a>) -> Self {
        let Ast::Body(base) = base else {
            panic!()
        };
        Self { base, defines }
    }
}

impl<'a> Iterator for AstIterator<'a> {
    type Item = Spanned<&'a str>;

    fn next(&mut self) -> Option<Self::Item> {
        // None => continue
        // Some(None) => empty
        // Some(Some(_)) => value
        match evaluate_terms(&mut self.base, &mut self.defines) {
            (true, None) => self.next(),
            (true, item) => item,
            (false, None) => None,
            (false, item) => item,
        }
    }
}

pub fn tokens<'a>(data: &'a str, defines: Defines<'a>) -> Result<AstIterator<'a>, Error> {
    let pairs = pairs(data)?;
    let ast = parse(pairs);
    Ok(AstIterator::new(ast, defines))
}
