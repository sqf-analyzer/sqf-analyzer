use std::{
    collections::{HashMap, VecDeque},
    sync::Arc,
};

use crate::span::Spanned;

/// # Note
/// This struct cannot have an associated lifetime because it can be shared between files,
/// that have been opened with diferent lifetimes
#[derive(Debug, Clone, PartialEq)]
pub struct Define {
    pub keyword: Spanned<Arc<str>>,
    pub name: Spanned<Arc<str>>,
    // None => no arguments
    // Some => zero or more arguments
    pub arguments: Option<Vec<Spanned<Arc<str>>>>,
    pub body: VecDeque<Spanned<Arc<str>>>,
}

pub type Defines = HashMap<Arc<str>, Define>;

#[derive(Debug, Clone, PartialEq)]
pub struct IfDefined<'a> {
    pub keyword: Spanned<&'a str>,
    pub term: Spanned<&'a str>,
    pub then: VecDeque<Ast<'a>>,
    pub else_keyword: Option<Spanned<&'a str>>,
    pub else_: VecDeque<Ast<'a>>,
    pub endif_keyword: Spanned<&'a str>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct If<'a> {
    pub keyword: Spanned<&'a str>,
    pub expr: VecDeque<Ast<'a>>,
    pub then: VecDeque<Ast<'a>>,
    pub else_keyword: Option<Spanned<&'a str>>,
    pub else_: VecDeque<Ast<'a>>,
    pub endif_keyword: Spanned<&'a str>,
}

/// The preprocessor's abstract syntatic tree (AST)
#[derive(Debug, Clone, PartialEq)]
pub enum Ast<'a> {
    Ifdef(IfDefined<'a>),
    Ifndef(IfDefined<'a>),
    If(If<'a>),
    Define(Define),
    Undefine(Spanned<&'a str>, Spanned<&'a str>),
    Include(Spanned<&'a str>, Spanned<&'a str>),
    Comment(Spanned<&'a str>),
    Term(Spanned<&'a str>),
}
