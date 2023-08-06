use std::collections::{HashMap, VecDeque};

use crate::span::Spanned;

/// # Note
/// This struct cannot have an associated lifetime because it can be shared between files,
/// that have been opened with diferent lifetimes
#[derive(Debug, Clone)]
pub struct Define {
    pub name: Spanned<String>,
    pub arguments: Vec<Spanned<String>>,
    pub body: VecDeque<Spanned<String>>,
}

pub type Defines = HashMap<String, Define>;

/// The preprocessor's abstract syntatic tree (AST)
#[derive(Debug, Clone)]
pub enum Ast<'a> {
    Ifdef(Spanned<&'a str>, VecDeque<Ast<'a>>, VecDeque<Ast<'a>>),
    Ifndef(Spanned<&'a str>, VecDeque<Ast<'a>>, VecDeque<Ast<'a>>),
    If(
        VecDeque<Spanned<&'a str>>,
        VecDeque<Ast<'a>>,
        VecDeque<Ast<'a>>,
    ),
    Define(Define),
    Undefine(Spanned<&'a str>),
    Include(Spanned<&'a str>),
    Body(VecDeque<Ast<'a>>),
    Comment(Spanned<&'a str>),
    Term(Spanned<&'a str>),
}