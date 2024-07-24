use std::collections::{HashMap, HashSet, VecDeque};
use std::path::{Path, PathBuf};
use std::sync::Arc;

use crate::error::Error;
use crate::span::{Span, Spanned};

use super::ast::{If, IfDefined};
use super::include::process_include;
use super::*;

/// An iterator over an [`Ast`] that produces a sequence of tokens
#[derive(Clone)]
pub struct AstIterator<'a> {
    base: VecDeque<Ast<'a>>,
    pub state: State,
}

pub(super) type Arguments = Vec<VecDeque<Spanned<Arc<str>>>>;

#[derive(Debug, Clone)]
pub struct DefineState {
    pub define: Define,
    pub arguments: Arguments,
    pub state: Spanned<MacroState>,
}

#[derive(Debug, Clone, Default)]
pub struct PreprocessorState {
    pub define: Option<DefineState>,
    pub in_recursion: HashSet<Arc<str>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Configuration {
    pub defines: Defines,
    /// Path of the file (e.g. ../description.txt or ../bla.sqf)
    pub path: Arc<Path>,
    pub addons: HashMap<Arc<str>, PathBuf>, // e.g. x/cba -> /../cba
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            path: PathBuf::default().into(),
            defines: Default::default(),
            addons: Default::default(),
        }
    }
}

impl Configuration {
    pub fn with_path(path: PathBuf) -> Self {
        Configuration {
            path: path.into(),
            ..Default::default()
        }
    }
}

#[derive(Debug, Clone)]
pub struct State {
    pub configuration: Configuration,
    pub current_macro: PreprocessorState,
    pub if_results: HashMap<Span, bool>,
    pub stack: VecDeque<Spanned<Arc<str>>>,
    pub errors: Vec<Error>,
}

/// Takes a term from the front, removing the Ast from the deque if it is empty
/// Returns whether there are more terms and the term
fn evaluate_terms(
    terms: &mut VecDeque<Ast<'_>>,
    state: &mut State,
) -> (bool, Option<Spanned<Arc<str>>>) {
    // go to front, take (has_more, item); if not has_more; remove it
    match terms.front_mut() {
        Some(item) => {
            let (has_more, item) = take_last(item, state);
            if !has_more {
                terms.pop_front();
            }
            (true, item)
        }
        None => (false, None),
    }
}

#[allow(clippy::needless_bool)]
fn evaluate_ifdef(span: Span, result: bool, if_results: &mut HashMap<Span, bool>) -> bool {
    *if_results.entry(span).or_insert_with(|| result)
}

#[allow(clippy::needless_bool)]
fn evaluate_if(
    span: Span,
    expr: &VecDeque<Ast<'_>>,
    _defines: &Defines,
    if_results: &mut HashMap<Span, bool>,
) -> bool {
    // todo: evaluate the tokens using AstIterator.collect()
    *if_results.entry(span).or_insert_with(|| {
        let first = expr.clone().pop_front();
        if matches!(
            first,
            Some(Ast::Term(Spanned {
                inner: "__has_include",
                ..
            }))
        ) {
            true
        } else {
            false
        }
    })
}

/// State machine to identify macro calls
/// Valid transitions
/// None -> ParenthesisStart
/// ParenthesisStart -> Argument
/// Argument -> Coma
/// Coma -> Argument
/// Coma -> None
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MacroState {
    /// not in a macro
    None,
    /// found a macro and it is waiting for "("
    ParenthesisStart,
    /// consumed "(" or "," and it expects an argument
    /// inner counts the number of openned parenthesis inside arguments
    Argument(i32),
}

/// returns (whether the AST has more terms, the next term in the AST, if any)
/// Examples:
/// * comment: (false, None)
/// * token: (false, Some(...))
fn take_last(ast: &mut Ast<'_>, state: &mut State) -> (bool, Option<Spanned<Arc<str>>>) {
    match ast {
        Ast::Ifndef(IfDefined {
            term, else_, then, ..
        }) => {
            if evaluate_ifdef(
                term.span,
                !state.configuration.defines.contains_key(term.inner),
                &mut state.if_results,
            ) {
                evaluate_terms(then, state)
            } else {
                evaluate_terms(else_, state)
            }
        }
        Ast::Ifdef(IfDefined {
            term, then, else_, ..
        }) => {
            if evaluate_ifdef(
                term.span,
                state.configuration.defines.contains_key(term.inner),
                &mut state.if_results,
            ) {
                evaluate_terms(then, state)
            } else {
                evaluate_terms(else_, state)
            }
        }
        Ast::If(If {
            keyword,
            expr,
            then,
            else_,
            ..
        }) => {
            if evaluate_if(
                keyword.span,
                expr,
                &state.configuration.defines,
                &mut state.if_results,
            ) {
                evaluate_terms(then, state)
            } else {
                evaluate_terms(else_, state)
            }
        }
        Ast::Define(define) => {
            state
                .configuration
                .defines
                .insert(define.name.inner.clone(), define.clone());
            (false, None)
        }
        Ast::Undefine(_, name) => {
            state.configuration.defines.remove(name.inner);
            (false, None)
        }
        Ast::Include(_, name) => match process_include(name, state) {
            Some(mut include) => {
                include.iter_mut().for_each(|x| x.span = name.span);
                state.stack = include;
                pop_stack(&mut state.stack)
            }
            None => (false, None),
        },
        Ast::Term(term) => (false, Some(term.map(|x| x.to_owned().into()))),
        Ast::Comment(_) => (false, None),
    }
}

fn pop_stack(stack: &mut VecDeque<Spanned<Arc<str>>>) -> (bool, Option<Spanned<Arc<str>>>) {
    stack
        .pop_front()
        .map(|x| (stack.is_empty(), Some(x)))
        .unwrap_or((false, None))
}

fn advance(terms: &mut VecDeque<Ast>, state: &mut State) -> (bool, Option<Spanned<Arc<str>>>) {
    let (has_more, item) = pop_stack(&mut state.stack);
    if item.is_some() {
        return (has_more, item);
    };

    evaluate_terms(terms, state)
}

fn ignore_line(
    item: &Option<Spanned<Arc<str>>>,
    terms: &mut VecDeque<Ast>,
    state: &mut State,
) -> bool {
    if let Some(may_line) = &item {
        if may_line.inner.as_ref() == "#line" {
            // ignore the line
            advance(terms, state);
            // ignore the file
            advance(terms, state);
            return true;
        }
    }
    false
}

// pulls a new item from terms, evaluating the different functions
fn next(terms: &mut VecDeque<Ast<'_>>, state: &mut State) -> Option<Spanned<Arc<str>>> {
    // if we have something in the stack, return it
    let (_, item) = pop_stack(&mut state.stack);
    if item.is_some() {
        if ignore_line(&item, terms, state) {
            return next(terms, state);
        }
        return item;
    };

    let (has_more, item) = evaluate_terms(terms, state);
    if ignore_line(&item, terms, state) {
        return next(terms, state);
    }

    let item = match (has_more, item) {
        (true, None) => next(terms, state)?,
        (false, None) => return None,
        (true, Some(item)) => item,
        (false, Some(item)) => item,
    };

    if define::update(state, &item) {
        // token consumed, take next
        return next(terms, state);
    }

    Some(item)
}

impl<'a> AstIterator<'a> {
    /// path represents the location of the config.cpp
    pub fn new(base: VecDeque<Ast<'a>>, configuration: Configuration) -> Self {
        Self {
            base,
            state: State {
                configuration,
                if_results: Default::default(),
                current_macro: Default::default(),
                stack: Default::default(),
                errors: Default::default(),
            },
        }
    }
}

impl<'a> Iterator for AstIterator<'a> {
    type Item = Spanned<Arc<str>>;

    fn next(&mut self) -> Option<Self::Item> {
        next(&mut self.base, &mut self.state)
    }
}
