use std::collections::{HashMap, VecDeque};
use std::path::PathBuf;
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

pub(super) type DefineState = (Define, Arguments, Spanned<MacroState>);

#[derive(Debug, Clone)]
pub struct State {
    pub defines: Defines,
    pub current_macro: Option<DefineState>,
    pub if_results: HashMap<Span, bool>,
    pub stack: VecDeque<Spanned<Arc<str>>>,
    pub path: PathBuf,
    pub errors: Vec<Error>,
}

fn evaluate_terms<'a>(
    terms: &mut VecDeque<Ast<'a>>,
    state: &mut State,
) -> (bool, Option<Spanned<Arc<str>>>) {
    let (has_more, item) = pop_stack(&mut state.stack);
    if item.is_some() {
        return (has_more, item);
    };

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
fn evaluate_if<'a>(
    span: Span,
    expr: &VecDeque<Ast<'a>>,
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
fn take_last<'a>(ast: &mut Ast<'a>, state: &mut State) -> (bool, Option<Spanned<Arc<str>>>) {
    match ast {
        Ast::Ifndef(IfDefined {
            term, else_, then, ..
        })
        | Ast::Ifdef(IfDefined {
            term, then, else_, ..
        }) => {
            if state.defines.contains_key(term.inner) {
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
            if evaluate_if(keyword.span, expr, &state.defines, &mut state.if_results) {
                evaluate_terms(then, state)
            } else {
                evaluate_terms(else_, state)
            }
        }
        Ast::Define(define) => {
            state
                .defines
                .insert(define.name.inner.clone(), define.clone());
            (false, None)
        }
        Ast::Undefine(_, name) => {
            state.defines.remove(name.inner);
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
        Ast::Body(terms) => evaluate_terms(terms, state),
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

// pulls a new item from terms, evaluating the different functions
fn next<'a>(terms: &mut VecDeque<Ast<'a>>, state: &mut State) -> Option<Spanned<Arc<str>>> {
    let (mut has_more, mut item) = evaluate_terms(terms, state);
    if let Some(may_line) = &item {
        if may_line.inner.as_ref() == "#line" {
            // pop the line
            evaluate_terms(terms, state);
            // pop the file
            evaluate_terms(terms, state);

            (has_more, item) = evaluate_terms(terms, state);
        }
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
    pub fn new(base: Ast<'a>, defines: Defines, path: PathBuf) -> Self {
        let Ast::Body(base) = base else {
            panic!()
        };
        Self {
            base,
            state: State {
                defines,
                path,
                if_results: Default::default(),
                current_macro: None,
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
