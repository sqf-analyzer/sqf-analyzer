use std::collections::VecDeque;
use std::path::PathBuf;

use crate::error::Error;
use crate::span::Spanned;

use super::include::process_include;
use super::*;

/// An iterator over an [`Ast`] that produces a sequence of tokens
#[derive(Clone)]
pub struct AstIterator<'a> {
    base: VecDeque<Ast<'a>>,
    pub state: State,
}

pub(super) type Arguments = Vec<VecDeque<Spanned<String>>>;

#[derive(Debug, Clone)]
pub struct State {
    pub defines: Defines,
    // stack of define that need to be evaluated. E.g.
    /*
    ```
    #define C(B) C(B)##1
    #define A(B) B##C(B)##1
    ```
    `A(a)` results in `aa11`

    define is the stack of each of the defines that are being evaluated
    `[], [A], [A, C], [A], []`
    */
    pub define: Option<(Define, Arguments)>,
    pub stack: VecDeque<Spanned<String>>,
    pub path: PathBuf,
    pub state: Spanned<MacroState>,
    pub errors: Vec<Error>,
}

fn evaluate_terms<'a>(
    terms: &mut VecDeque<Ast<'a>>,
    state: &mut State,
) -> (bool, Option<SpannedRef<'a>>) {
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
    /// consumed argument and is waiting for a coma or ")"
    Coma,
    /// consumed "(" or "," and it expects an argument
    Argument,
}

/// returns (whether the AST has more terms, the next term in the AST, if any)
/// Examples:
/// * comment: (false, None)
/// * token: (false, Some(...))
fn take_last<'a>(ast: &mut Ast<'a>, state: &mut State) -> (bool, Option<SpannedRef<'a>>) {
    match ast {
        Ast::Ifndef(_, term, else_, then) | Ast::Ifdef(_, term, then, else_) => {
            if state.defines.contains_key(term.inner) {
                evaluate_terms(then, state)
            } else {
                evaluate_terms(else_, state)
            }
        }
        Ast::If(expr, then, else_) => {
            if evaluate_if(expr, &state.defines) {
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
            Some(include) => {
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

fn pop_stack<'a>(stack: &mut VecDeque<Spanned<String>>) -> (bool, Option<SpannedRef<'a>>) {
    stack
        .pop_front()
        .map(|x| (stack.is_empty(), Some(x.map(|x| x.into()))))
        .unwrap_or((false, None))
}

// pulls a new item from terms, evaluating the different functions
fn next<'a>(terms: &mut VecDeque<Ast<'a>>, state: &mut State) -> Option<SpannedRef<'a>> {
    let item = match evaluate_terms(terms, state) {
        (true, None) => next(terms, state)?,
        (false, None) => return None,
        (true, Some(item)) => item,
        (false, Some(item)) => item,
    };

    if let Some(()) = define::update(state, &item) {
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
                state: Spanned::new(MacroState::None, (0, 0)),
                define: Default::default(),
                stack: Default::default(),
                errors: Default::default(),
            },
        }
    }
}

impl<'a> Iterator for AstIterator<'a> {
    type Item = SpannedRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        next(&mut self.base, &mut self.state)
    }
}
