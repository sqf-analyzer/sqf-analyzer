use std::collections::VecDeque;
use std::path::PathBuf;

use crate::error::Error;
use crate::span::{Span, Spanned};

use super::*;

/// An iterator over an [`Ast`] that produces a sequence of tokens
#[derive(Clone)]
pub struct AstIterator<'a> {
    base: VecDeque<Ast<'a>>,
    pub state: State<'a>,
}

#[derive(Debug, Clone)]
pub struct State<'a> {
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
    pub stack: VecDeque<(Define, Vec<SpannedRef<'a>>)>,
    pub include: VecDeque<Spanned<String>>,
    pub path: PathBuf,
    pub state: MacroState,
    pub errors: Vec<Error>,
}

fn evaluate_terms<'a>(
    terms: &mut VecDeque<Ast<'a>>,
    state: &mut State<'a>,
) -> (bool, Option<SpannedRef<'a>>) {
    // go to front, take (has_more, item); if not has_more; remove it
    match terms.front_mut() {
        Some(item) => {
            let (has_more, item) = take_last(item, state);
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

fn parse_file(
    span: Span,
    content: String,
    state: &mut State,
    path: PathBuf,
) -> Option<VecDeque<Spanned<String>>> {
    // parse into VecDeque<Spanned<String>> due to no lifetime management
    let defines = std::mem::take(&mut state.defines);

    let mut ast = match tokens(&content, defines, path.clone()) {
        Ok(ast) => ast,
        Err(e) => {
            // todo: how do we handle errors in other files? Error may need a lift to track files
            let error = Error {
                inner: format!(
                    "Error while parsing file \"{}\" at position {:?}: {}",
                    path.display(),
                    e.span,
                    e.inner
                ),
                span,
            };
            state.errors.push(error);
            return None;
        }
    };

    let tokens = ast.by_ref().map(|x| x.map(|x| x.into_owned())).collect();
    // collect errors and defines
    state.errors.extend(ast.state.errors);
    state.defines = ast.state.defines;

    Some(tokens)
}

fn process_include(name: &Spanned<&str>, state: &mut State) -> Option<VecDeque<Spanned<String>>> {
    let mut path = state.path.clone();
    path.pop();
    path.push(name.inner);
    match std::fs::read_to_string(path.clone()) {
        Err(_) => {
            state.errors.push(Error {
                inner: format!("Cannot read file in path \"{}\"", path.display()),
                span: name.span,
            });
            None
        }
        Ok(content) => parse_file(name.span, content, state, path),
    }
}

fn take_last<'a>(ast: &mut Ast<'a>, state: &mut State<'a>) -> (bool, Option<SpannedRef<'a>>) {
    match ast {
        Ast::Ifndef(term, else_, then) | Ast::Ifdef(term, then, else_) => {
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
        Ast::Undefine(name) => {
            state.defines.remove(name.inner);
            (false, None)
        }
        Ast::Include(name) => match process_include(name, state) {
            Some(include) => {
                state.include = include;
                pop_include(&mut state.include)
            }
            None => (false, None),
        },
        Ast::Body(terms) => evaluate_terms(terms, state),
        Ast::Term(term) => (false, Some(term.map(|x| x.to_owned().into()))),
        Ast::Comment(_) => (false, None),
    }
}

fn pop_include<'a>(stack: &mut VecDeque<Spanned<String>>) -> (bool, Option<SpannedRef<'a>>) {
    stack
        .pop_front()
        .map(|x| (stack.is_empty(), Some(x.map(|x| x.into()))))
        .unwrap_or((false, None))
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
                state: MacroState::None,
                stack: Default::default(),
                include: Default::default(),
                errors: Default::default(),
            },
        }
    }

    fn pop_define(&mut self) -> Option<SpannedRef<'a>> {
        if self.state.stack.is_empty() {
            return None;
        }
        if self.state.state != MacroState::None {
            return None;
        }

        // clear any defines that have an empty body
        self.state.stack.retain(|x| !x.0.body.is_empty());

        // get the first front item of the stack that has all arguments available
        let (define, arguments) = self
            .state
            .stack
            .iter_mut()
            .rev()
            .find(|(define, args)| define.arguments.len() == args.len())?;

        // argument replacement
        let replace = |arg: String| {
            define
                .arguments
                .iter()
                .zip(arguments.iter())
                .find(|x| x.0.inner == arg)
                .map(|x| x.1.inner.clone())
                .unwrap_or_else(|| arg.into())
        };

        // operator expansion
        define.body.pop_front().map(|x| match x {
            DefineExpr::Quote(lhs) => Spanned {
                inner: format!("\"{}\"", replace(lhs.inner)).into(),
                span: lhs.span,
            },
            DefineExpr::Concat(lhs, rhs) => Spanned {
                inner: format!(
                    "{}{}",
                    replace(lhs.inner).as_ref(),
                    replace(rhs.inner).as_ref()
                )
                .into(),
                span: (lhs.span.0, rhs.span.1),
            },
            DefineExpr::Term(term) => term.map(|x| x.into()),
        })
    }

    fn define_update(&mut self, item: &SpannedRef<'a>) -> Option<()> {
        let define = self.state.defines.get(item.inner.as_ref())?;

        // there is a define with this token => push it to the stack
        self.state.stack.push_back((define.clone(), vec![]));
        if self.state.state != MacroState::None {
            self.state.errors.push(Error {
                inner: "macro invoked inside another macro".to_string(),
                span: item.span,
            });
            self.state.state = MacroState::None;
        }
        if !define.arguments.is_empty() {
            self.state.state = MacroState::ParenthesisStart;
        }
        // ignore the token and request another one (possibly a paranthesis for arguments)
        Some(())
    }

    fn define_arg_update(&mut self, item: &SpannedRef<'a>) -> Option<()> {
        // get the top of the stack missing arguments
        let (_, arguments) = self
            .state
            .stack
            .iter_mut()
            .rev()
            .find(|(define, arguments)| define.arguments.len() != arguments.len())?;

        arguments.push(item.clone());
        self.state.state = MacroState::Coma;
        Some(())
    }
}

impl<'a> Iterator for AstIterator<'a> {
    type Item = SpannedRef<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        // if we are in the define state, advance it
        let (has_more, item) = if let Some(item) = self.pop_define() {
            (true, Some(item))
        } else {
            evaluate_terms(&mut self.base, &mut self.state)
        };

        if self.state.state == MacroState::Coma
            && matches!(
                item.as_ref().map(|x| x.as_deref().map(|x| x.as_ref())),
                Some(Spanned { inner: ")", .. })
            )
        {
            self.state.state = MacroState::None;
            return self.next();
        }

        if matches!(
            self.state.state,
            MacroState::ParenthesisStart | MacroState::Coma
        ) {
            self.state.state = MacroState::Argument;
            return self.next();
        }

        if let Some(item) = &item {
            if let Some(()) = self.define_update(item) {
                return self.next();
            };

            if let Some(()) = self.define_arg_update(item) {
                return self.next();
            };
        }

        match (has_more, item) {
            (true, None) => self.next(),
            (true, item) => item,
            (false, None) => None,
            (false, item) => item,
        }
    }
}
