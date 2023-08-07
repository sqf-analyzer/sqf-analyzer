use std::collections::hash_map::HashMap;
use std::path::PathBuf;

use crate::database::SIGNATURES;
use crate::error::Error;
use crate::parser::Expr;
use crate::preprocessor::SpannedRef;
use crate::span::{Span, Spanned};
use crate::types::*;

fn _build_binary() -> HashMap<&'static str, HashMap<(Type, Type), Type>> {
    let mut r: HashMap<&'static str, HashMap<(Type, Type), Type>> = Default::default();
    for s in SIGNATURES {
        if let Signature::Binary(lhs, name, rhs, type_) = s {
            r.entry(name)
                .and_modify(|e| {
                    e.insert((lhs, rhs), type_);
                })
                .or_insert_with(|| HashMap::from([((lhs, rhs), type_)]));
        }
    }
    r
}

fn _build_unary() -> HashMap<&'static str, HashMap<Type, Type>> {
    let mut r: HashMap<&'static str, HashMap<Type, Type>> = Default::default();
    for s in SIGNATURES {
        if let Signature::Unary(name, rhs, type_) = s {
            r.entry(name)
                .and_modify(|e| {
                    e.insert(rhs, type_);
                })
                .or_insert_with(|| HashMap::from([(rhs, type_)]));
        }
    }
    r
}

fn _build_nullary() -> HashMap<&'static str, Type> {
    let mut r: HashMap<&'static str, Type> = Default::default();
    for s in SIGNATURES {
        if let Signature::Nullary(name, type_) = s {
            r.insert(name, type_);
        }
    }
    r
}

lazy_static::lazy_static! {

    pub static ref BINARY: HashMap<&'static str, HashMap<(Type, Type), Type>> = {
        _build_binary()
    };

    pub static ref UNARY: HashMap<&'static str, HashMap<Type, Type>> = {
        _build_unary()
    };

    pub static ref NULLARY: HashMap<&'static str, Type> = {
        _build_nullary()
    };
}

#[inline]
fn _is_private(v: &str) -> bool {
    v.as_bytes().first() == Some(&b'_')
}

fn process_param_variable(v: &str, span: Span, state: &mut State, type_: Type) {
    if _is_private(v) {
        state.namespace.insert(
            Spanned {
                inner: v.to_owned(),
                span,
            },
            Some(type_),
            true,
        );
    } else {
        state.errors.push(Spanned {
            span,
            inner: "Argument must begin with _".to_string(),
        })
    }
}

fn process_param_types(
    name: &str,
    name_span: Span,
    types: Option<&Expr>,
    state: &mut State,
) -> Option<Type> {
    let types = if let Some(types) = types {
        types
    } else {
        process_param_variable(name, name_span, state, Type::Anything);
        return None;
    };

    let (types, span) = if let Expr::Array(expr) = &types {
        (expr, expr.span)
    } else {
        state.errors.push(Spanned {
            span: types.span(),
            inner: "params' third argument must be an array".to_string(),
        });
        return None;
    };

    Some(types.inner.iter().fold(Type::Nothing, |acc, maybe_type| {
        // reduce the list of types to a single type
        let type_ = infer_type(maybe_type, state);
        let type_ = type_.unwrap_or_else(|| {
            state.errors.push(Spanned {
                span,
                inner: "params' third argument's elements must be typed".to_string(),
            });
            Type::Anything
        });
        match acc {
            // first type => get it
            Type::Nothing => type_,
            // more than one type => Anything
            _ => Type::Anything,
        }
    }))
}

fn process_params_variable(param: &Expr, state: &mut State) {
    if let Expr::String(v) = &param {
        process_param_variable(&v.inner, param.span(), state, Type::Anything);
        return;
    }

    let Expr::Array(expr) = param else {
        state.errors.push(Spanned {
            span: param.span(),
            inner: "params' argument must be either a string or array".to_string(),
        });
        return;
    };

    let mut iter = expr.inner.iter();
    let name = iter.next();
    let default = iter.next();
    let types = iter.next();
    let _counts = iter.next();
    for remaining in iter {
        state.errors.push(Spanned {
            span: remaining.span(),
            inner: "params' arguments only accept up to 4 arguments".to_string(),
        })
    }

    let (name, name_span) = if let Some(Expr::String(Spanned {
        inner: name,
        span: name_span,
    })) = name
    {
        (name, *name_span)
    } else {
        state.errors.push(Spanned {
            span: param.span(),
            inner: "params' first argument must be a string".to_string(),
        });
        return;
    };

    let Some(default) = default else {
        process_param_variable(name, name_span, state, Type::Anything);
        return;
    };
    let default_type = infer_type(default, state).unwrap_or(Type::Anything);

    let Some(type_) = process_param_types(name, name_span, types, state) else {
        return
    };

    if !type_.consistent(default_type) {
        state.errors.push(Spanned {
            span: param.span(),
            inner: format!("params' default argument type \"{:?}\" is inconsistent with expected type \"{:?}\"", default_type, type_),
        });
    }

    process_param_variable(name, name_span, state, type_)
}

/// infers the type of a bynary expression by considering all possible options
fn infer_unary(
    name: &SpannedRef,
    rhs: Option<Type>,
    errors: &mut Vec<Spanned<String>>,
) -> Option<Type> {
    let lower = name.inner.to_ascii_lowercase();

    let Some(options) = UNARY.get(lower.as_str()) else {
        errors.push(Spanned {
            span: name.span,
            inner: format!("No unary operator named \"{}\"", name.inner),
        });
        return None;
    };

    match rhs {
        None => {
            let mut options = options.values();
            options
                .next()
                .and_then(|first| options.next().is_none().then_some(*first))
        }
        Some(rhs) => {
            let mut options = options
                .iter()
                .filter_map(|(x_rhs, type_)| x_rhs.consistent(rhs).then_some(type_));
            let first = options.next();
            if let Some(first) = first {
                options.next().is_none().then_some(*first)
            } else if rhs == Type::Anything {
                None
            } else {
                errors.push(Spanned {
                    span: name.span,
                    inner: format!(
                        "\"{}\" does not support a rhs of type \"{:?}\"",
                        name.inner, rhs
                    ),
                });
                None
            }
        }
    }
}

/// infers the type of a bynary expression by considering all possible options
fn infer_binary(
    lhs: Option<Type>,
    name: &SpannedRef,
    rhs: Option<Type>,
    errors: &mut Vec<Spanned<String>>,
) -> Option<Type> {
    let lower = name.inner.to_ascii_lowercase();

    let Some(options) = BINARY.get(lower.as_str()) else {
        errors.push(Spanned {
            span: name.span,
            inner: format!("No binary operator named \"{}\"", name.inner),
        });
        return None;
    };

    match (lhs, rhs) {
        (None, None) => {
            let mut options = options.values();
            options
                .next()
                .and_then(|first| options.next().is_none().then_some(*first))
        }
        (None, Some(rhs)) => {
            let mut options = options
                .iter()
                .filter_map(|((_, x_rhs), type_)| x_rhs.consistent(rhs).then_some(type_));
            let first = options.next();
            if let Some(first) = first {
                options.next().is_none().then_some(*first)
            } else if rhs == Type::Anything {
                None
            } else {
                errors.push(Spanned {
                    span: name.span,
                    inner: format!(
                        "\"{}\" does not support a rhs of type \"{:?}\"",
                        name.inner, rhs
                    ),
                });
                None
            }
        }
        (Some(lhs), None) => {
            let mut options = options
                .iter()
                .filter_map(|((x_lhs, _), type_)| x_lhs.consistent(lhs).then_some(type_));
            let first = options.next();
            if let Some(first) = first {
                options.next().is_none().then_some(*first)
            } else {
                if lhs != Type::Anything {
                    errors.push(Spanned {
                        span: name.span,
                        inner: format!(
                            "No binary operator named \"{}\" with lhs of type \"{:?}\"",
                            name.inner, rhs
                        ),
                    });
                }
                None
            }
        }
        (Some(lhs), Some(rhs)) => options.get(&(lhs, rhs)).cloned(),
    }
}

fn infer_code(code: &[Expr], state: &mut State) {
    state.namespace.push_stack();
    for expr in code {
        infer_type(expr, state);
    }
    state.namespace.pop_stack();
}

fn infer_assign(lhs: &Expr, rhs: &Expr, state: &mut State) {
    let rhs_type = infer_type(rhs, state);
    let (is_private, variable) = if let Expr::Unary(Spanned { inner, .. }, variable) = lhs {
        // private a = ...
        if inner.as_ref() != "private" {
            state.errors.push(Error {
                inner: "assigment can only have variables in left side".to_string(),
                span: variable.span(),
            });
            return;
        }

        let Expr::Variable(variable) = variable.as_ref() else {
            state.errors.push(Error {
                inner: "assigment can only have variables in left side".to_string(),
                span: variable.span(),
            });
            return;
        };
        (true, variable)
    } else {
        // a = ...
        let Expr::Variable(variable) = lhs else {
            state.errors.push(Error {
                inner: "assigment can only have variables in left side".to_string(),
                span: lhs.span(),
            });
            return
        };
        (false, variable)
    };
    let variable = variable.clone().map(|x| x.to_string());

    state.types.insert(variable.clone(), rhs_type);
    state.namespace.insert(variable, rhs_type, is_private);
}

fn infer_type(expr: &Expr, state: &mut State) -> Option<Type> {
    match expr {
        Expr::Number(_) => Some(Type::Number),
        Expr::String(_) => Some(Type::String),
        Expr::Boolean(_) => Some(Type::Boolean),
        Expr::Array(expr) => {
            infer_code(&expr.inner, state);
            Some(Type::Array)
        }
        Expr::Code(code) => {
            infer_code(&code.inner, state);
            Some(Type::Code)
        }
        Expr::Nullary(variable) => {
            let name = variable.inner.to_ascii_lowercase();
            NULLARY.get(name.as_str()).cloned()
        }
        Expr::Variable(variable) => {
            let name = variable.inner.to_ascii_lowercase();

            (name == "_this").then_some(Type::Anything).or_else(|| {
                if let Some((span, type_)) = state.namespace.get(&variable.inner) {
                    state.origins.insert(variable.span, span);
                    type_
                } else {
                    None
                }
            })
        }
        Expr::Binary(lhs, op, rhs) => {
            if op.inner == "=" {
                infer_assign(lhs, rhs, state);
                Some(Type::Nothing)
            } else {
                let lhs_type = infer_type(lhs, state);
                let rhs_type = infer_type(rhs, state);
                infer_binary(lhs_type, op, rhs_type, &mut state.errors)
            }
        }
        Expr::Unary(op, rhs) => {
            match op.inner.as_ref() {
                "for" => {
                    if let Expr::String(x) = rhs.as_ref() {
                        state.types.insert(
                            Spanned {
                                inner: x.inner.to_string(),
                                span: x.span,
                            },
                            Some(Type::Number),
                        );
                    } else {
                        state.errors.push(Spanned {
                            inner: "parameter of `for` must be a string".to_string(),
                            span: rhs.span(),
                        })
                    }
                }
                "params" => {
                    if let Expr::Array(x) = rhs.as_ref() {
                        x.inner
                            .iter()
                            .for_each(|x| process_params_variable(x, state))
                    }
                }
                _ => {}
            };
            let rhs_type = infer_type(rhs, state);
            infer_unary(op, rhs_type, &mut state.errors)
        }
        Expr::Nil(_) => None,
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Namespace {
    pub stack: Vec<HashMap<String, (Span, Option<Type>)>>,
    pub mission: HashMap<String, (Origin, Option<Type>)>,
}

impl Namespace {
    #[allow(clippy::map_entry)]
    pub fn insert(&mut self, key: Spanned<String>, value: Option<Type>, is_private: bool) {
        if is_private {
            self.stack
                .last_mut()
                .unwrap()
                .insert(key.inner, (key.span, value));
        } else {
            for stack in self.stack.iter_mut().rev() {
                if stack.contains_key(&key.inner) {
                    // entries API would require cloning, which is more expensive than this lookup
                    stack.insert(key.inner, (key.span, value));
                    return;
                }
            }
            self.mission
                .insert(key.inner, (Origin::File(key.span), value));
        }
    }

    pub fn push_stack(&mut self) {
        self.stack.push(Default::default());
    }

    pub fn pop_stack(&mut self) {
        self.stack.pop();
    }

    pub fn get(&self, key: &str) -> Option<(Origin, Option<Type>)> {
        for stack in self.stack.iter().rev() {
            if let Some((span, a)) = stack.get(key) {
                return Some((Origin::File(*span), *a));
            }
        }
        self.mission.get(key).copied()
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Origin {
    File(Span),
    External(Span, usize),
}

#[derive(Debug, Default, PartialEq)]
pub struct State {
    pub types: HashMap<Spanned<String>, Option<Type>>,
    pub namespace: Namespace,
    pub origins: HashMap<Span, Origin>,
    pub errors: Vec<Error>,
}

#[derive(Debug)]
pub struct Configuration {
    pub directory: PathBuf,
}

pub type Types = HashMap<Spanned<String>, Option<Type>>;

pub fn analyze(program: &[Expr]) -> State {
    let mut state = State::default();

    state.namespace.push_stack();
    for expr in program {
        infer_type(expr, &mut state);
    }
    state
}
