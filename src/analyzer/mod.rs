use std::collections::hash_map::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use crate::database::SIGNATURES;
use crate::error::Error;
use crate::parser::Expr;
use crate::span::{Span, Spanned};
use crate::types::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Arc<str>,
    pub type_: Type,
    pub has_default: bool,
}

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

fn process_param_variable(v: &str, span: Span, state: &mut State, type_: Type, has_default: bool) {
    if _is_private(v) {
        let p = Parameter {
            name: v.into(),
            type_,
            has_default,
        };
        let s = &mut state.namespace.stack.last_mut().unwrap().signature;
        if let Some(s) = s {
            s.push(p)
        } else {
            *s = Some(vec![p])
        }

        state.namespace.insert(
            Spanned {
                inner: v.to_owned(),
                span,
            },
            Some(type_.into()),
            true,
        );
    } else {
        state.errors.push(Spanned {
            span,
            inner: "Argument must begin with _".to_string(),
        })
    }
}

fn process_param_types(types: &Expr, state: &mut State) -> Option<Type> {
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
        let type_ = infer_type(maybe_type, state).map(|x| x.type_());
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
        process_param_variable(&v.inner, param.span(), state, Type::Anything, false);
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
        process_param_variable(name, name_span, state, Type::Anything, false);
        return;
    };
    let default_type = infer_type(default, state)
        .map(|x| x.type_())
        .unwrap_or(Type::Anything);

    let Some(types) = types else {
        process_param_variable(name, name_span, state, default_type, true);
        return;
    };

    let Some(type_) = process_param_types(types, state) else {
        return
    };

    if !type_.consistent(default_type) {
        state.errors.push(Spanned {
            span: param.span(),
            inner: format!("params' default argument type \"{:?}\" is inconsistent with expected type \"{:?}\"", default_type, type_),
        });
    }

    process_param_variable(name, name_span, state, type_, true)
}

/// infers the type of a bynary expression by considering all possible options
fn infer_unary(
    name: &Spanned<Arc<str>>,
    rhs: Option<Output>,
    errors: &mut Vec<Spanned<String>>,
) -> Option<Output> {
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
                .map(|x| x.into())
        }
        Some(rhs) => {
            let mut options = options
                .iter()
                .filter_map(|(x_rhs, type_)| x_rhs.consistent(rhs.type_()).then_some(type_));
            let first = options.next();
            if let Some(first) = first {
                options.next().is_none().then_some(*first).map(|x| x.into())
            } else if rhs.type_() == Type::Anything {
                None
            } else {
                errors.push(Spanned {
                    span: name.span,
                    inner: format!(
                        "\"{}\" does not support a rhs of type \"{:?}\"",
                        name.inner,
                        rhs.type_()
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
    name: &Spanned<Arc<str>>,
    rhs: Option<Type>,
    errors: &mut Vec<Spanned<String>>,
) -> Option<Output> {
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
                .map(|x| x.into())
        }
        (None, Some(rhs)) => {
            let mut options = options
                .iter()
                .filter_map(|((_, x_rhs), type_)| x_rhs.consistent(rhs).then_some(type_));
            let first = options.next();
            if let Some(first) = first {
                options.next().is_none().then_some(*first).map(|x| x.into())
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
                options.next().is_none().then_some(*first).map(|x| x.into())
            } else {
                if lhs != Type::Anything {
                    errors.push(Spanned {
                        span: name.span,
                        inner: format!(
                            "No binary operator named \"{}\" with lhs of type \"{:?}\"",
                            name.inner, lhs
                        ),
                    });
                }
                None
            }
        }
        (Some(lhs), Some(rhs)) => options.get(&(lhs, rhs)).cloned().map(|x| x.into()),
    }
}

fn infer_expressions(expressions: &[Expr], state: &mut State) {
    for expr in expressions {
        infer_type(expr, state);
    }
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

    state
        .types
        .insert(variable.span, rhs_type.as_ref().map(|x| x.type_()));
    state.namespace.insert(variable, rhs_type, is_private);
}

fn process_parameters(lhs: &Spanned<Vec<Expr>>, parameters: &[Parameter], state: &mut State) {
    let required_args = parameters.iter().filter(|x| !x.has_default).count();

    if lhs.inner.len() < required_args || lhs.inner.len() > parameters.len() {
        state.errors.push(Error {
            inner: format!(
                "Function expects {} parameters, but received {}",
                parameters.len(),
                lhs.inner.len()
            ),
            span: lhs.span,
        })
    }
    state.parameters.extend(
        lhs.inner
            .iter()
            .zip(parameters.iter())
            .map(|(lhs, param)| (lhs.span(), param.name.clone())),
    );
}

fn infer_type(expr: &Expr, state: &mut State) -> Option<Output> {
    match expr {
        Expr::Number(_) => Some(Type::Number.into()),
        Expr::String(_) => Some(Type::String.into()),
        Expr::Boolean(_) => Some(Type::Boolean.into()),
        Expr::Array(expr) => {
            infer_expressions(&expr.inner, state);
            Some(Type::Array.into())
        }
        Expr::Code(code) => {
            state.namespace.push_stack();
            infer_expressions(&code.inner, state);
            let parameters =
                std::mem::take(&mut state.namespace.stack.last_mut().unwrap().signature);
            state.namespace.pop_stack();
            if let Some(parameters) = parameters {
                Some(Output::Code(parameters))
            } else {
                Some(Type::Code.into())
            }
        }
        Expr::Nullary(variable) => {
            let name = variable.inner.to_ascii_lowercase();
            NULLARY.get(name.as_str()).cloned().map(|x| x.into())
        }
        Expr::Variable(variable) => {
            let name = variable.inner.to_ascii_lowercase();

            (name == "_this")
                .then_some(Type::Anything.into())
                .or_else(|| {
                    if let Some((origin, type_)) = state.namespace.get(&variable.inner) {
                        state.origins.insert(variable.span, origin);
                        type_
                    } else {
                        None
                    }
                })
        }
        Expr::Binary(lhs, op, rhs) => {
            if op.inner.as_ref() == "=" {
                infer_assign(lhs, rhs, state);
                Some(Type::Nothing.into())
            } else {
                let rhs = infer_type(rhs, state);
                if op.inner.to_ascii_lowercase() == "call" {
                    if let Some(Output::Code(parameters)) = &rhs {
                        if let Expr::Array(lhs) = lhs.as_ref() {
                            // annotate parameters with the functions' signature if it is available
                            process_parameters(lhs, parameters, state)
                        }
                    }
                }
                let lhs = infer_type(lhs, state).map(|x| x.type_());
                infer_binary(lhs, op, rhs.map(|x| x.type_()), &mut state.errors)
            }
        }
        Expr::Unary(op, rhs) => {
            match op.inner.as_ref() {
                "for" => {
                    if let Expr::String(x) = rhs.as_ref() {
                        state.types.insert(x.span, Some(Type::Number));
                    } else {
                        state.errors.push(Spanned {
                            inner: "parameter of `for` must be a string".to_string(),
                            span: rhs.span(),
                        })
                    }
                }
                "params" => {
                    // store the names of the variables to build the function's signature
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
pub struct Stack {
    pub variables: HashMap<String, (Span, Option<Output>)>,
    pub signature: Option<Vec<Parameter>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Namespace {
    pub stack: Vec<Stack>,
    pub mission: HashMap<Arc<str>, (Origin, Option<Output>)>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    Type(Type),
    Code(Vec<Parameter>),
}

impl Output {
    pub fn type_(&self) -> Type {
        match self {
            Output::Type(t) => *t,
            Output::Code(_) => Type::Code,
        }
    }
}

impl From<Type> for Output {
    fn from(value: Type) -> Self {
        Output::Type(value)
    }
}

impl Namespace {
    #[allow(clippy::map_entry)]
    pub fn insert(&mut self, key: Spanned<String>, value: Option<Output>, is_private: bool) {
        if is_private {
            self.stack
                .last_mut()
                .unwrap()
                .variables
                .insert(key.inner, (key.span, value));
        } else {
            for stack in self.stack.iter_mut().rev() {
                if stack.variables.contains_key(&key.inner) {
                    // entries API would require cloning, which is more expensive than this lookup
                    stack.variables.insert(key.inner, (key.span, value));
                    return;
                }
            }
            self.mission
                .insert(key.inner.into(), (Origin::File(key.span), value));
        }
    }

    pub fn push_stack(&mut self) {
        self.stack.push(Default::default());
    }

    pub fn pop_stack(&mut self) {
        self.stack.pop();
    }

    pub fn get(&self, key: &str) -> Option<(Origin, Option<Output>)> {
        for stack in self.stack.iter().rev() {
            if let Some((span, a)) = stack.variables.get(key) {
                return Some((Origin::File(*span), a.clone()));
            }
        }
        self.mission.get(key).cloned()
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Origin {
    File(Span),
    External(Arc<str>),
}

pub type Types = HashMap<Span, Option<Type>>;
pub type Parameters = HashMap<Span, Arc<str>>;

#[derive(Debug, Default, PartialEq)]
pub struct State {
    pub types: Types,
    pub parameters: Parameters,
    pub namespace: Namespace,
    pub origins: HashMap<Span, Origin>,
    // parameters in the current scope (last call of `params []` in the scope)
    //current_signature: Option<Vec<Parameter>>,
    // parameters in the root scope (last call of `params []` in the first stack)
    //pub signature: Option<Vec<Parameter>>,
    pub errors: Vec<Error>,
}

impl State {
    pub fn signature(&self) -> Option<&Vec<Parameter>> {
        self.namespace.stack.last().unwrap().signature.as_ref()
    }
}

#[derive(Debug)]
pub struct Configuration {
    pub directory: PathBuf,
}

pub fn analyze(program: &[Expr], state: &mut State) {
    state.namespace.push_stack();
    for expr in program {
        infer_type(expr, state);
    }
}
