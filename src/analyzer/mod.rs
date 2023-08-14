use std::collections::hash_map::HashMap;
use std::path::PathBuf;
use std::sync::Arc;

use crate::error::Error;
use crate::parser::Expr;
use crate::span::{Span, Spanned};
use crate::types::*;

mod database;
mod operators;
pub use database::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Parameter {
    pub name: Arc<str>,
    pub type_: Type,
    pub has_default: bool,
}

#[inline]
fn _is_private(v: &str) -> bool {
    matches!(v.as_bytes().first(), Some(&b'_') | None)
}

fn process_param_variable(
    v: &Arc<str>,
    span: Span,
    state: &mut State,
    type_: Type,
    has_default: bool,
) {
    if !_is_private(v) {
        state.errors.push(Spanned {
            span,
            inner: "Argument must begin with _".to_string(),
        });
        return;
    }

    let p = Parameter {
        name: v.clone(),
        type_,
        has_default,
    };
    if let Some(s) = state.namespace.stack.last_mut().unwrap().signature.as_mut() {
        s.push(p)
    } else {
        state.namespace.stack.last_mut().unwrap().signature = Some(vec![p])
    };

    state.namespace.insert(
        Spanned {
            inner: v.clone(),
            span,
        },
        Some(type_.into()),
        true,
    );
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
) -> Option<InnerType> {
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
            let options = options.values();
            take_only(options)
        }
        Some(rhs) => {
            let options = options
                .iter()
                .filter_map(|(x_rhs, type_)| x_rhs.consistent(rhs.type_()).then_some(type_));

            if let Some(first) = take_only(options) {
                Some(first)
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
    span: Span,
    errors: &mut Vec<Spanned<String>>,
) -> Option<InnerType> {
    let lower = name.inner.to_ascii_lowercase();

    let Some(options) = BINARY.get(lower.as_str()) else {
        errors.push(Spanned {
            span,
            inner: format!("\"{}\" is not a binary operator", name.inner),
        });
        return None;
    };

    match (lhs, rhs) {
        (None, None) => {
            let options = options.values();
            take_only(options)
        }
        (None, Some(rhs)) => {
            let options = options
                .iter()
                .filter_map(|((_, x_rhs), type_)| x_rhs.consistent(rhs).then_some(type_));
            if let Some(first) = take_only(options) {
                Some(first)
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
            let options = options
                .iter()
                .filter_map(|((x_lhs, _), type_)| x_lhs.consistent(lhs).then_some(type_));
            if let Some(first) = take_only(options) {
                Some(first)
            } else {
                if lhs != Type::Anything {
                    errors.push(Spanned {
                        span: name.span,
                        inner: format!(
                            "\"{}\" does not support a left side of type \"{:?}\"",
                            name.inner, lhs
                        ),
                    });
                }
                None
            }
        }
        (Some(lhs), Some(rhs)) => {
            let options = options.iter().filter_map(|((x_lhs, x_rhs), type_)| {
                (x_lhs.consistent(lhs) && x_rhs.consistent(rhs)).then_some(type_)
            });
            if let Some(first) = take_only(options) {
                Some(first)
            } else {
                errors.push(Spanned {
                    span: name.span,
                    inner: format!(
                        "\"{}\" does not support left side of type \"{:?}\" and right side of type \"{:?}\"",
                        name.inner, lhs, rhs
                    ),
                });
                None
            }
        }
    }
}

fn infer_expressions(expressions: &[Expr], state: &mut State) -> Option<Output> {
    let mut output = None;
    for expr in expressions {
        output = infer_type(expr, state);
    }
    output
}

fn infer_assign(lhs: &Expr, rhs: &Expr, state: &mut State) {
    let rhs_type = infer_type(rhs, state);
    let (is_private, variable) = if let Expr::Unary(Spanned { inner, span }, variable) = lhs {
        // private a = ...
        if inner.as_ref() != "private" {
            state.errors.push(Error {
                inner: "assigment can only have variables in left side".to_string(),
                span: variable.span(),
            });
            return;
        }
        state.explanations.insert(
            *span,
            UNARY
                .get(&"private")
                .unwrap()
                .values()
                .next()
                .unwrap()
                .first()
                .unwrap()
                .1,
        );

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
    let variable = variable.clone().map(|x| x.to_string().into());

    state
        .types
        .insert(variable.span, rhs_type.as_ref().map(|x| x.type_()));
    state.namespace.insert(variable, rhs_type, is_private);
}

fn process_parameters(lhs: &Spanned<Vec<Expr>>, parameters: &[Parameter], state: &mut State) {
    if lhs.inner.len() > parameters.len() {
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
        Expr::Array(expr) => {
            infer_expressions(&expr.inner, state);
            Some(Type::Array.into())
        }
        Expr::Code(code) => {
            state.namespace.push_stack();
            let return_type = infer_expressions(&code.inner, state);
            let parameters =
                std::mem::take(&mut state.namespace.stack.last_mut().unwrap().signature);
            state.namespace.pop_stack();
            Some(Output::Code(parameters, return_type.map(|x| x.type_())))
        }
        Expr::Nullary(variable) => {
            let name = variable.inner.to_ascii_lowercase();
            NULLARY
                .get(name.as_str())
                .cloned()
                .map(|(type_, explanation)| {
                    state.explanations.insert(variable.span, explanation);
                    type_.into()
                })
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
            } else if op.inner.as_ref().eq_ignore_ascii_case("then") {
                operators::then(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("foreach") {
                operators::foreach(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("else") {
                operators::else_(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("remoteexec") {
                operators::remoteexec(expr.span(), lhs, op, rhs, state)
            } else {
                if op.inner.eq_ignore_ascii_case("call") {
                    let rhs = infer_type(rhs, state);
                    if let Some(Output::Code(parameters, type_)) = &rhs {
                        if let Expr::Array(lhs) = lhs.as_ref() {
                            // annotate parameters with the functions' signature if it is available
                            if let Some(parameters) = parameters {
                                process_parameters(lhs, parameters, state)
                            }
                        }
                        let _ = infer_binary(
                            infer_type(lhs, state).map(|x| x.type_()),
                            op,
                            rhs.as_ref().map(|x| x.type_()),
                            expr.span(),
                            &mut state.errors,
                        )
                        .map(|(_, explanation)| {
                            state.explanations.insert(op.span, explanation);
                        });
                        return type_.map(Output::Type);
                    }
                }

                infer_binary(
                    infer_type(lhs, state).map(|x| x.type_()),
                    op,
                    infer_type(rhs, state).map(|x| x.type_()),
                    expr.span(),
                    &mut state.errors,
                )
                .map(|(type_, explanation)| {
                    state.explanations.insert(op.span, explanation);
                    type_.into()
                })
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
            infer_unary(op, rhs_type, &mut state.errors).map(|(type_, explanation)| {
                state.explanations.insert(op.span, explanation);
                type_.into()
            })
        }
        Expr::Nil(_) => None,
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Stack {
    pub variables: HashMap<Arc<str>, (Span, Option<Output>)>,
    // None => unknown signature
    pub signature: Option<Vec<Parameter>>,
    pub return_type: Option<Type>,
}

pub type MissionNamespace = HashMap<Arc<str>, (Origin, Option<Output>)>;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Namespace {
    pub stack: Vec<Stack>,
    pub mission: MissionNamespace,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Output {
    Type(Type),
    Code(Option<Vec<Parameter>>, Option<Type>),
}

impl Output {
    pub fn type_(&self) -> Type {
        match self {
            Output::Type(t) => *t,
            Output::Code(_, _) => Type::Code,
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
    pub fn insert(&mut self, key: Spanned<Arc<str>>, value: Option<Output>, is_private: bool) {
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
                .insert(key.inner.clone(), (Origin::File(key.span), value));
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
    External(Arc<str>, Option<Span>),
}

pub type Types = HashMap<Span, Option<Type>>;
pub type Parameters = HashMap<Span, Arc<str>>;

#[derive(Debug, Default, PartialEq)]
pub struct State {
    pub types: Types,
    pub parameters: Parameters,
    pub namespace: Namespace,
    pub origins: HashMap<Span, Origin>,
    pub errors: Vec<Error>,
    pub explanations: HashMap<Span, &'static str>,
}

impl State {
    pub fn signature(&self) -> Option<&Vec<Parameter>> {
        self.namespace.stack.last().unwrap().signature.as_ref()
    }

    pub fn return_type(&self) -> Option<Type> {
        self.namespace.stack.last().unwrap().return_type
    }

    /// Returns the set of all globals established by this state, assuming a function_name
    pub fn globals(&self, function_name: Arc<str>) -> HashMap<Arc<str>, (Origin, Option<Output>)> {
        let globals = &self.namespace.mission;
        let signature = self.signature();
        let return_type = self.return_type();
        //signature
        globals
            .iter()
            .filter_map(|(variable_name, (origin, output))| {
                if let Origin::File(span) = origin {
                    Some((
                        variable_name.clone(),
                        (
                            Origin::External(function_name.clone(), Some(*span)),
                            output.clone(),
                        ),
                    ))
                } else {
                    None
                }
            })
            .chain(signature.into_iter().map(|x| {
                (
                    function_name.clone(),
                    (
                        Origin::External(function_name.clone(), None),
                        Some(Output::Code(Some(x.clone()), return_type)),
                    ),
                )
            }))
            .collect()
    }
}

#[derive(Debug)]
pub struct Configuration {
    pub directory: PathBuf,
}

pub fn analyze(program: &[Expr], state: &mut State) {
    state.namespace.push_stack();
    let output = infer_expressions(program, state);
    state.namespace.stack.last_mut().unwrap().return_type = output.map(|x| x.type_());
}
