use std::collections::hash_map::HashMap;
use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use uncased::UncasedStr;

use crate::error::{Error, ErrorType};
use crate::parser::Expr;
use crate::span::{Span, Spanned};
use crate::{types::*, uncased};

mod database;
mod operators;
mod type_operations;
mod unary;
pub use database::*;
use type_operations::*;

#[derive(Debug, Clone, PartialEq)]
pub struct Configuration {
    /// Path of the file
    pub file_path: Arc<Path>,
    /// Path of the mission or addon
    pub base_path: PathBuf,
    /// path to the addons (e.g. x/cba -> /../cba)
    pub addons: HashMap<Arc<str>, PathBuf>,
}

impl Default for Configuration {
    fn default() -> Self {
        Self {
            file_path: PathBuf::default().into(),
            base_path: Default::default(),
            addons: Default::default(),
        }
    }
}

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
    v: &Spanned<Arc<str>>,
    state: &mut State,
    type_: Type,
    has_default: bool,
    add_to_signature: bool,
) {
    if !_is_private(&v.inner) {
        state
            .errors
            .push(Error::new(ErrorType::GlobalVariableParam, v.span));
        return;
    }

    if add_to_signature {
        let p = Parameter {
            name: v.inner.clone(),
            type_,
            has_default,
        };
        if let Some(s) = state.namespace.stack.last_mut().unwrap().signature.as_mut() {
            s.push(p)
        } else {
            state.namespace.stack.last_mut().unwrap().signature = Some(vec![p])
        };
    }

    state.namespace.insert(
        uncased(v.inner.as_ref()),
        Some(type_.into()),
        (v.span, &state.configuration).into(),
        true,
    );
}

fn process_param_types(types: &Expr, state: &mut State) -> Option<Type> {
    let (types, span) = if let Expr::Array(expr) = &types {
        (expr, expr.span)
    } else {
        state.errors.push(Error::new(
            ErrorType::ExpectedType(Type::Array),
            types.span(),
        ));
        return None;
    };

    Some(types.inner.iter().fold(Type::Nothing, |acc, maybe_type| {
        // reduce the list of types to a single type
        let type_ = infer_type(maybe_type, state).map(|x| x.type_());
        let type_ = type_.unwrap_or_else(|| {
            state.errors.push(Error::new(
                "params' third argument's elements must be typed".to_string(),
                span,
            ));
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

fn process_params_variable(
    param: &Expr,
    state: &mut State,
    add_to_signature: bool,
) -> Option<Spanned<Arc<UncasedStr>>> {
    if let Expr::String(v) = &param {
        process_param_variable(v, state, Type::Anything, false, add_to_signature);
        return Some(v.as_ref().map(|x| uncased(x.as_ref())));
    }

    let Expr::Array(expr) = param else {
        state.errors.push(Error::new(
            "params' argument must be either a string or array".to_string(),
            param.span(),
        ));
        return None;
    };

    let mut iter = expr.inner.iter();
    let name = iter.next();
    let default = iter.next();
    let types = iter.next();
    let _counts = iter.next();
    for remaining in iter {
        state.errors.push(Error::new(
            "params' arguments only accept up to 4 arguments".to_string(),
            remaining.span(),
        ))
    }

    let name = if let Some(Expr::String(name)) = name {
        name
    } else {
        state.errors.push(Error::new(
            ErrorType::ExpectedType(Type::String),
            param.span(),
        ));
        return None;
    };

    let Some(default) = default else {
        process_param_variable(name, state, Type::Anything, false, add_to_signature);
        return Some(name.as_ref().map(|x| uncased(x.as_ref())));
    };

    let Some(types) = types else {
        process_param_variable(name, state, Type::Anything, true, add_to_signature);
        return Some(name.as_ref().map(|x| uncased(x.as_ref())));
    };

    let Some(type_) = process_param_types(types, state) else {
        return Some(name.as_ref().map(|x| uncased(x.as_ref())));
    };

    let default_type = infer_type(default, state)
        .map(|x| x.type_())
        .unwrap_or(Type::Anything);

    if !type_.consistent(default_type) {
        state.errors.push(Error::new(
            ErrorType::IncompatibleParamArgument(default_type, type_),
            param.span(),
        ));
    }

    process_param_variable(name, state, type_, true, add_to_signature);
    Some(name.as_ref().map(|x| uncased(x.as_ref())))
}

/// infers the type of a bynary expression by considering all possible options
fn infer_unary(
    name: &Spanned<Arc<str>>,
    rhs: Option<Type>,
    errors: &mut Vec<Error>,
) -> Option<InnerType> {
    let Some(options) = UNARY.get(&UncasedStr::new(&name.inner)) else {
        errors.push(Error::new(
            format!("No unary operator named \"{}\"", name.inner),
            name.span,
        ));
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
                .filter_map(|(x_rhs, type_)| x_rhs.consistent(rhs).then_some(type_));

            if let Some(first) = take_only(options) {
                Some(first)
            } else if rhs == Type::Anything {
                None
            } else {
                errors.push(Error::new(
                    ErrorType::InvalidType(name.inner.clone(), rhs),
                    name.span,
                ));
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
    errors: &mut Vec<Error>,
) -> Option<InnerType> {
    let Some(options) = BINARY.get(&UncasedStr::new(name.inner.as_ref())) else {
        errors.push(Error::new(
            format!("\"{}\" is not a binary operator", name.inner),
            span,
        ));
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
                errors.push(Error::new(
                    format!(
                        "\"{}\" does not support a rhs of type \"{:?}\"",
                        name.inner, rhs
                    ),
                    name.span,
                ));
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
                    errors.push(Error::new(
                        format!(
                            "\"{}\" does not support a left side of type \"{:?}\"",
                            name.inner, lhs
                        ),
                        name.span,
                    ));
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
                errors.push(Error::new(
                    format!(
                        "\"{}\" does not support left side of type \"{:?}\" and right side of type \"{:?}\"",
                        name.inner, lhs, rhs
                    ),
                    name.span,
                ));
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
        if !inner.as_ref().eq_ignore_ascii_case("private") {
            state.errors.push(Error::new(
                "left side of assignment must be \"private\"".to_string(),
                *span,
            ));
            return;
        }
        state.explanations.insert(
            *span,
            UNARY
                .get(&UncasedStr::new("private"))
                .unwrap()
                .values()
                .next()
                .unwrap()
                .first()
                .unwrap()
                .1,
        );

        let Expr::Variable(variable) = variable.as_ref() else {
            state.errors.push(Error::new(
                "assigment can only have variables in left side".to_string(),
                variable.span(),
            ));
            return;
        };
        (true, variable)
    } else {
        // a = ...
        let Expr::Variable(variable) = lhs else {
            state.errors.push(Error::new(
                "assigment can only have variables in left side".to_string(),
                lhs.span(),
            ));
            return;
        };
        (false, variable)
    };
    let variable = variable.as_ref().map(|x| uncased(x.as_ref()));

    state
        .types
        .insert(variable.span, rhs_type.as_ref().map(|x| x.type_()));

    let is_internal = variable.inner.starts_with("_");
    let span = variable.span;

    let in_mission = state.namespace.insert(
        variable.inner,
        rhs_type,
        (variable.span, &state.configuration).into(),
        is_private,
    );

    if in_mission && is_internal {
        state
            .errors
            .push(Error::new(ErrorType::PrivateAssignedToMission, span))
    }
}

fn process_parameters(lhs: &Spanned<Vec<Expr>>, parameters: &[Parameter], state: &mut State) {
    if lhs.inner.len() > parameters.len() {
        state.errors.push(Error::new(
            ErrorType::InsufficientArguments {
                expected: parameters.len(),
                passed: lhs.inner.len(),
            },
            lhs.span,
        ))
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

            state.errors.extend(
                state
                    .namespace
                    .unused()
                    .map(|span| Error::new(ErrorType::UnusedVariable, span)),
            );

            state.namespace.pop_stack();
            Some(Output::Code(parameters, return_type.map(|x| x.type_())))
        }
        Expr::Nullary(variable) => NULLARY
            .get(&UncasedStr::new(variable.inner.as_ref()))
            .cloned()
            .map(|(type_, explanation)| {
                state.explanations.insert(variable.span, explanation);
                type_.into()
            }),
        Expr::Variable(variable) => {
            let name = UncasedStr::new(&variable.inner);

            (name == UncasedStr::new("_this"))
                .then_some(Type::Anything.into())
                .or_else(|| {
                    if let Some((origin, type_)) = state.namespace.get(&variable.inner) {
                        state.origins.insert(variable.span, origin);
                        type_
                    } else {
                        if !UncasedStr::new(variable.inner.as_ref()).starts_with("bis_") {
                            state.errors.push(Error::new(
                                ErrorType::UndefinedVariable(variable.inner.clone()),
                                variable.span,
                            ));
                        };
                        None
                    }
                })
        }
        Expr::Binary(lhs, op, rhs) => {
            if op.inner.as_ref() == "=" {
                infer_assign(lhs, rhs, state);
                Some(Type::Nothing.into())
            } else if op.inner.as_ref().eq_ignore_ascii_case("execVM") {
                operators::exec_vm(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("then") {
                operators::then(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("foreach") {
                operators::foreach(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("exitWith") {
                operators::exit_with(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("else") {
                operators::else_(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("remoteexec") {
                operators::remoteexec(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("spawn") {
                operators::spawn(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("params") {
                operators::params(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("count") {
                operators::count(expr.span(), lhs, op, rhs, state)
            } else if op.inner.as_ref().eq_ignore_ascii_case("select")
                || op.inner.as_ref().eq_ignore_ascii_case("apply")
                || op.inner.as_ref().eq_ignore_ascii_case("findIf")
            {
                operators::select(expr.span(), lhs, op, rhs, state)
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
            let rhs_type = infer_type(rhs, state);
            let output = if op.inner.as_ref().eq_ignore_ascii_case("for") {
                if let Expr::String(x) = rhs.as_ref() {
                    state.types.insert(x.span, Some(Type::Number));
                    state.namespace.insert(
                        uncased(x.inner.as_ref()),
                        Some(Type::Number.into()),
                        (x.span, &state.configuration).into(),
                        true,
                    );
                }
                None
            } else if op.inner.as_ref().eq_ignore_ascii_case("params") {
                // store the names of the variables to build the function's signature
                if let Expr::Array(x) = rhs.as_ref() {
                    x.inner.iter().for_each(|x| {
                        process_params_variable(x, state, true);
                    })
                }
                None
            } else if op.inner.as_ref().eq_ignore_ascii_case("compile") {
                unary::compile(rhs, state);
                None
            } else if op.inner.as_ref().eq_ignore_ascii_case("call") {
                unary::call(&rhs_type)
            } else if op.inner.as_ref().eq_ignore_ascii_case("execVM") {
                unary::exec_vm(rhs, state);
                None
            } else if op.inner.as_ref().eq_ignore_ascii_case("isnil") {
                unary::is_nil(rhs, state);
                None
            } else if op.inner.as_ref().eq_ignore_ascii_case("compileScript") {
                unary::compile_script(rhs, state);
                None
            } else if op.inner.as_ref().eq_ignore_ascii_case("private") {
                if let Expr::Array(array) = rhs.as_ref() {
                    for entry in &array.inner {
                        if let Expr::String(name) = entry {
                            state.namespace.insert(
                                uncased(name.inner.as_ref()),
                                None,
                                (name.span, &state.configuration).into(),
                                true,
                            );
                        } else {
                            state.errors.push(Error::new(
                                ErrorType::ExpectedType(Type::String),
                                entry.span(),
                            ));
                        }
                    }
                } else if let Expr::String(name) = rhs.as_ref() {
                    state.namespace.insert(
                        uncased(name.inner.as_ref()),
                        None,
                        (name.span, &state.configuration).into(),
                        true,
                    );
                };
                None
            } else {
                None
            };
            infer_unary(op, rhs_type.map(|x| x.type_()), &mut state.errors).map(
                |(type_, explanation)| {
                    state.explanations.insert(op.span, explanation);
                    output.unwrap_or_else(|| type_.into())
                },
            )
        }
        Expr::Nil(_) => None,
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Stack {
    pub variables: HashMap<Arc<UncasedStr>, (Origin, Option<Output>)>,
    // None => unknown signature
    pub signature: Option<Vec<Parameter>>,
    pub return_type: Option<Type>,
}

pub type MissionNamespace = HashMap<Arc<UncasedStr>, (Origin, Option<Output>)>;

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Namespace {
    pub stack: Vec<Stack>,
    pub mission: MissionNamespace,
    read: HashSet<Arc<UncasedStr>>,
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
    /// Returns whether the variable was inserted to the mission namespace
    #[allow(clippy::map_entry)]
    pub fn insert(
        &mut self,
        key: Arc<UncasedStr>,
        value: Option<Output>,
        origin: Origin,
        is_private: bool,
    ) -> bool {
        if is_private {
            self.stack
                .last_mut()
                .unwrap()
                .variables
                .insert(key, (origin, value));
            false
        } else {
            for stack in self.stack.iter_mut().rev() {
                if stack.variables.contains_key(&key) {
                    // entries API would require cloning, which is more expensive than this lookup
                    stack.variables.insert(key, (origin, value));
                    return false;
                }
            }
            self.mission.insert(key, (origin, value));
            true
        }
    }

    pub fn push_stack(&mut self) {
        self.stack.push(Default::default());
    }

    pub fn pop_stack(&mut self) {
        self.stack.pop();
    }

    pub fn get(&mut self, key: &str) -> Option<(Origin, Option<Output>)> {
        let sqf_key = UncasedStr::new(key);
        for stack in self.stack.iter().rev() {
            if let Some((origin, a)) = stack.variables.get(sqf_key) {
                self.read.insert(uncased(key));
                return Some((origin.clone(), a.clone()));
            }
        }
        self.mission.get(sqf_key).cloned()
    }

    pub fn unused(&self) -> impl Iterator<Item = Span> + '_ {
        self.stack
            .last()
            .as_ref()
            .unwrap()
            .variables
            .iter()
            .filter(|(name, (_, _))| !self.read.contains(*name))
            .filter_map(|(_, (origin, _))| origin.1)
    }
}

#[derive(Debug, Clone, Eq, Hash, PartialEq)]
pub struct Origin(pub Arc<Path>, pub Option<Span>);

impl From<(Span, &Configuration)> for Origin {
    fn from(value: (Span, &Configuration)) -> Self {
        Self(value.1.file_path.clone(), Some(value.0))
    }
}

pub type Types = HashMap<Span, Option<Type>>;
pub type Parameters = HashMap<Span, Arc<str>>;

#[derive(Debug, Default, PartialEq, Clone)]
pub struct Settings {}

#[derive(Debug, Default, PartialEq)]
pub struct State {
    pub configuration: Configuration,
    pub settings: Settings,
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
    pub fn globals(
        &self,
        function_name: Option<Arc<UncasedStr>>,
    ) -> HashMap<Arc<UncasedStr>, (Origin, Option<Output>)> {
        let globals = &self.namespace.mission;
        let signature = self.signature();
        let return_type = self.return_type();
        //signature
        globals
            .iter()
            .map(|(variable_name, (origin, output))| {
                (variable_name.clone(), (origin.clone(), output.clone()))
            })
            .chain(function_name.into_iter().map(|name| {
                (
                    name,
                    (
                        Origin(self.configuration.file_path.clone(), None),
                        Some(Output::Code(signature.cloned(), return_type)),
                    ),
                )
            }))
            .collect()
    }

    /// Updates the return type based on the new type of the return by taking the union of the types
    fn update_return(&mut self, new_type: Option<Type>) {
        self.namespace.stack.last_mut().unwrap().return_type =
            union_types(new_type, self.namespace.stack.last().unwrap().return_type);
    }
}

pub fn analyze(program: &[Expr], state: &mut State) {
    state.namespace.push_stack();
    let output = infer_expressions(program, state);

    state.errors.extend(
        state
            .namespace
            .unused()
            .map(|span| Error::new(ErrorType::UnusedVariable, span)),
    );

    state.update_return(output.map(|x| x.type_()));
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_infer_binary() {
        let mut errors = vec![];
        infer_binary(
            Some(Type::Anything),
            &Spanned {
                inner: "aaa".into(),
                span: (0, 3),
            },
            Some(Type::Anything),
            (0, 4),
            &mut errors,
        );
        assert!(!errors.is_empty());
    }

    #[test]
    fn test_infer_binary_rhs_anything() {
        let mut errors = vec![];
        infer_binary(
            None,
            &Spanned {
                inner: "call".into(),
                span: (0, 3),
            },
            Some(Type::Anything),
            (0, 4),
            &mut errors,
        );
        assert!(errors.is_empty());
    }
}
