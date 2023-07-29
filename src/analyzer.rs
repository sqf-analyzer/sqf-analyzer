use std::collections::hash_map::Entry;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::PathBuf;

use crate::database::SIGNATURES;
use crate::parser::parse;
use crate::types::*;

/// infers the type of a bynary expression by considering all possible options
fn infer_unary(name: &str, rhs: Option<Type>) -> Option<Type> {
    let data = SIGNATURES
        .iter()
        .filter_map(|x| {
            if let Signature::Unary(name, rhs, type_) = x {
                Some(((*name, *rhs), *type_))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();

    match rhs {
        None => {
            let options = data
                .into_iter()
                .filter_map(|((x, _), type_)| (x == name).then_some(type_))
                .collect::<HashSet<_>>();
            if options.len() == 1 {
                Some(*options.iter().next().unwrap())
            } else {
                None
            }
        }
        // todo: if there are functions defined, this should use them
        Some(rhs) => data.get(&(name, rhs)).cloned(),
    }
}

/// infers the type of a bynary expression by considering all possible options
fn infer_binary(lhs: Option<Type>, name: &str, rhs: Option<Type>) -> Option<Type> {
    let data = SIGNATURES
        .iter()
        .filter_map(|x| {
            if let Signature::Binary(lhs, name, rhs, type_) = x {
                Some(((*lhs, *name, *rhs), *type_))
            } else {
                None
            }
        })
        .collect::<HashMap<_, _>>();

    match (lhs, rhs) {
        (None, None) => {
            let options = data
                .into_iter()
                .filter_map(|((_, x, _), type_)| (x == name).then_some(type_))
                .collect::<HashSet<_>>();
            if options.len() == 1 {
                Some(*options.iter().next().unwrap())
            } else {
                None
            }
        }
        (None, Some(rhs)) => {
            let options = data
                .into_iter()
                .filter_map(|((_, x, x_rhs), type_)| (x == name && x_rhs == rhs).then_some(type_))
                .collect::<HashSet<_>>();
            if options.len() == 1 {
                Some(*options.iter().next().unwrap())
            } else {
                None
            }
        }
        (Some(lhs), None) => {
            let options = data
                .into_iter()
                .filter_map(|((x_lhs, x, _), type_)| (x == name && x_lhs == lhs).then_some(type_))
                .collect::<HashSet<_>>();
            if options.len() == 1 {
                Some(*options.iter().next().unwrap())
            } else {
                None
            }
        }
        // todo: if there are functions defined, this should use them
        (Some(lhs), Some(rhs)) => data.get(&(lhs, name, rhs)).cloned(),
    }
}

fn infer_code(code: Vec<Span<Expr>>, state: &mut State) {
    state.namespace.push_stack();
    for expr in code {
        infer_type(expr.inner, state);
    }
    state.namespace.pop_stack();
}

fn infer_type(expr: Expr, state: &mut State) -> Option<Type> {
    match expr {
        Expr::Value(value) => Some(match value {
            Value::Number(_) => Type::Number,
            Value::String(_) => Type::String,
            Value::Array(_) => Type::Array,
            Value::Boolean(_) => Type::Boolean,
            Value::Code(code) => {
                infer_code(code, state);
                Type::Code
            }
        }),

        Expr::Variable(variable) => {
            let name = &variable.inner;

            let nullary = SIGNATURES
                .iter()
                .filter_map(|x| {
                    if let Signature::Nullary(name, type_) = x {
                        Some((*name, *type_))
                    } else {
                        None
                    }
                })
                .find_map(|(x, type_)| (x == name).then_some(type_));

            nullary.or_else(|| {
                state
                    .namespace
                    .get(&variable.inner)
                    .cloned()
                    // todo: variable must be defined...
                    .unwrap_or_default()
            })
        }
        Expr::BinaryOp { lhs, op, rhs } => {
            let name = op.as_str();
            let lhs_type = infer_type(lhs.inner, state);
            let rhs_type = infer_type(rhs.inner, state);
            infer_binary(lhs_type, name, rhs_type)
        }
        Expr::UnaryOp { op, rhs } => {
            let name = op.as_str();
            let rhs_type = infer_type(rhs.inner, state);
            infer_unary(name, rhs_type)
        }
        Expr::Assignment {
            is_private,
            variable,
            expr,
        } => {
            let type_ = infer_type(expr.inner, state);
            state.types.insert(variable.clone(), type_);
            state.namespace.insert(variable.inner, type_, is_private);
            type_
        }
        Expr::For { variable, do_, .. } => {
            state.types.insert(variable.clone(), Some(Type::Number));
            state.namespace.push_stack();
            state
                .namespace
                .insert(variable.inner, Some(Type::Number), true);
            for expr in do_ {
                infer_type(expr.inner, state);
            }
            state.namespace.pop_stack();
            Some(Type::ForType)
        }
        Expr::Macro(name, argument) => {
            let a = state.defines.get(&name.inner).expect("defined macro");
            let _compiled = a
                .body
                .as_ref()
                .unwrap()
                .replace(&a.arguments[0], &argument.inner);
            // todo: parse and process "compiled"
            None
        }
        Expr::Include(name) => {
            // copy all expressions and process them
            let a = state.files.get(&name).unwrap().clone();
            for expr in a {
                infer_type(expr.inner, state);
            }
            Some(Type::Nothing)
        }
        Expr::Define(define) => {
            state.defines.insert(define.name.clone(), define);
            Some(Type::Nothing)
        }
    }
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Namespace {
    pub stack: Vec<HashMap<String, Option<Type>>>,
    pub mission: HashMap<String, Option<Type>>,
}

impl Namespace {
    pub fn insert(&mut self, key: String, value: Option<Type>, is_private: bool) {
        if is_private {
            self.stack.last_mut().unwrap().insert(key, value);
        } else {
            self.mission.insert(key, value);
        }
    }

    pub fn push_stack(&mut self) {
        self.stack.push(Default::default());
    }

    pub fn pop_stack(&mut self) {
        self.stack.pop();
    }

    pub fn get(&self, key: &str) -> Option<&Option<Type>> {
        self.stack
            .last()
            .unwrap()
            .get(key)
            .or_else(|| self.mission.get(key))
    }
}

#[derive(Debug, Default)]
struct State {
    pub types: HashMap<Span<String>, Option<Type>>,
    pub namespace: Namespace,
    pub files: HashMap<String, Vec<Span<Expr>>>,
    pub defines: HashMap<String, Define>,
}

#[derive(Debug)]
pub struct Configuration {
    pub directory: PathBuf,
}

pub fn analyze(
    program: Vec<Span<Expr>>,
    directory: PathBuf,
) -> HashMap<Span<String>, Option<Type>> {
    let mut state = State::default();

    for expr in program.iter() {
        if let Expr::Include(name) = &expr.inner {
            match state.files.entry(name.clone()) {
                Entry::Occupied(_) => {}
                Entry::Vacant(v) => {
                    println!("{:?}", directory.join(name));
                    let data = fs::read_to_string(directory.join(name)).unwrap();
                    v.insert(parse(&data));
                }
            }
        }
    }
    //let mut configuration = Configuration { directory };
    state.namespace.push_stack();
    for expr in program {
        infer_type(expr.inner, &mut state);
    }
    state.types
}
