use std::collections::{HashMap, VecDeque};
use std::sync::Arc;

use crate::preprocessor::{self, Configuration};
use crate::{
    error::Error,
    preprocessor::AstIterator,
    span::{Span, Spanned},
    uncased,
};

mod parser;
use parser::*;
use uncased::UncasedStr;

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum Value {
    Number(f32),
    Boolean(bool),
    GameType(Arc<str>),
    String(String),
    Array(Vec<Spanned<Value>>),
}

type Assignments = HashMap<Arc<[Spanned<String>]>, HashMap<String, Spanned<Value>>>;

#[derive(Debug, Default, Clone)]
pub struct State {
    namespace: Vec<Spanned<String>>,
    namespaces: Vec<Arc<[Spanned<String>]>>,
    assignments: Assignments,
}

pub type Functions = HashMap<Arc<UncasedStr>, Spanned<String>>;

impl State {
    fn functions(&self) -> Functions {
        let mut r = HashMap::default();
        for namespace in &self.namespaces {
            if namespace.len() != 4 {
                continue;
            }

            if !namespace[0].inner.eq_ignore_ascii_case("CfgFunctions") {
                continue;
            }

            let tag = self
                .assignments
                .get(&namespace[..2])
                .and_then(|x| {
                    x.get("tag").and_then(|x| {
                        if let Value::String(tag) = &x.inner {
                            Some(tag)
                        } else {
                            None
                        }
                    })
                })
                .unwrap_or(&namespace[1].inner);

            let category = &namespace[2].inner;
            let function_name = &namespace[3].inner;
            let span = namespace[3].span;

            let maybe_path = self.assignments.get(namespace).and_then(|x| x.get("file"));

            let name = uncased(format!("{tag}_fnc_{function_name}").as_ref());
            let path = if let Some(Spanned {
                inner: Value::String(path),
                ..
            }) = maybe_path
            {
                Spanned {
                    inner: path.clone(),
                    span,
                }
            } else {
                let maybe_category_file = self
                    .assignments
                    .get(&namespace[..3])
                    .and_then(|x| x.get("file"));
                if let Some(Spanned {
                    inner: Value::String(path),
                    ..
                }) = maybe_category_file
                {
                    Spanned {
                        inner: format!("{path}\\fn_{function_name}.sqf"),
                        span,
                    }
                } else {
                    Spanned {
                        inner: format!("{category}\\fn_{function_name}.sqf"),
                        span,
                    }
                }
            };
            r.insert(name, path);
        }
        r
    }
}

pub fn analyze(iter: AstIterator) -> (Functions, Vec<Error>) {
    let (mut expr, mut errors) = parse(iter);

    let mut state = State::default();
    state.namespaces.push(Arc::new([]));

    process_code(&mut expr, &mut state, &mut errors);

    (state.functions(), errors)
}

fn to_value(expr: Expr, errors: &mut Vec<Error>) -> Option<Spanned<Value>> {
    match expr {
        Expr::Number(number) => Some(number.map(Value::Number)),
        Expr::Boolean(value) => Some(value.map(Value::Boolean)),
        Expr::String(string) => Some(string.map(|x| Value::String(x.to_string()))),
        Expr::GameType(string) => Some(string.map(Value::GameType)),
        Expr::Code(expr) => Some(expr.map(|x| {
            Value::Array(
                x.into_iter()
                    .filter(|e| !matches!(e, Expr::Token(_))) // todo: improve to parse "," correctly
                    .filter_map(|expr| to_value(expr, errors))
                    .collect(),
            )
        })),
        Expr::Token(a) => {
            for trial in [
                "safeZoneX",
                "safeZoneY",
                "safeZoneZ",
                "safeZoneH",
                "safeZoneW",
                "safeZoneXAbs",
                "safeZoneWAbs",
            ] {
                if a.inner.as_ref().eq_ignore_ascii_case(trial) {
                    return None;
                }
            }
            errors.push(Error::new(
                format!(
                    "Unexpected assignment to \"{}\" (undefined macro?)",
                    a.inner.as_ref()
                ),
                a.span,
            ));
            None
        }
        Expr::Expr(_) => None,
        _ => {
            errors.push(Error::new("Unexpected token".to_string(), expr.span()));
            None
        }
    }
}

fn process_body(
    name: Spanned<Arc<str>>,
    expr: &mut VecDeque<Expr>,
    state: &mut State,
    errors: &mut Vec<Error>,
) {
    if matches!(expr.front(), Some(Expr::Code(_))) {
        let Some(Expr::Code(mut body)) = expr.pop_front() else {
            unreachable!()
        };
        state.namespace.push(name.map(|x| x.to_string()));
        state.namespaces.push(state.namespace.clone().into());
        process_code(&mut body.inner, state, errors);
        state.namespace.pop();
    }
}

fn process_subclass(
    name: Spanned<Arc<str>>,
    expr: &mut VecDeque<Expr>,
    state: &mut State,
    errors: &mut Vec<Error>,
) {
    let Some(Expr::Token(colon)) = expr.front() else {
        process_body(name, expr, state, errors);
        return;
    };
    if colon.inner.as_ref() != ":" {
        process_body(name, expr, state, errors);
        return;
    }
    expr.pop_front();
    let Some(Expr::Token(name)) = expr.pop_front() else {
        errors.push(Error::new(
            "subclass requires a name".to_string(),
            name.span,
        ));
        return;
    };
    process_body(name, expr, state, errors)
}

fn update_token(token: &Expr, content: &mut String, span: &mut Option<Span>) {
    use std::fmt::Write;
    let mut update_span = |new: Span| {
        *span = Some(match span {
            None => new,
            Some((min, max)) => (*min, *max + (new.1 - new.0)),
        })
    };

    match token {
        Expr::Number(a) => {
            let _ = write!(content, "{}", a.inner);
            update_span(a.span);
        }
        Expr::Boolean(a) => {
            let _ = write!(content, "{}", a.inner);
            update_span(a.span);
        }
        Expr::Token(a) | Expr::String(a) | Expr::GameType(a) => {
            let _ = write!(content, "{}", a.inner);
            update_span(a.span);
        }
        Expr::Code(a) => {
            for token in &a.inner {
                update_token(token, content, span)
            }
        }
        Expr::Array(_) => {}
        Expr::Expr(_) => {}
    };
}

fn concatenate(tokens: &VecDeque<Expr>) -> Spanned<Value> {
    let mut content = String::new();
    let mut span = Option::<(usize, usize)>::None;

    for token in tokens {
        update_token(token, &mut content, &mut span)
    }
    Spanned::new(Value::String(content), span.unwrap_or_default())
}

fn process_expr(expr: &mut VecDeque<Expr>, state: &mut State, errors: &mut Vec<Error>) {
    let Some(first) = expr.pop_front() else {
        return;
    };

    if let Expr::Code(_) = first {
        return;
    };

    let Expr::Token(first) = first else {
        errors.push(Error::new("body expects a token".to_string(), first.span()));
        return;
    };

    if first.inner.as_ref() == "class" {
        let name = expr.pop_front();
        let Some(Expr::Token(name)) = name else {
            errors.push(Error::new("class requires a name".to_string(), first.span));
            return;
        };
        process_subclass(name, expr, state, errors);
    } else {
        // assign
        let name = first;
        let array_or_eq = expr.pop_front();

        if let Some(Expr::Array(_)) = array_or_eq {
            expr.pop_front(); // "="
        };

        // we need at least one
        if expr.is_empty() {
            errors.push(Error::new(
                "assignment requires a right side".to_string(),
                name.span,
            ));
            return;
        }
        let value = if expr.len() == 1 {
            let Some(value) = to_value(expr.pop_back().unwrap(), errors) else {
                return;
            };
            value
        } else {
            // more than one token, let's interpret it as a string for now
            concatenate(expr)
        };

        let lhs = state.namespaces.last().unwrap().clone();
        let key = name.inner.to_string();
        use std::collections::hash_map::Entry;
        match state.assignments.entry(lhs) {
            Entry::Occupied(mut inner) => {
                inner.get_mut().insert(key, value);
            }
            Entry::Vacant(inner) => {
                inner.insert(HashMap::from([(key, value)]));
            }
        };
    }
}

fn process_code(expressions: &mut VecDeque<Expr>, state: &mut State, errors: &mut Vec<Error>) {
    for expr in expressions {
        if let Expr::Expr(expr) = expr {
            process_expr(&mut expr.inner, state, errors)
        }
    }
}

/// Given a path to a config.cpp or equivalent (e.g. description.ext), it tries to open it and process all functions
/// and errors on it.
pub fn analyze_file(configuration: Configuration) -> Result<(Functions, Vec<Error>), String> {
    let Ok(content) = std::fs::read_to_string(&configuration.path) else {
        return Err(format!(
            "File \"{}\" not found",
            configuration.path.display()
        ));
    };

    let iter = preprocessor::tokens(&content, configuration).map_err(|e| e.1.type_.to_string())?;
    Ok(analyze(iter))
}
