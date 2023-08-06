use std::collections::{HashMap, VecDeque};
use std::iter::Peekable;
use std::path::PathBuf;
use std::sync::Arc;

use crate::preprocessor;
use crate::{
    error::Error,
    preprocessor::{AstIterator, SpannedRef},
    span::{Span, Spanned},
};

#[derive(Clone, Debug)]
pub enum Expr<'a> {
    Number(Spanned<f32>),
    String(SpannedRef<'a>),
    Token(SpannedRef<'a>),
    Code(Spanned<VecDeque<Expr<'a>>>),
    Array(Spanned<VecDeque<Expr<'a>>>),
    Nil(Span),
}

impl Expr<'_> {
    fn span(&self) -> Span {
        match self {
            Expr::Token(o) | Expr::String(o) => o.span,
            Expr::Array(o) | Expr::Code(o) => o.span,
            Expr::Number(o) => o.span,
            Expr::Nil(s) => *s,
        }
    }
}

fn code<'a, I: Iterator<Item = SpannedRef<'a>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> VecDeque<Expr<'a>> {
    let mut expressions = Default::default();
    if matches(iter.peek(), "}") {
        return expressions;
    };

    while iter.peek().is_some() {
        let expression = expr(iter, errors);
        expressions.push_back(expression);

        if matches(iter.peek(), ";") {
            iter.next().unwrap();
        }
        if matches(iter.peek(), "}") {
            break;
        }
    }
    expressions
}

fn expr<'a, I: Iterator<Item = SpannedRef<'a>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> Expr<'a> {
    match iter.next() {
        None => {
            errors.push(Error {
                inner: "Un-expected end of file".to_string(),
                span: (0, 0),
            });
            return Expr::Nil((0, 0));
        }
        Some(Spanned { inner, span }) => match inner.as_ref() {
            ";" => Expr::Code(Spanned {
                inner: Default::default(),
                span,
            }),
            "{" => {
                let expr = code(iter, errors);
                let last = iter.next();
                if !matches(last.as_ref(), "}") {
                    errors.push(Error {
                        inner: "\"{\" is not closed".to_string(),
                        span,
                    })
                }
                let start = span.0;
                let end = last.map(|x| x.span.1).unwrap_or(start);

                Expr::Code(Spanned {
                    inner: expr,
                    span: (start, end),
                })
            }
            "[" => {
                let expr = array(iter, errors);
                let last = iter.next();
                if !matches(last.as_ref(), "]") {
                    errors.push(Error {
                        inner: "\"[\" is not closed".to_string(),
                        span,
                    })
                }
                let start = span.0;
                let end = last.map(|x| x.span.1).unwrap_or(start);

                Expr::Array(Spanned {
                    inner: expr,
                    span: (start, end),
                })
            }
            _ => token_to_expr(Spanned { inner, span }),
        },
    }
}

fn parse(mut iter: AstIterator) -> (VecDeque<Expr>, Vec<Error>) {
    let mut peekable = iter.by_ref().peekable();
    let mut errors = vec![];
    let expr = code(&mut peekable, &mut errors);
    errors.extend(iter.state.errors);
    (expr, errors)
}

fn array<'a, I: Iterator<Item = SpannedRef<'a>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> VecDeque<Expr<'a>> {
    let mut expressions = Default::default();
    if matches(iter.peek(), "]") {
        return expressions;
    };
    while iter.peek().is_some() {
        let expression = expr(iter, errors);
        expressions.push_back(expression);

        if matches(iter.peek(), ",") {
            iter.next().unwrap();
        }
        if matches(iter.peek(), "]") {
            break;
        }
    }
    expressions
}

#[inline]
fn matches(token: Option<&SpannedRef>, v: &str) -> bool {
    if let Some(Spanned { inner, .. }) = token {
        *inner == v
    } else {
        false
    }
}

fn token_to_expr(token: SpannedRef) -> Expr {
    let bytes = token.inner.as_bytes();
    if bytes.first() == Some(&b'"') && bytes.last() == Some(&b'"') {
        Expr::String(
            token
                .as_ref()
                .map(|x| x.get(1..x.len() - 1).unwrap().to_owned().into()),
        )
    } else {
        token
            .clone()
            .inner
            .parse::<f32>()
            .map(|x| Spanned {
                inner: x,
                span: token.span,
            })
            .map(Expr::Number)
            .ok()
            .unwrap_or(Expr::Token(token))
    }
}

#[derive(Debug, Clone)]
enum Value {
    Number(f32),
    String(String),
    Array(String),
}

#[derive(Debug, Default, Clone)]
pub struct State {
    namespace: Vec<String>,
    namespaces: Vec<Arc<[String]>>,
    assignments: HashMap<Arc<[String]>, HashMap<String, Value>>,
}

pub type Functions = HashMap<String, String>;

impl State {
    fn functions(&self) -> Functions {
        let mut r = HashMap::default();
        for namespace in &self.namespaces {
            if namespace.len() != 4 {
                continue;
            }
            let tag = self
                .assignments
                .get(&namespace[..2])
                .and_then(|x| {
                    x.get("tag").and_then(|x| {
                        if let Value::String(tag) = x {
                            Some(tag)
                        } else {
                            None
                        }
                    })
                })
                .unwrap_or(&namespace[1]);

            let category = &namespace[2];
            let function_name = &namespace[3];

            let maybe_path = self.assignments.get(namespace).and_then(|x| x.get("file"));

            let name = format!("{tag}_fn_{function_name}");
            let path = if let Some(Value::String(path)) = maybe_path {
                path.clone()
            } else {
                let maybe_category_file = self
                    .assignments
                    .get(&namespace[..3])
                    .and_then(|x| x.get("file"));
                if let Some(Value::String(path)) = maybe_category_file {
                    format!("{path}\\fn_{function_name}.sqf")
                } else {
                    format!("{category}\\fn_{function_name}.sqf")
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

    while !expr.is_empty() {
        process_code(&mut expr, &mut state, &mut errors);
    }

    (state.functions(), errors)
}

fn process_code(expr: &mut VecDeque<Expr>, state: &mut State, errors: &mut Vec<Error>) {
    let first = expr.pop_front();

    let Some(first) = first else  {
        return
    };

    let Expr::Token(first) = first else {
        errors.push(Error {
            inner: "body expects a token".to_string(),
            span: first.span(),
        });
        return;
    };

    if first.inner == "class" {
        let name = expr.pop_front();
        let body = expr.pop_front();
        let Some(Expr::Token(name)) = name else {
            errors.push(Error {
                inner: format!("class requires a name but \"{:?}\" is not one", name),
                span: first.span,
            });
            return
        };
        let Some(Expr::Code(mut body)) = body else {
            errors.push(Error {
                inner: "class requires a body".to_string(),
                span: first.span,
            });
            return
        };
        state.namespace.push(name.inner.to_string());
        state.namespaces.push(state.namespace.clone().into());
        while !body.inner.is_empty() {
            process_code(&mut body.inner, state, errors);
        }
        state.namespace.pop();
    } else {
        // assign
        let name = first;
        let array_or_eq = expr.pop_front();

        if let Some(Expr::Array(_)) = array_or_eq {
            expr.pop_front(); // "="
        };

        let Some(value) = expr.pop_front() else {
            errors.push(Error {
                inner: "assignment requires a right side".to_string(),
                span: name.span,
            });
            return
        };

        let value = match value {
            Expr::Number(number) => Value::Number(number.inner),
            Expr::String(string) => Value::String(string.inner.to_string()),
            Expr::Code(expr) => Value::Array(
                expr.inner
                    .into_iter()
                    .filter_map(|expr| {
                        let Expr::String(string) = expr else {
                        errors.push(Error {
                            inner: "arrays must be composed of strings".to_string(),
                            span: name.span,
                        });
                        return None;
                    };
                        Some(string.inner.to_string())
                    })
                    .collect(),
            ),
            other => {
                errors.push(Error {
                    inner: format!("assignment requires a right side: {:?}", other),
                    span: name.span,
                });
                return;
            }
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

/// Given a directory, it tries to open the file "config.cpp" and
/// retrieve the list of function names and corresponding paths in the addon
pub fn analyze_addon(mut directory: PathBuf) -> (Functions, Vec<Error>) {
    directory.push("config.cpp");

    let Ok(content) = std::fs::read_to_string(directory.clone()) else {
        return (Default::default(), vec![Error {
            span: (0, 0),
            inner: format!("File \"{directory:?}\" not found"),
        }])
    };

    // it is an addon, parse the config.cpp to fetch list of functions

    let iter = preprocessor::tokens(&content, Default::default(), Default::default()).unwrap();
    analyze(iter)
}
