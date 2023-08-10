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
    Version(String),
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

pub type Functions = HashMap<Arc<str>, Spanned<PathBuf>>;

impl State {
    fn functions(&self, cpp_path: PathBuf) -> Functions {
        let mut r = HashMap::default();
        for namespace in &self.namespaces {
            if namespace.len() != 4 {
                continue;
            }

            if &namespace[0].inner != "CfgFunctions" {
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

            let name = format!("{tag}_fnc_{function_name}");
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
            r.insert(
                name.into(),
                path.map(|x| preprocessor::build_path(cpp_path.clone(), x.as_str())),
            );
        }
        r
    }
}

pub fn analyze(iter: AstIterator) -> (Functions, Vec<Error>) {
    let path = iter.state.path.clone();
    let (mut expr, mut errors) = parse(iter);

    let mut state = State::default();
    state.namespaces.push(Arc::new([]));

    while !expr.is_empty() {
        process_code(&mut expr, &mut state, &mut errors);
    }

    (state.functions(path), errors)
}

fn to_value(expr: Expr, errors: &mut Vec<Error>) -> Option<Spanned<Value>> {
    match expr {
        Expr::Number(number) => Some(number.map(Value::Number)),
        Expr::String(string) => Some(string.map(|x| Value::String(x.to_string()))),
        Expr::Code(expr) => Some(expr.map(|x| {
            Value::Array(
                x.into_iter()
                    .filter(|e| !matches!(e, Expr::Token(_))) // todo: improve to parse "," correctly
                    .filter_map(|expr| to_value(expr, errors))
                    .collect(),
            )
        })),
        _ => {
            errors.push(Error {
                inner: "Unexpected token".to_string(),
                span: expr.span(),
            });
            None
        }
    }
}

fn _process_v<A: std::fmt::Display>(
    expr: &mut VecDeque<Expr>,
    v: A,
    span: Span,
) -> Option<Spanned<Value>> {
    if let Some(Expr::Token(a)) = expr.front() {
        if matches(Some(a), ".") {
            expr.pop_front();
            if let Some(Expr::Number(a)) = expr.pop_front() {
                let value = Spanned::new(
                    Value::Version(format!("{v:.1}.{}", a.inner)),
                    (span.0, a.span.1),
                );
                return Some(process_version(expr, value));
            }
        }
    } else if let Some(Expr::Number(_)) = expr.front() {
        if let Some(Expr::Number(a)) = expr.pop_front() {
            let value = Spanned::new(
                Value::Version(format!("{v:.1}.{}", a.inner)),
                (span.0, a.span.1),
            );
            return Some(process_version(expr, value));
        }
    }
    None
}

fn process_version(expr: &mut VecDeque<Expr>, value: Spanned<Value>) -> Spanned<Value> {
    if let Spanned {
        inner: Value::Number(number),
        span,
    } = value
    {
        return _process_v(expr, number, span).unwrap_or(value);
    } else if let Spanned {
        inner: Value::Version(number),
        span,
    } = &value
    {
        return _process_v(expr, number, *span).unwrap_or(value);
    }
    value
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
        let Some(Expr::Token(mut name)) = name else {
            errors.push(Error {
                inner: "class requires a name".to_string(),
                span: first.span,
            });
            return
        };
        let body = expr.pop_front();
        let mut body = match body {
            Some(Expr::Code(body)) => body,
            Some(Expr::Token(other)) => {
                if matches(Some(&other), ":") {
                    let Some(Expr::Token(n)) = expr.pop_front() else {
                        errors.push(Error {
                            inner: "class subclass requires a name".to_string(),
                            span: first.span,
                        });
                        return;
                    };
                    name = n;
                    let body = expr.pop_front();
                    match body {
                        Some(Expr::Code(body)) => body,
                        _ => {
                            errors.push(Error {
                                inner: "class requires a body".to_string(),
                                span: first.span,
                            });
                            return;
                        }
                    }
                } else {
                    errors.push(Error {
                        inner: "class requires a body".to_string(),
                        span: first.span,
                    });
                    return;
                }
            }
            _ => {
                errors.push(Error {
                    inner: "class requires a body".to_string(),
                    span: first.span,
                });
                return;
            }
        };

        state.namespace.push(name.map(|x| x.to_string()));
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

        let Some(mut value) = to_value(value, errors) else {
            return
        };

        value = process_version(expr, value);

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
pub fn analyze_addon(mut directory: PathBuf) -> Result<(Functions, Vec<Error>), String> {
    directory.push("config.cpp");
    analyze_file(directory)
}

/// Given a directory, it tries to open the file "config.cpp" and
/// retrieve the list of function names and corresponding paths in the addon
pub fn analyze_file(path: PathBuf) -> Result<(Functions, Vec<Error>), String> {
    let Ok(content) = std::fs::read_to_string(path.clone()) else {
        return Err(format!("File \"{path:?}\" not found"))
    };

    // it is an addon, parse the config.cpp to fetch list of functions

    let iter = preprocessor::tokens(&content, Default::default(), path).map_err(|e| e.1.inner)?;
    Ok(analyze(iter))
}
