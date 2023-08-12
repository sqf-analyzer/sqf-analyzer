use std::collections::{HashMap, VecDeque};
use std::iter::Peekable;
use std::path::PathBuf;
use std::sync::Arc;

use crate::preprocessor::{self, parse_hexadecimal};
use crate::{
    error::Error,
    preprocessor::AstIterator,
    span::{Span, Spanned},
};

#[derive(Clone, Debug)]
pub enum Expr {
    Number(Spanned<f32>),
    Boolean(Spanned<bool>),
    GameType(Spanned<Arc<str>>),
    String(Spanned<Arc<str>>),
    Token(Spanned<Arc<str>>),
    Code(Spanned<VecDeque<Expr>>),
    Array(Span),
    Expr(Spanned<VecDeque<Expr>>),
    Nil(Span),
}

impl Expr {
    fn span(&self) -> Span {
        match self {
            Expr::GameType(o) | Expr::Token(o) | Expr::String(o) => o.span,
            Expr::Code(o) => o.span,
            Expr::Number(o) => o.span,
            Expr::Boolean(o) => o.span,
            Expr::Array(s) | Expr::Nil(s) => *s,
            Expr::Expr(s) => s.span,
        }
    }
}

fn expression<I: Iterator<Item = Spanned<Arc<str>>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> VecDeque<Expr> {
    let mut expressions = Default::default();
    if matches(iter.peek(), ")") {
        return expressions;
    };

    while iter.peek().is_some() {
        let expression = expr(iter, errors);
        expressions.push_back(expression);

        if matches(iter.peek(), ")") {
            break;
        }
    }
    expressions
}

fn code<I: Iterator<Item = Spanned<Arc<str>>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> VecDeque<Expr> {
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

fn expr<I: Iterator<Item = Spanned<Arc<str>>>>(
    iter: &mut Peekable<I>,
    errors: &mut Vec<Error>,
) -> Expr {
    let Spanned { inner, span } = iter.next().expect("function invariant");
    match inner.as_ref() {
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
            let last = iter.next();
            if !matches(last.as_ref(), "]") {
                errors.push(Error {
                    inner: "\"[\" is not closed".to_string(),
                    span,
                })
            }
            let start = span.0;
            let end = last.map(|x| x.span.1).unwrap_or(start);

            Expr::Array((start, end))
        }
        "(" => {
            let expr = expression(iter, errors);
            let last = iter.next();
            if !matches(last.as_ref(), ")") {
                errors.push(Error {
                    inner: "\"(\" is not closed".to_string(),
                    span,
                })
            }
            let start = span.0;
            let end = last.map(|x| x.span.1).unwrap_or(start);

            Expr::Expr(Spanned {
                inner: expr,
                span: (start, end),
            })
        }
        _ => token_to_expr(Spanned { inner, span }),
    }
}

fn parse(mut iter: AstIterator) -> (VecDeque<Expr>, Vec<Error>) {
    let mut peekable = iter.by_ref().peekable();
    let mut errors = vec![];
    let expr = code(&mut peekable, &mut errors);
    errors.extend(iter.state.errors);
    (expr, errors)
}

#[inline]
fn matches(token: Option<&Spanned<Arc<str>>>, v: &str) -> bool {
    if let Some(Spanned { inner, .. }) = token {
        inner.as_ref() == v
    } else {
        false
    }
}

fn token_to_expr(token: Spanned<Arc<str>>) -> Expr {
    let bytes = token.inner.as_bytes();
    if bytes.first() == Some(&b'"') && bytes.last() == Some(&b'"') {
        Expr::String(
            token
                .as_ref()
                .map(|x| x.get(1..x.len() - 1).unwrap().to_owned().into()),
        )
    } else if bytes == b"true" {
        Expr::Boolean(token.map(|_| true))
    } else if bytes == b"false" {
        Expr::Boolean(token.map(|_| false))
    } else if bytes.eq_ignore_ascii_case(b"coop") {
        Expr::GameType(token)
    } else if let Ok(number) = token.inner.parse::<f32>() {
        Expr::Number(Spanned {
            inner: number,
            span: token.span,
        })
    } else if let Some(int) = parse_hexadecimal(&token.inner) {
        Expr::Number(Spanned {
            inner: int as f32,
            span: token.span,
        })
    } else {
        Expr::Token(token)
    }
}

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

fn to_value(expr: Expr, errors: &mut Vec<Error>, is_negative: bool) -> Option<Spanned<Value>> {
    match expr {
        Expr::Number(number) => Some(
            number
                .map(|x| x * (1.0 - 2.0 * is_negative as i32 as f32))
                .map(Value::Number),
        ),
        Expr::Boolean(value) => Some(value.map(Value::Boolean)),
        Expr::String(string) => Some(string.map(|x| Value::String(x.to_string()))),
        Expr::GameType(string) => Some(string.map(Value::GameType)),
        Expr::Code(expr) => Some(expr.map(|x| {
            Value::Array(
                x.into_iter()
                    .filter(|e| !matches!(e, Expr::Token(_))) // todo: improve to parse "," correctly
                    .filter_map(|expr| to_value(expr, errors, true))
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
            errors.push(Error {
                inner: format!(
                    "Unexpected assignment to \"{}\" (undefined macro?)",
                    a.inner.as_ref()
                ),
                span: a.span,
            });
            None
        }
        Expr::Expr(_) => None,
        _ => {
            errors.push(Error {
                inner: "Unexpected token".to_string(),
                span: expr.span(),
            });
            None
        }
    }
}

fn process_value(
    span: Span,
    expr: &mut VecDeque<Expr>,
    errors: &mut Vec<Error>,
) -> Option<Spanned<Value>> {
    let Some(mut value) = expr.pop_front() else {
        errors.push(Error {
            inner: "assignment requires a right side".to_string(),
            span,
        });
        return None
    };

    let mut is_negative = false;
    if let Expr::Token(token) = &value {
        if matches(Some(token), "-") {
            value = match expr.pop_front() {
                Some(e) => e,
                None => {
                    errors.push(Error {
                        inner: "assignment requires a right side".to_string(),
                        span,
                    });
                    return None;
                }
            };
            is_negative = true
        }
    };

    to_value(value, errors, is_negative)
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
        while !body.inner.is_empty() {
            process_code(&mut body.inner, state, errors);
        }
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
        return
    };
    if colon.inner.as_ref() != ":" {
        process_body(name, expr, state, errors);
        return;
    }
    expr.pop_front();
    let Some(Expr::Token(name)) = expr.pop_front() else {
        errors.push(Error {
            inner: "subclass requires a name".to_string(),
            span: name.span,
        });
        return
    };
    process_body(name, expr, state, errors)
}

fn skip_until(expr: &mut VecDeque<Expr>, tokens: [&str; 2]) {
    while !expr.is_empty() {
        if let Some(Expr::Token(first)) = expr.front() {
            if first.inner.as_ref() == tokens[0] || first.inner.as_ref() == tokens[1] {
                return;
            }
        }
        expr.pop_front();
    }
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

    if first.inner.as_ref() == "class" {
        let name = expr.pop_front();
        let Some(Expr::Token(name)) = name else {
            errors.push(Error {
                inner: "class requires a name".to_string(),
                span: first.span,
            });
            return
        };
        process_subclass(name, expr, state, errors);
    } else {
        // assign
        let name = first;
        let array_or_eq = expr.pop_front();

        if let Some(Expr::Array(_)) = array_or_eq {
            expr.pop_front(); // "="
        };

        let Some(value) = process_value(name.span, expr, errors) else {
            return
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
    skip_until(expr, [";", "class"]);
}

/// Given a directory, it tries to open the file "config.cpp" and
/// retrieve the list of function names and corresponding paths in the addon
pub fn analyze_addon(mut directory: PathBuf) -> Result<(Functions, Vec<Error>), String> {
    directory.push("config.cpp");
    analyze_file(directory)
}

/// Given a directory, it tries to open the file "config.cpp" and
/// retrieve the list of function names and corresponding paths in the addon
pub fn analyze_mission(mut directory: PathBuf) -> Result<(Functions, Vec<Error>), String> {
    directory.push("description.ext");
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
