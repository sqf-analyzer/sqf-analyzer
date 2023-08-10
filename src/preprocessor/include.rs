use std::collections::VecDeque;
use std::path::PathBuf;

use crate::error::Error;
use crate::span::{Span, Spanned};

use super::iterator::State;
use super::*;

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
        Err((defines, e)) => {
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
            state.defines = defines;
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

pub fn process_include(
    name: &Spanned<&str>,
    state: &mut State,
) -> Option<VecDeque<Spanned<String>>> {
    if name.inner.starts_with("\\A3") || name.inner.starts_with("\\a3") {
        return None;
    };
    let path = build_path(state.path.clone(), name.inner);

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
