use std::collections::VecDeque;
use std::path::PathBuf;
use std::sync::Arc;

use crate::error::Error;
use crate::get_path;
use crate::span::{Span, Spanned};

use super::iterator::State;
use super::*;

fn parse_file(
    span: Span,
    content: String,
    state: &mut State,
    path: PathBuf,
) -> Option<VecDeque<Spanned<Arc<str>>>> {
    // parse into VecDeque<Spanned<String>> due to no lifetime management
    let defines = std::mem::take(&mut state.configuration.defines);
    let addons = std::mem::take(&mut state.configuration.addons);
    let configuration = Configuration {
        defines,
        path,
        addons,
    };

    let mut ast = match tokens(&content, configuration) {
        Ok(ast) => ast,
        Err((configuration, e)) => {
            // todo: how do we handle errors in other files? Error may need a lift to track files
            let error = Error {
                inner: format!(
                    "Error while parsing file \"{}\" at position {:?}: {}",
                    configuration.path.display(),
                    e.span,
                    e.inner
                ),
                span,
            };
            state.configuration.defines = configuration.defines;
            state.configuration.addons = configuration.addons;
            state.errors.push(error);
            return None;
        }
    };

    let tokens = ast.by_ref().collect();
    // collect errors and defines
    state.errors.extend(ast.state.errors);
    state.configuration.defines = ast.state.configuration.defines;
    state.configuration.addons = ast.state.configuration.addons;

    Some(tokens)
}

pub fn process_include(
    name: &Spanned<&str>,
    state: &mut State,
) -> Option<VecDeque<Spanned<Arc<str>>>> {
    if name.inner.starts_with("\\A3") || name.inner.starts_with("\\a3") {
        return None;
    };

    let path = get_path(name.inner, &state.configuration)
        .map_err(|e| {
            state.errors.push(Error {
                inner: e,
                span: name.span,
            })
        })
        .ok()?;

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
