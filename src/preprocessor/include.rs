use std::collections::VecDeque;
use std::path::Path;
use std::sync::Arc;

use crate::error::Error;
use crate::get_path;
use crate::span::Spanned;

use super::iterator::State;
use super::*;

fn parse_file(
    content: String,
    state: &mut State,
    path: Arc<Path>,
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
        Err((configuration, mut error)) => {
            error.origin = Some(configuration.path.clone());
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

    let path = get_path(
        name.inner,
        &state.configuration.path,
        &state.configuration.addons,
    )
    .map_err(|e| state.errors.push(Error::new(e, name.span)))
    .ok()?;

    match std::fs::read_to_string(path.clone()) {
        Err(_) => {
            state.errors.push(Error::new(
                format!("Cannot read file in path \"{}\"", path.display()),
                name.span,
            ));
            None
        }
        Ok(content) => parse_file(content, state, path),
    }
}
