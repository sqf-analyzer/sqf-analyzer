use std::collections::VecDeque;
use std::path::{Component, PathBuf};

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

fn string_to_path(path: &str) -> PathBuf {
    let mut new: PathBuf = PathBuf::new();
    for path in path.split('\\') {
        new.push(path)
    }
    new
}

fn build_path(mut file_path: PathBuf, include_path: &str) -> PathBuf {
    file_path.pop();
    file_path.push(include_path);
    if include_path.starts_with(r"\x\") {
        // need to identify the root based on the file_path
        let include_path = string_to_path(include_path);
        let cp = include_path.components().nth(1);
        let Some(Component::Normal(cp)) = cp else {
            return include_path;
        };
        while file_path.parent().is_some() {
            if file_path.ends_with(cp) {
                break;
            }
            file_path.pop();
        }
        file_path.extend(include_path.components().skip(2));
        return file_path;
    }

    let include_path = string_to_path(include_path);

    file_path.pop();
    file_path.push(include_path);
    file_path
}

pub fn process_include(
    name: &Spanned<&str>,
    state: &mut State,
) -> Option<VecDeque<Spanned<String>>> {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic() {
        assert_eq!(
            build_path("/a/a.sqf".into(), "macros.hpp"),
            PathBuf::from("/a/macros.hpp")
        );
    }

    #[test]
    fn test_relative() {
        // // #include "..\file.sqf"
        assert_eq!(
            build_path("/a/a.sqf".into(), "..\\file.hpp"),
            PathBuf::from("/a/../file.hpp")
        );
    }

    #[test]
    fn test_abs_x() {
        // #include "\x\A3A\addons\core\Includes\script_mod.hpp"
        assert_eq!(
            build_path(
                "/home/user/a/A3A/addons/events/script_component.hpp".into(),
                r#"\x\A3A\addons\core\Includes\script_mod.hpp"#
            ),
            PathBuf::from("/home/user/a/A3A/addons/core/Includes/script_mod.hpp")
        );
    }
}
