use std::path::Component;
use std::path::PathBuf;
use std::sync::Arc;

use pest::iterators::Pair;

use crate::error::Error;
use crate::span::Spanned;

mod ast;
mod define;
mod include;
mod iterator;
mod parser;

pub use ast::{Ast, Define, Defines};
pub use iterator::AstIterator;

impl<'a> From<Pair<'a, parser::Rule>> for Spanned<Arc<str>> {
    fn from(value: Pair<'a, parser::Rule>) -> Self {
        Self {
            span: (value.as_span().start(), value.as_span().end()),
            inner: value.as_str().into(),
        }
    }
}

pub fn parse(data: &str) -> Result<Ast, Error> {
    parser::pairs(data).map(parser::parse)
}

pub fn tokens(
    data: &str,
    defines: Defines,
    path: PathBuf,
) -> Result<AstIterator, (Defines, Error)> {
    match parse(data) {
        Ok(ast) => Ok(AstIterator::new(ast, defines, path)),
        Err(e) => Err((defines, e)),
    }
}

fn string_to_path(path: &str) -> PathBuf {
    let mut new: PathBuf = PathBuf::new();
    for path in path.split('\\') {
        new.push(path)
    }
    new
}

pub fn build_path(mut file_path: PathBuf, include_path: &str) -> PathBuf {
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
