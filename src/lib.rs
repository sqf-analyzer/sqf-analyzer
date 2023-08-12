#![forbid(unsafe_code)]

use std::path::{Path, PathBuf};

use path_clean::PathClean;
use walkdir::WalkDir;

pub mod analyzer;
pub mod database;
pub mod error;
pub mod types;

pub use pest;
pub mod cpp;
pub mod parser;
pub mod preprocessor;
pub mod span;

/// Projects the original path into an absolute, case-insensitive path
/// by transversing the fs and identify the correct path.
pub fn correct_path(path: &Path) -> Option<PathBuf> {
    let path = path.clean(); // replace relative path (e.g. "../")

    // find the directory inside `addons`
    let mut directory = path.to_owned();
    while directory
        .file_name()
        .map(|x| x.to_string_lossy() != "addons")
        .unwrap_or(false)
    {
        if !directory.pop() {
            break;
        }
    }

    // find the case-insensitive path that results in `path`
    let name = path.as_os_str().to_str()?;

    WalkDir::new(&directory).into_iter().find_map(|e| {
        let dir = e.ok()?;
        dir.path()
            .as_os_str()
            .to_str()?
            .eq_ignore_ascii_case(name)
            .then_some(dir.path().to_owned())
    })
}

pub fn check(path: &std::path::Path) -> Vec<error::Error> {
    // atempt to find the file case-insensitive
    let corrected_path = correct_path(path);

    let Some(corrected_path) = corrected_path else {
        return vec![error::Error {
            inner: format!("file \"{}\" not available", path.display()),
            span: (1,1)
        }]
    };

    let Ok(case) = std::fs::read_to_string(&corrected_path) else {
        return vec![error::Error {
            inner: format!("file \"{}\" not available", corrected_path.display()),
            span: (1,1)
        }]
    };
    let iter = preprocessor::tokens(&case, Default::default(), path.to_owned());

    let iter = match iter {
        Ok(iter) => iter,
        Err(e) => return vec![e.1],
    };
    let (expr, errors) = parser::parse(iter);

    let mut state = Default::default();
    analyzer::analyze(&expr, &mut state);
    state.errors.extend(errors);
    state.errors
}
