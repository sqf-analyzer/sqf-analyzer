#![forbid(unsafe_code)]

pub mod analyzer;
pub mod database;
pub mod error;
pub mod types;
use std::path::PathBuf;

pub use pest;
pub mod cpp;
pub mod parser;
pub mod preprocessor;
pub mod span;

fn find_file(path: &std::path::Path) -> Option<PathBuf> {
    let mut dir = path.to_owned();
    let name = path.as_os_str().to_str().unwrap().to_ascii_lowercase();
    // atempt to find the file case-insensitive
    let mut correct_path = None;
    dir.pop();
    let Ok(entries) = std::fs::read_dir(&dir) else {
        return None
    };
    for entry in entries {
        let entry = entry.unwrap();
        let meta = entry.metadata().unwrap();
        if meta.is_file() {
            let maybe = entry.path();
            let normalized = maybe.as_os_str().to_str().unwrap().to_ascii_lowercase();
            if normalized == name {
                correct_path = Some(maybe);
                break;
            }
        }
    }

    correct_path
}

pub fn check(path: &std::path::Path) -> Vec<error::Error> {
    // atempt to find the file case-insensitive
    let corrected_path = find_file(path);

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
