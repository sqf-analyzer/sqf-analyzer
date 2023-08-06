#![forbid(unsafe_code)]

pub mod analyzer;
pub mod database;
pub mod error;
pub mod types;
pub use pest;
pub mod cpp;
pub mod parser;
pub mod preprocessor;
pub mod span;

pub fn check(path: &std::path::Path) -> Vec<error::Error> {
    let Ok(case) = std::fs::read_to_string(path) else {
        return vec![error::Error {
            inner: format!("file \"{:?}\" not available", path.as_os_str()),
            span: (1,1)
        }]
    };
    let iter = preprocessor::tokens(&case, Default::default(), path.to_owned());

    let iter = match iter {
        Ok(iter) => iter,
        Err(e) => return vec![e],
    };
    let (expr, errors) = parser::parse(iter);

    let mut r = analyzer::analyze(&expr);
    r.errors.extend(errors);
    r.errors
}
