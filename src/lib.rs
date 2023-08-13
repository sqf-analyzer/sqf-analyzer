#![forbid(unsafe_code)]

use std::path::{Path, PathBuf};

pub mod analyzer;
pub mod database;
pub mod error;
pub mod types;

use path_clean::PathClean;
pub use pest;
use walkdir::WalkDir;
pub mod cpp;
pub mod parser;
pub mod preprocessor;
pub mod span;

/// Given a name taken from a file representing a path, together with the path of the file
/// it was taken from, tries to reconstruct the absolute path of the file
pub fn get_path(name: &str, root: PathBuf) -> Result<PathBuf, String> {
    let path: PathBuf = name.replace('\\', "/").into();

    let mut directory = root;
    directory.pop();

    if !path.is_absolute() {
        directory.push(path);
        directory = directory.clean();
        Ok(find_path(&directory).unwrap_or(directory))
    } else {
        // find $PBOPREFIX$ in the parent directory of the file and see how it is
        // located in relation to what $PBOPREFIX$ says
        // TODO: loop over directories until PBOPREFIX is found
        let pbo_path = directory.join("$PBOPREFIX$");
        let pbo_prefix = std::fs::read_to_string(&pbo_path).map_err(|_| {
            format!(
                "The included path \"{}\" is absolute and no $PBOPREFIX$ was found at {}",
                path.display(),
                pbo_path.display()
            )
        })?;

        let (mut project_root, pbo) = compute_project_root(directory, pbo_prefix);
        let relative_path = path.strip_prefix(&pbo).map_err(|_| {
            format!(
                "The $PBOPREFIX$ has a path \"{}\" that is incompatible with \"{}\"",
                pbo.display(),
                path.display(),
            )
        })?;

        project_root.push(relative_path);

        Ok(find_path(&project_root).unwrap_or(project_root))
    }
}

fn compute_project_root(mut path: PathBuf, pbo_prefix: String) -> (PathBuf, PathBuf) {
    let mut content: PathBuf = format!("/{}", pbo_prefix.replace('\\', "/")).into();

    // align paths
    while path.file_name() != content.file_name() {
        path.pop();
        if path.as_os_str().is_empty() || content.as_os_str().is_empty() {
            break;
        }
    }

    // take combined until we reach to the root
    while path.file_name() == content.file_name() {
        path.pop();
        content.pop();
        if path.as_os_str().is_empty() || content.as_os_str().is_empty() {
            break;
        }
    }

    (path, content)
}

/// Projects the original path into an absolute, case-insensitive path
/// by transversing the fs and identify the correct path.
fn find_path(path: &Path) -> Option<PathBuf> {
    // find the case-insensitive path that results in `path`
    let name = path.as_os_str().to_str()?;

    for ancestor in path.ancestors().take(3) {
        if let Some(matc) = WalkDir::new(ancestor).into_iter().find_map(|e| {
            let dir = e.ok()?;
            dir.path()
                .as_os_str()
                .to_str()?
                .eq_ignore_ascii_case(name)
                .then_some(dir.path().to_owned())
        }) {
            return Some(matc);
        }
    }
    None
}

pub fn check(path: &std::path::Path) -> Vec<error::Error> {
    let Ok(case) = std::fs::read_to_string(path) else {
        return vec![error::Error {
            inner: format!("file \"{}\" not available", path.display()),
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

#[cfg(test)]
mod tests {
    use super::*;

    use std::path::PathBuf;
    #[test]
    fn assymetric_path() {
        let pbo_prefix = "x/A3A/addons/events".into();
        let directory: PathBuf =
            "../A3-Antistasi/A3A/addons/events/functions/fn_validateEventArguments.sqf".into();
        let path: PathBuf = "/x/A3A/addons/core/Includes/script_mod.hpp".into();

        let (project_root, pbo) = compute_project_root(directory, pbo_prefix);
        let r = project_root.join(path.strip_prefix(&pbo).unwrap());
        assert_eq!(
            r,
            PathBuf::from("../A3-Antistasi/A3A/addons/core/Includes/script_mod.hpp")
        );
    }
}
