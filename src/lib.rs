use std::path::{Path, PathBuf};

pub mod analyzer;
pub mod cpp;
pub mod database;
pub mod error;
pub mod parser;
pub mod preprocessor;
pub mod span;
pub mod types;

use analyzer::{MissionNamespace, Settings, State};
use path_clean::PathClean;
use path_slash::PathBufExt;
pub use pest;
use preprocessor::Configuration;
use walkdir::WalkDir;

fn find_pboprefix(mut directory: PathBuf) -> Option<String> {
    while !directory.as_os_str().is_empty() {
        let pbo_path = directory.join("$PBOPREFIX$");
        if let Ok(mut pbo_prefix) = std::fs::read_to_string(pbo_path) {
            if pbo_prefix.ends_with('\n') {
                pbo_prefix.pop();
            }
            return Some(pbo_prefix);
        };
        directory.pop();
    }
    None
}

/// Given a name taken from a file representing a path, together with the path of the file
/// it was taken from, tries to reconstruct the absolute path of the file
pub fn get_path(name: &str, configuration: &Configuration) -> Result<PathBuf, String> {
    // Note: path.is_absolute() requires the drive on windows, while Arma 3 absolute is without drive
    let is_absolute = name.starts_with('\\') | name.starts_with('/');
    let path = PathBuf::from_slash(name.replace('\\', "/"));

    let mut directory = configuration.path.clone();
    directory.pop();

    if !is_absolute {
        directory.push(path);
        directory = directory.clean();
        Ok(find_path(&directory).unwrap_or(directory))
    } else {
        // e.g. x/cba -> /../cba
        let project_root = if let Some((prefix, directory)) =
            configuration
                .addons
                .iter()
                .find_map(|(prefix, addon_path)| {
                    path.starts_with(prefix.as_ref())
                        .then_some((prefix, addon_path))
                }) {
            let relative_path = path.strip_prefix(prefix.as_ref()).unwrap();
            directory.join(relative_path)
        } else {
            // find $PBOPREFIX$ in the parent directory of the file and see how it is
            // located in relation to what $PBOPREFIX$ says
            // TODO: loop over directories until PBOPREFIX is found
            let pbo_prefix = find_pboprefix(directory.clone()).ok_or_else(|| {
                format!(
                    "The included path \"{}\" is absolute and no $PBOPREFIX$ was found at {} or parent paths",
                    path.display(),
                    directory.display()
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
            project_root
        };

        Ok(find_path(&project_root).unwrap_or(project_root))
    }
}

fn compute_project_root(mut path: PathBuf, pbo_prefix: String) -> (PathBuf, PathBuf) {
    let mut prefix: PathBuf = format!("/{}", pbo_prefix.replace('\\', "/")).into();

    // align paths
    while path.file_name() != prefix.file_name() {
        path.pop();
        if path.as_os_str().is_empty() || prefix.as_os_str().is_empty() {
            break;
        }
    }

    // take combined until we reach to the root
    while path.file_name() == prefix.file_name() {
        path.pop();
        prefix.pop();
        if path.as_os_str().is_empty() || prefix.as_os_str().is_empty() {
            break;
        }
    }

    (path, prefix)
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

pub fn check(
    path: &std::path::Path,
    mission: MissionNamespace,
    settings: Settings,
) -> Result<State, error::Error> {
    let case = std::fs::read_to_string(path).map_err(|_| {
        error::Error::new(format!("file \"{}\" not available", path.display()), (1, 1))
    })?;
    let iter =
        preprocessor::tokens(&case, Configuration::with_path(path.to_owned())).map_err(|x| x.1)?;

    let (expr, errors) = parser::parse(iter);

    let mut state = State {
        settings,
        ..Default::default()
    };
    state.namespace.mission = mission;
    analyzer::analyze(&expr, &mut state);
    state.errors.extend(errors);
    Ok(state)
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

pub use uncased::UncasedStr;

/// converts an &UncasedStr to an Arc<UncasedStr>
pub fn uncased(v: &str) -> std::sync::Arc<UncasedStr> {
    // same implementation as https://doc.rust-lang.org/src/alloc/sync.rs.html#2691
    // waiting for https://github.com/SergioBenitez/uncased/pull/8
    let arc = std::sync::Arc::<[u8]>::from(v.as_bytes());
    unsafe { std::sync::Arc::from_raw(std::sync::Arc::into_raw(arc) as *const UncasedStr) }
}
