use std::{
    collections::HashMap,
    path::{Path, PathBuf},
    sync::Arc,
};

pub mod analyzer;
pub mod cpp;
pub mod database;
pub mod error;
pub mod parser;
pub mod preprocessor;
pub mod span;
pub mod types;

use analyzer::{Configuration, MissionNamespace, Settings, State};
use path_clean::PathClean;
use path_slash::PathBufExt;
pub use pest;
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
pub fn get_path(
    name: &str,
    base_path: &Path,
    addons: &HashMap<Arc<str>, PathBuf>,
) -> Result<Arc<Path>, String> {
    // Note: path.is_absolute() requires the drive on windows, while Arma 3 absolute is without drive
    let is_absolute = name.starts_with('\\') | name.starts_with('/');
    let path = PathBuf::from_slash(name.replace('\\', "/"));

    let mut directory = base_path.to_owned();
    directory.pop();

    if !is_absolute {
        directory.push(path);
        directory = directory.clean();
        Ok(find_path(&directory).unwrap_or_else(|| directory.into()))
    } else {
        // e.g. x/cba -> /../cba
        let project_root = if let Some((prefix, directory)) =
            addons.iter().find_map(|(prefix, addon_path)| {
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
                    "The $PBOPREFIX$ has a path \"{}\" and configuration has prefixes {:?}, but the path \"{}\" is incompatible with all",
                    pbo.display(),
                    addons.keys().collect::<Vec<_>>(),
                    path.display(),
                )
            })?;

            project_root.push(relative_path);
            project_root
        };

        Ok(find_path(&project_root).unwrap_or_else(|| project_root.into()))
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
fn find_path(path: &Path) -> Option<Arc<Path>> {
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
            return Some(matc.into());
        }
    }
    None
}

pub fn check(
    configuration: Configuration,
    mission: MissionNamespace,
    settings: Settings,
) -> Result<State, error::Error> {
    let text = std::fs::read_to_string(&configuration.file_path).map_err(|e| {
        error::Error::new(
            format!(
                "file \"{}\" not available: {e}",
                &configuration.file_path.display()
            ),
            (1, 1),
        )
    })?;
    check_content(&text, configuration, mission, settings)
}

pub fn check_content(
    text: &str,
    configuration: Configuration,
    mission: MissionNamespace,
    settings: Settings,
) -> Result<State, error::Error> {
    let conf = preprocessor::Configuration {
        path: configuration.file_path.clone(),
        addons: configuration.addons.clone(),
        ..Default::default()
    };
    let iter = preprocessor::tokens(&text, conf).map_err(|x| x.1)?;

    let (expr, errors) = parser::parse(iter);

    let mut state = State {
        settings,
        configuration,
        ..Default::default()
    };
    state.namespace.mission = mission;
    analyzer::analyze(&expr, &mut state);
    state.errors.extend(errors);
    state.errors.iter_mut().for_each(|error| {
        error.origin = std::mem::take(&mut error.origin)
            .or_else(|| Some(state.configuration.file_path.clone()));
    });
    Ok(state)
}

/// Set of filenames that are by default handled by the engine
pub const MISSION_INIT_SCRIPTS: [&str; 8] = [
    "init.sqf",
    "initPlayerLocal.sqf",
    "initPlayerServer.sqf",
    "onPlayerKilled.sqf",
    "onPlayerRespawn.sqf",
    "initServer.sqf",
    "initPlayerServer.sqf",
    "initPlayerLocal.sqf",
];

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
