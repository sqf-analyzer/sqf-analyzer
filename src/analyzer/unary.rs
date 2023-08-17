use crate::preprocessor::Configuration;
use crate::{check, get_path};
use crate::{error::Error, parser::Expr};

use super::State;

pub fn compile(rhs: &Expr, state: &mut State) {
    // check that the previous call is a preprocessFileLineNumbers, so that we can fetch the file
    if let Expr::Unary(process, code) = rhs {
        if process
            .inner
            .eq_ignore_ascii_case("preprocessFileLineNumbers")
        {
            if let Expr::String(path_str) = code.as_ref() {
                let configuration = Configuration {
                    path: state.path.clone(),
                    ..Default::default()
                };
                match get_path(path_str.inner.as_ref(), &configuration) {
                    Ok(path) => {
                        let file_state = check(
                            &path,
                            state.namespace.mission.clone(),
                            state.settings.clone(),
                        );
                        match file_state {
                            Ok(mut file_state) => {
                                state.namespace.mission = file_state.namespace.mission;
                                file_state.errors.iter_mut().for_each(|x| {
                                    x.origin = Some(path.clone());
                                });
                                state.errors.extend(file_state.errors);
                            }
                            Err(e) => state.errors.push(e),
                        }
                    }
                    Err(e) => state.errors.push(Error::new(e, path_str.span)),
                };
            }
        }
    }
}
