use crate::analyzer::Configuration;
use crate::{check, get_path};
use crate::{error::Error, parser::Expr};

use super::State;

pub fn compile(rhs: &Expr, state: &mut State) {
    // check that the previous call is a preprocessFileLineNumbers, so that we can fetch the file
    let Expr::Unary(process, code) = rhs else {
        return
    };
    if !process
        .inner
        .eq_ignore_ascii_case("preprocessFileLineNumbers")
    {
        return;
    };

    compile_(code.as_ref(), state)
}

pub fn compile_(rhs: &Expr, state: &mut State) {
    println!("compile: {rhs:?}");
    let Expr::String(path_str) = rhs else {return};
    if path_str.inner.starts_with("\\A3") || path_str.inner.starts_with("\\a3") {
        return;
    };

    let file_path = match get_path(
        path_str.inner.as_ref(),
        &state.configuration.base_path,
        &state.configuration.addons,
    ) {
        Ok(file_path) => file_path,
        Err(e) => {
            state.errors.push(Error::new(e, path_str.span));
            return;
        }
    };

    let configuration = Configuration {
        file_path,
        base_path: state.configuration.base_path.clone(),
        addons: state.configuration.addons.clone(),
    };

    let file_state = check(
        configuration,
        state.namespace.mission.clone(),
        state.settings.clone(),
    );
    match file_state {
        Ok(file_state) => {
            state.namespace.mission = file_state.namespace.mission;
            state.errors.extend(file_state.errors);
        }
        Err(mut e) => {
            e.span = path_str.span;
            e.origin = Some(state.configuration.file_path.clone());
            state.errors.push(e)
        }
    }
}

pub fn exec_vm(rhs: &Expr, state: &mut State) {
    compile_(rhs, state);
}
