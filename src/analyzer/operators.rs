use std::sync::Arc;

use crate::{
    parser::Expr,
    span::{Span, Spanned},
};

use super::{infer_binary, infer_type, Output, State};

pub fn then(
    span: Span,
    lhs: &Expr,
    op: &Spanned<Arc<str>>,
    rhs: &Expr,
    state: &mut State,
) -> Option<Output> {
    // the evaluate of "then" may result in a branch with different types
    // clone the original stack and evaluate "else" with the original stack
    let lhs = infer_type(lhs, state);
    let rhs = infer_type(rhs, state);
    infer_binary(
        lhs.map(|x| x.type_()),
        op,
        rhs.as_ref().map(|x| x.type_()),
        span,
        &mut state.errors,
    )
    .map(|(_, explanation)| {
        state.explanations.insert(op.span, explanation);
    })?;
    if let Some(Output::Code(_, t)) = rhs {
        t.map(Output::Type)
    } else {
        // return the type from the only branch
        rhs
    }
}

pub fn else_(
    span: Span,
    lhs: &Expr,
    op: &Spanned<Arc<str>>,
    rhs: &Expr,
    state: &mut State,
) -> Option<Output> {
    // the evaluate of "then" may result in a branch with different types
    // clone the original stack and evaluate "else" with the original stack
    let original = state.namespace.stack.clone();
    let lhs = infer_type(lhs, state).map(|x| x.type_());
    state.namespace.stack = original;
    let rhs = infer_type(rhs, state);
    infer_binary(
        lhs,
        op,
        rhs.as_ref().map(|x| x.type_()),
        span,
        &mut state.errors,
    )
    .map(|(_, explanation)| {
        state.explanations.insert(op.span, explanation);
    })?;
    // return the type from one of the branches.
    rhs
}

pub fn remoteexec(
    span: Span,
    lhs: &Expr,
    op: &Spanned<Arc<str>>,
    rhs: &Expr,
    state: &mut State,
) -> Option<Output> {
    if let Expr::Array(items) = &rhs {
        if let Some(Expr::String(name)) = items.inner.first() {
            if let Some((origin, _)) = state.namespace.get(&name.inner) {
                state.origins.insert(name.span, origin);
            }
        }
    }

    infer_binary(
        infer_type(lhs, state).map(|x| x.type_()),
        op,
        infer_type(rhs, state).map(|x| x.type_()),
        span,
        &mut state.errors,
    )
    .map(|(type_, explanation)| {
        state.explanations.insert(op.span, explanation);
        type_.into()
    })
}
