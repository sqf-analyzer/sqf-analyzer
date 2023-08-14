use std::sync::Arc;

use crate::{
    parser::Expr,
    span::{Span, Spanned},
    types::Type,
};

use super::type_operations::union_stacks;
use super::{infer_binary, infer_type, process_parameters, union_output, Output, State};

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
    let lhs = infer_type(lhs, state);
    // state stack now contains the "then" branch
    let then = std::mem::take(&mut state.namespace.stack);
    state.namespace.stack = original;
    let rhs = infer_type(rhs, state);
    let else_ = std::mem::take(&mut state.namespace.stack);
    state.namespace.stack = union_stacks(then, else_);

    infer_binary(
        lhs.as_ref().map(|x| x.type_()),
        op,
        rhs.as_ref().map(|x| x.type_()),
        span,
        &mut state.errors,
    )
    .map(|(_, explanation)| {
        state.explanations.insert(op.span, explanation);
    })?;
    // when lhs was code with a return type, use it
    let lhs = if let Some(Output::Code(_, t)) = lhs {
        t.map(Output::Type)
    } else {
        lhs
    };
    // when rhs was code with a return type, use it
    let rhs = if let Some(Output::Code(_, t)) = rhs {
        t.map(Output::Type)
    } else {
        rhs
    };
    // returns the union of the types
    Some(Output::Code(
        None,
        union_output(lhs, rhs).map(|x| x.type_()),
    ))
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
            if let Some((origin, return_type)) = state.namespace.get(&name.inner) {
                state.origins.insert(name.span, origin);

                if let Some(Output::Code(parameters, _)) = return_type {
                    if let Expr::Array(lhs) = lhs {
                        // annotate parameters with the functions' signature if it is available
                        if let Some(parameters) = parameters {
                            process_parameters(lhs, &parameters, state)
                        }
                    }
                }
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

pub fn foreach(
    span: Span,
    lhs: &Expr,
    op: &Spanned<Arc<str>>,
    rhs: &Expr,
    state: &mut State,
) -> Option<Output> {
    // technically not right as the _x should be assigned to the new future stack of lhs, not here
    state.namespace.insert(
        Spanned::new("_x".into(), op.span),
        Some(Type::Anything.into()),
        true,
    );
    let lhs = infer_type(lhs, state);
    state
        .namespace
        .stack
        .last_mut()
        .unwrap()
        .variables
        .remove("_x");

    if let Some((_, explanation)) = infer_binary(
        lhs.as_ref().map(|x| x.type_()),
        op,
        infer_type(rhs, state).map(|x| x.type_()),
        span,
        &mut state.errors,
    ) {
        state.explanations.insert(op.span, explanation);
    }

    // when lhs was code with a return type, use it
    if let Some(Output::Code(_, t)) = lhs {
        t.map(Output::Type)
    } else {
        lhs
    }
}

pub fn exit_with(
    span: Span,
    lhs: &Expr,
    op: &Spanned<Arc<str>>,
    rhs: &Expr,
    state: &mut State,
) -> Option<Output> {
    let lhs = infer_type(lhs, state).map(|x| x.type_());
    let rhs = infer_type(rhs, state).map(|x| x.type_());

    state.update_return(rhs);

    infer_binary(lhs, op, rhs, span, &mut state.errors).map(|(type_, explanation)| {
        state.explanations.insert(op.span, explanation);
        type_.into()
    })
}