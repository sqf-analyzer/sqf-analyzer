use std::collections::VecDeque;
use std::sync::Arc;

use crate::error::Error;
use crate::preprocessor::iterator::MacroState;
use crate::span::Spanned;

use super::iterator::{Arguments, DefineState, State};
use super::*;

/// returns whether the token is consumed
fn advance_state(
    define_state: &mut DefineState,
    item: &Spanned<Arc<str>>,
    errors: &mut Vec<Error>,
) {
    let mut macro_state = &mut define_state.2;
    match (macro_state.inner, item.inner.as_ref()) {
        (MacroState::Argument(0), ")") => {
            // end state
            macro_state.inner = MacroState::None;
            macro_state.span.1 += item.span.1 - item.span.0;
        }
        // start state
        (MacroState::ParenthesisStart, "(") => {
            macro_state.inner = MacroState::Argument(0);
            macro_state.span.1 += item.span.1 - item.span.0;
        }
        (MacroState::ParenthesisStart, _) => {
            todo!()
        }
        (MacroState::Argument(0), ",") => {
            define_state.1.push(Default::default());
            macro_state.span.1 += item.span.1 - item.span.0;
        }
        (MacroState::Argument(other), v) => {
            push_argument(&mut define_state.1, item);
            macro_state.span.1 += item.span.1 - item.span.0;
            if v == "(" {
                macro_state.inner = MacroState::Argument(other + 1)
            };
            if v == ")" {
                macro_state.inner = MacroState::Argument(other - 1)
            };
        }
        (MacroState::None, _) => {
            errors.push(Error {
                inner: format!(
                    "Token {} un-expected while parsing macro arguments",
                    item.inner.as_ref()
                ),
                span: item.span,
            });
        }
    }
}

/// Returns whether the token is consumed by this update or not
pub fn update(state: &mut State, item: &Spanned<Arc<str>>) -> bool {
    let consumed = update_(
        &mut state.other,
        &mut state.errors,
        &state.defines,
        item,
        &mut state.stack,
    );
    concat(&mut state.stack);
    consumed
}

/// fills the stack with items based on the state and set of defines
/// E.g. items ["A", "(", "1", ")"] will result in the stack with the body of macro A with corresponding replacement.
pub fn update_(
    state: &mut Option<DefineState>,
    errors: &mut Vec<Error>,
    defines: &Defines,
    item: &Spanned<Arc<str>>,
    stack: &mut VecDeque<Spanned<Arc<str>>>,
) -> bool {
    if let Some(def) = state {
        advance_state(def, item, errors);
        if def.2.inner == MacroState::None {
            let (mut define, mut arguments, state) = std::mem::take(state).unwrap();
            // expand each of the arguments of the macro with other macro calls
            expand(defines, &mut arguments, errors);

            // replace arguments of define in its body
            replace_(&mut define.body, &define.arguments, &arguments);

            // expand the body with other macro calls
            expand_(&mut define.body, defines, errors);

            define.body.iter_mut().for_each(|x| x.span = state.span);

            stack.extend(define.body);
        }
        return true; // we consume all tokens until we are done
    };

    if let Some(define) = defines.get(item.inner.as_ref()) {
        if define.arguments.is_none() {
            let mut tokens = define.body.clone();
            expand_(&mut tokens, defines, errors);

            tokens.iter_mut().for_each(|x| x.span = item.span);

            stack.extend(tokens);
        } else {
            // enter the start of a define
            *state = Some((
                define.clone(),
                Default::default(),
                Spanned::new(MacroState::ParenthesisStart, define.keyword.span),
            ));
        }

        return true; // we consumed the macro token
    };
    false
}

/// re-writes tokens by concatenating tokens
fn concat(tokens: &mut VecDeque<Spanned<Arc<str>>>) {
    if tokens.iter().any(|x| x.inner.as_ref() == "##") {
        // join tokens in pairs
        let mut merged = VecDeque::new();
        let mut next_merges = false;
        for token in tokens.iter_mut() {
            if token.inner.as_ref() == "##" {
                next_merges = true;
            } else if next_merges {
                if let Some(previous) = merged.back_mut() {
                    let Spanned { inner, .. } = previous;
                    *inner = format!("{}{}", inner, token.inner).into();
                } else {
                    merged.push_back(token.clone())
                }
                next_merges = false;
            } else {
                merged.push_back(token.clone())
            }
        }
        *tokens = merged;
    };
}

/// Expands the arguments by evaluating macros inside them and replacing them
fn expand_(tokens: &mut VecDeque<Spanned<Arc<str>>>, defines: &Defines, errors: &mut Vec<Error>) {
    let taken_tokens = std::mem::take(tokens);

    let mut new_tokens = VecDeque::new();
    let mut state = None;
    for token in taken_tokens {
        if !update_(&mut state, errors, defines, &token, &mut new_tokens) {
            new_tokens.push_back(token)
        }
    }
    *tokens = new_tokens;
}

/// Expands the arguments by evaluating macros inside them.
fn expand(defines: &Defines, arguments: &mut Arguments, errors: &mut Vec<Error>) {
    for argument in arguments {
        expand_(argument, defines, errors);
        concat(argument);
    }
}

fn push_argument(arguments: &mut Arguments, item: &Spanned<Arc<str>>) {
    // get the top of the stack missing arguments
    if let Some(last) = arguments.last_mut() {
        last.push_back(item.clone())
    } else {
        arguments.push(VecDeque::from([item.clone()]))
    };
}

fn quote(tokens: &VecDeque<Spanned<Arc<str>>>) -> Arc<str> {
    let mut quoted = String::new();
    quoted.push('\"');
    for token in tokens {
        quoted.push_str(token.inner.as_ref());
    }
    quoted.push('\"');
    quoted.into()
}

/// replaces all macro arguments by its corresponding arguments, quoting (# -> "") any argument accordingly
fn replace_(
    tokens: &mut VecDeque<Spanned<Arc<str>>>,
    define_arguments: &Option<Vec<Spanned<Arc<str>>>>,
    arguments: &[VecDeque<Spanned<Arc<str>>>],
) {
    let Some(args) = &define_arguments else {
        return;
    };
    let taken_tokens = std::mem::take(tokens);

    // todo : check number of arguments is the same
    let replace = |arg: &str| {
        args.iter()
            .zip(arguments.iter())
            .find(|(key, _)| key.inner.as_ref() == arg)
            .map(|x| x.1)
    };

    let mut new_tokens: VecDeque<Spanned<Arc<str>>> = Default::default();
    for token in taken_tokens {
        let is_quote = token.inner.starts_with('#')
            && token.inner.as_ref() != "##"
            && token.inner.as_ref() != "#";
        let key = if is_quote {
            token.inner.get(1..).unwrap_or("")
        } else {
            &token.inner
        };
        if let Some(tokens) = replace(key) {
            if is_quote {
                new_tokens.push_back(Spanned {
                    inner: quote(tokens),
                    span: (tokens.get(0).map(|x| x.span).unwrap_or((0, 0))),
                })
            } else {
                new_tokens.extend(tokens.iter().cloned())
            }
        } else if is_quote {
            new_tokens.push_back(token.as_ref().map(|s| format!("\"{s}\"").into()))
        } else {
            new_tokens.push_back(token.clone())
        };
    }

    *tokens = new_tokens;
}
