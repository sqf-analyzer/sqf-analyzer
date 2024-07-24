use std::collections::{HashSet, VecDeque};
use std::sync::Arc;

use crate::error::Error;
use crate::preprocessor::iterator::MacroState;
use crate::span::Spanned;

use super::iterator::{Arguments, DefineState, PreprocessorState, State};
use super::*;

/// returns whether the token is consumed
fn advance_state(
    define_state: &mut DefineState,
    item: &Spanned<Arc<str>>,
    errors: &mut Vec<Error>,
) -> bool {
    let macro_state = &mut define_state.state;
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
        (MacroState::ParenthesisStart, _) => return false,
        (MacroState::Argument(0), ",") => {
            define_state.arguments.push(Default::default());
            macro_state.span.1 += item.span.1 - item.span.0;
        }
        (MacroState::Argument(other), v) => {
            push_argument(&mut define_state.arguments, item);
            macro_state.span.1 += item.span.1 - item.span.0;
            if matches!(v, "(" | "[") {
                macro_state.inner = MacroState::Argument(other + 1)
            };
            if matches!(v, ")" | "]") {
                macro_state.inner = MacroState::Argument(other - 1)
            };
        }
        (MacroState::None, _) => {
            errors.push(Error::new(
                format!(
                    "Token {} un-expected while parsing macro arguments",
                    item.inner.as_ref()
                ),
                item.span,
            ));
        }
    }
    true
}

/// Returns whether the token is consumed by this update or not
pub fn update(state: &mut State, item: &Spanned<Arc<str>>) -> bool {
    let consumed = update_(
        &mut state.current_macro,
        &mut state.errors,
        &state.configuration.defines,
        item,
        &mut state.stack,
    );
    concat(&mut state.stack);
    state.stack.retain(|x| x.inner.as_ref() != " ");
    consumed
}

/// fills the stack with items based on the state and set of defines
/// E.g. items ["A", "(", "1", ")"] will result in the stack with the body of macro A with corresponding replacement.
fn update_(
    preprocessor_state: &mut PreprocessorState,
    errors: &mut Vec<Error>,
    defines: &Defines,
    item: &Spanned<Arc<str>>,
    stack: &mut VecDeque<Spanned<Arc<str>>>,
) -> bool {
    if let Some(def) = &mut preprocessor_state.define {
        let matched = advance_state(def, item, errors);
        if !matched {
            // the keyword was the same, but there are no parenthesis and thus
            // it is not a macro call
            let def = std::mem::take(&mut preprocessor_state.define).unwrap();
            preprocessor_state
                .in_recursion
                .remove(&def.define.name.inner);
            stack.push_back(def.define.name);
            stack.push_back(item.clone());
            return true;
        }
        if def.state.inner == MacroState::None {
            let DefineState {
                mut define,
                mut arguments,
                state,
            } = std::mem::take(&mut preprocessor_state.define).unwrap();
            // expand each of the arguments of the macro with other macro calls
            for argument in arguments.iter_mut() {
                expand_(argument, defines, errors, &preprocessor_state.in_recursion);
                concat(argument);
            }

            if !preprocessor_state
                .in_recursion
                .insert(define.name.inner.clone())
            {
                errors.push(Error::new(
                    format!("Macro {} recurses", define.name.inner),
                    item.span,
                ));
                stack.push_back(item.clone());
                return true;
            }

            // replace arguments of define in its body
            replace(&mut define.body, &define.arguments, &arguments);

            // expand the body with other macro calls
            expand_(
                &mut define.body,
                defines,
                errors,
                &preprocessor_state.in_recursion,
            );

            define.body.iter_mut().for_each(|x| x.span = state.span);

            stack.extend(define.body);

            preprocessor_state.in_recursion.remove(&define.name.inner);
        }
        return true; // we consume all tokens until we are done
    };

    if let Some(define) = defines.get(item.inner.as_ref()) {
        if define.arguments.is_none() {
            if !preprocessor_state
                .in_recursion
                .insert(define.name.inner.clone())
            {
                errors.push(Error::new(
                    format!("Macro {} recurses", define.name.inner),
                    item.span,
                ));
                stack.push_back(item.clone());
                return true;
            }

            let mut tokens = define.body.clone();
            expand_(
                &mut tokens,
                defines,
                errors,
                &preprocessor_state.in_recursion,
            );

            tokens.iter_mut().for_each(|x| x.span = item.span);

            stack.extend(tokens);

            preprocessor_state.in_recursion.remove(&define.name.inner);
        } else {
            // enter the start of a define
            preprocessor_state.define = Some(DefineState {
                define: define.clone(),
                arguments: Default::default(),
                state: Spanned::new(MacroState::ParenthesisStart, item.span),
            });
        }

        return true; // we consumed the macro token
    };
    false
}

/// re-writes tokens by concatenating tokens
fn concat(tokens: &mut VecDeque<Spanned<Arc<str>>>) {
    if tokens.iter().any(|x| x.inner.as_ref() == "##") {
        // join tokens in pairs
        let mut merged: VecDeque<Spanned<Arc<str>>> = VecDeque::new();
        let mut next_merges = false;
        for token in tokens.iter_mut() {
            if token.inner.as_ref() == "##" {
                next_merges = true;
            } else if next_merges {
                if let Some(previous) = merged.back_mut() {
                    let Spanned { inner, .. } = previous;
                    *inner = if inner.as_ref() == " " {
                        format!("{}", token.inner).into()
                    } else {
                        format!("{}{}", inner, token.inner).into()
                    };
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

    if tokens.iter().any(|x| x.inner.as_ref() == "'") {
        // join tokens in pairs
        let mut merged: VecDeque<Spanned<Arc<str>>> = VecDeque::new();
        let mut in_quote = false;

        for token in tokens.iter() {
            if token.inner.as_ref() == "'" && !in_quote {
                in_quote = true;
                merged.push_back(token.clone());
            } else if in_quote {
                if let Some(previous) = merged.back_mut() {
                    let Spanned { inner, .. } = previous;
                    *inner = format!("{}{}", inner, token.inner).into();
                } else {
                    merged.push_back(token.clone())
                }
                if token.inner.as_ref() == "'" {
                    in_quote = false;
                }
            } else {
                merged.push_back(token.clone())
            }
        }
        *tokens = merged;
    }
}

/// Expands the arguments by evaluating macros inside them and replacing them
fn expand_(
    tokens: &mut VecDeque<Spanned<Arc<str>>>,
    defines: &Defines,
    errors: &mut Vec<Error>,
    in_recursion: &HashSet<Arc<str>>,
) {
    let taken_tokens = std::mem::take(tokens);

    let mut new_tokens = VecDeque::new();
    let mut state = PreprocessorState {
        define: None,
        in_recursion: in_recursion.clone(),
    };
    for token in taken_tokens {
        if !update_(&mut state, errors, defines, &token, &mut new_tokens) {
            new_tokens.push_back(token)
        }
    }
    *tokens = new_tokens;
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
fn replace(
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
                    span: (tokens.front().map(|x| x.span).unwrap_or((0, 0))),
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
