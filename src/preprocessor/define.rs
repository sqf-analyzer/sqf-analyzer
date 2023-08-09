use std::collections::VecDeque;

use crate::error::Error;
use crate::preprocessor::iterator::MacroState;
use crate::span::{Span, Spanned};

use super::iterator::{Arguments, State};
use super::*;

pub fn update<'a>(state: &mut State, item: &SpannedRef<'a>) -> Option<()> {
    if let Some(define) = state.defines.get(item.inner.as_ref()) {
        // start of the state machine
        if define.arguments.is_empty() {
            state.stack.clear();
            evaluate(
                &mut define.clone(),
                item.span,
                &[],
                &state.defines,
                &mut state.errors,
                &mut state.stack,
            );
            return Some(());
        } else {
            // there is a define with this token => push it to the stack
            state.define = Some((define.clone(), vec![]));

            if state.state.inner != MacroState::None {
                state.errors.push(Error {
                    inner: "macro invoked inside another macro".to_string(),
                    span: item.span,
                });
                state.state.inner = MacroState::None;
            }
        }
        state.state.inner = MacroState::ParenthesisStart;
        state.state.span = item.span;
        return Some(());
    }

    // if no macro being operated on, continue
    let Some((_, arguments)) = state.define.as_mut() else {
        return None;
    };

    match (state.state.inner, item.inner.as_ref()) {
        (MacroState::Argument | MacroState::Coma, ")") => {
            // end state
            state.state.inner = MacroState::None;
            state.state.span.1 += item.span.1 - item.span.0;
            // evaluate define and push it to the stack
            let (define, arguments) = state.define.as_mut().unwrap();
            state.stack.clear();
            evaluate(
                define,
                state.state.span,
                arguments,
                &state.defines,
                &mut state.errors,
                &mut state.stack,
            );
            state.state.span = (0, 0);
            state.define = None;
            Some(())
        }
        // start state
        (MacroState::ParenthesisStart | MacroState::Coma, _) => {
            state.state.inner = MacroState::Argument;
            state.state.span.1 += item.span.1 - item.span.0;
            Some(())
        }
        (MacroState::Argument, ",") => {
            arguments.push(Default::default());
            state.state.span.1 += item.span.1 - item.span.0;
            Some(())
        }
        (MacroState::Argument, _) => {
            push_argument(arguments, item);
            state.state.span.1 += item.span.1 - item.span.0;
            Some(())
        }
        (MacroState::None, _) => {
            state.errors.push(Error {
                inner: format!(
                    "Token {} un-expected while parsing macro arguments",
                    item.inner.as_ref()
                ),
                span: item.span,
            });
            None // assumed - unknown correct behavior due to errro
        }
    }
}

fn push_argument<'a>(arguments: &mut Arguments, item: &SpannedRef<'a>) {
    // get the top of the stack missing arguments

    if let Some(last) = arguments.last_mut() {
        last.push_back(item.clone().map(|x| x.to_string()))
    } else {
        arguments.push(VecDeque::from([item.clone().map(|x| x.to_string())]))
    };
}

fn replace(
    define_arguments: &[Spanned<String>],
    arguments: &[VecDeque<Spanned<String>>],
    tokens: VecDeque<Spanned<String>>,
) -> VecDeque<Spanned<String>> {
    // todo : check number of arguments is the same
    let replace = |arg: &String| {
        define_arguments
            .iter()
            .zip(arguments.iter())
            .find(|x| &x.0.inner == arg)
            .map(|x| x.1)
    };

    let mut new_tokens: VecDeque<Spanned<String>> = Default::default();
    for token in tokens {
        if let Some(tokens) = replace(&token.inner) {
            new_tokens.extend(tokens.iter().cloned())
        } else {
            new_tokens.push_back(token)
        }
    }

    new_tokens
}

fn evaluate(
    define: &mut Define,
    span: Span,
    arguments: &[VecDeque<Spanned<String>>],
    defines: &Defines,
    errors: &mut Vec<Error>,
    new_tokens: &mut VecDeque<Spanned<String>>,
) {
    let body = std::mem::take(&mut define.body);

    let body = replace(&define.arguments, arguments, body);

    let mut tokens = body.into_iter();

    while let Some(item) = tokens.next() {
        if let Some(new_define) = defines.get(item.inner.as_str()) {
            let arguments = if !new_define.arguments.is_empty() {
                get_arguments(&mut tokens, defines, errors)
            } else {
                vec![]
            };

            evaluate(
                &mut new_define.clone(),
                span,
                &arguments,
                defines,
                errors,
                new_tokens,
            );
        } else {
            new_tokens.push_back(item)
        }
    }

    if new_tokens.iter().any(|x| x.inner == "##") {
        // join tokens in pairs
        let mut merged = VecDeque::new();
        let mut next_merges = false;
        for token in new_tokens.iter_mut() {
            if token.inner == "##" {
                next_merges = true;
            } else if next_merges {
                if let Some(previous) = merged.back_mut() {
                    let Spanned { inner, .. } = previous;
                    *inner = format!("{}{}", inner, token.inner);
                } else {
                    merged.push_back(std::mem::take(token))
                }
                next_merges = false;
            } else {
                merged.push_back(std::mem::take(token))
            }
        }
        *new_tokens = merged;
    };

    if let Some(i) = new_tokens
        .iter()
        .enumerate()
        .find_map(|(i, x)| (x.inner.as_str() == "#").then_some(i))
    {
        // join all tokens into a single token
        let mut quoted = String::new();

        quoted.push('\"');
        for token in new_tokens.iter().skip(i + 1) {
            quoted.push_str(token.inner.as_str());
        }
        quoted.push('\"');
        new_tokens.truncate(i);

        new_tokens.push_back(Spanned {
            inner: quoted,
            span: (0, 0),
        });
    }

    new_tokens.iter_mut().for_each(|t| t.span = span);
}

fn get_arguments(
    tokens: &mut impl Iterator<Item = Spanned<String>>,
    defines: &Defines,
    errors: &mut Vec<Error>,
) -> Vec<VecDeque<Spanned<String>>> {
    use MacroState::*;
    let mut state = ParenthesisStart;
    let mut arguments = vec![];
    while let Some(item) = tokens.next() {
        match (state, item.inner.as_str()) {
            (Argument | Coma, ")") => break, // end state
            (ParenthesisStart, "(") => {
                // start state
                state = Argument;
                arguments.push(Default::default());
            }
            (Argument, ",") => {
                arguments.push(Default::default());
                state = Argument;
            }
            (Argument, name) => {
                if let Some(new_define) = defines.get(name) {
                    let define_arguments = if !new_define.arguments.is_empty() {
                        get_arguments(tokens, defines, errors)
                    } else {
                        vec![]
                    };

                    evaluate(
                        &mut new_define.clone(),
                        (0, 0),
                        &define_arguments,
                        defines,
                        errors,
                        arguments.last_mut().unwrap(),
                    )
                    // evaluate define and push into arguments
                } else {
                    arguments.last_mut().unwrap().push_back(item);
                }
            }
            _ => {
                errors.push(Error {
                    inner: format!(
                        "Un-expected token {} while parsing macro arguments",
                        item.inner.as_str()
                    ),
                    span: item.span,
                });
            }
        }
    }
    arguments
}
