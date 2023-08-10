use std::collections::VecDeque;

use crate::error::Error;
use crate::preprocessor::iterator::MacroState;
use crate::span::{Span, Spanned};

use super::iterator::{Arguments, State};
use super::*;

pub fn update<'a>(state: &mut State, item: &SpannedRef<'a>) -> Option<()> {
    if let Some(define) = state.defines.get(item.inner.as_ref()) {
        // start of the state machine
        if define.arguments.is_none() {
            state.stack.clear();
            evaluate(
                &mut define.clone(),
                item.span,
                &[],
                &state.defines,
                &mut state.errors,
                &mut state.stack,
            );
        } else {
            // there is a define with this token => push it to the stack
            state.macro_stack.push((
                define.clone(),
                vec![],
                Spanned::new(MacroState::ParenthesisStart, item.span),
            ));
        }

        return Some(());
    }

    // if no macro being operated on, continue
    let Some((define, arguments, macro_state)) = state.macro_stack.last_mut() else {
        return None;
    };

    match (macro_state.inner, item.inner.as_ref()) {
        (MacroState::Argument(0), ")") => {
            // end state
            macro_state.inner = MacroState::None;
            macro_state.span.1 += item.span.1 - item.span.0;
            // evaluate define and push it to the stack
            state.stack.clear();
            evaluate(
                define,
                macro_state.span,
                arguments,
                &state.defines,
                &mut state.errors,
                &mut state.stack,
            );
            macro_state.span = (0, 0);
            state.macro_stack.pop();
            Some(())
        }
        // start state
        (MacroState::ParenthesisStart, _) => {
            macro_state.inner = MacroState::Argument(0);
            macro_state.span.1 += item.span.1 - item.span.0;
            Some(())
        }
        (MacroState::Argument(_), ",") => {
            arguments.push(Default::default());
            macro_state.span.1 += item.span.1 - item.span.0;
            Some(())
        }
        (MacroState::Argument(other), v) => {
            push_argument(arguments, item);
            macro_state.span.1 += item.span.1 - item.span.0;
            if v == "(" {
                macro_state.inner = MacroState::Argument(other + 1)
            };
            if v == ")" {
                macro_state.inner = MacroState::Argument(other - 1)
            };
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

fn quote(tokens: &VecDeque<Spanned<String>>) -> String {
    let mut quoted = String::new();
    quoted.push('\"');
    for token in tokens {
        quoted.push_str(token.inner.as_str());
    }
    quoted.push('\"');
    quoted
}

/// replaces all define arguments by arguments
fn replace(
    define_arguments: &[Spanned<String>],
    arguments: &[VecDeque<Spanned<String>>],
    tokens: VecDeque<Spanned<String>>,
) -> VecDeque<Spanned<String>> {
    // todo : check number of arguments is the same
    let replace = |arg: &str| {
        define_arguments
            .iter()
            .zip(arguments.iter())
            .find(|(key, _)| key.inner == arg)
            .map(|x| x.1)
    };

    let mut new_tokens: VecDeque<Spanned<String>> = Default::default();
    for token in tokens {
        let is_quote = token.inner.starts_with('#') && token.inner != "##" && token.inner != "#";
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
            new_tokens.push_back(token.map(|s| format!("\"{s}\"")))
        } else {
            new_tokens.push_back(token)
        };
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

    let body = if let Some(define_arguments) = &define.arguments {
        replace(define_arguments, arguments, body)
    } else {
        body
    };

    let mut tokens = body.into_iter();

    while let Some(item) = tokens.next() {
        if let Some(new_define) = defines.get(item.inner.as_str()) {
            let arguments = if new_define.arguments.is_some() {
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
            (Argument(0), ")") => {
                break;
            }
            (ParenthesisStart, "(") => {
                // start state
                state = Argument(0);
                arguments.push(Default::default());
            }
            (Argument(i), ",") => {
                arguments.push(Default::default());
                state = Argument(i);
            }
            (Argument(other), name) => {
                if let Some(new_define) = defines.get(name) {
                    let define_arguments = if new_define.arguments.is_some() {
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
                    if item.inner == "(" {
                        state = Argument(other + 1);
                    }
                    if item.inner == ")" {
                        state = Argument(other - 1);
                    }
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
