use std::collections::hash_map::HashMap;

use tinyvec::{array_vec, ArrayVec};

use crate::database;
use crate::types::{Signature, Type};

type Types = ArrayVec<[Type; 5]>;

fn _build_binary() -> HashMap<&'static str, HashMap<(Type, Type), Types>> {
    let mut r: HashMap<&'static str, HashMap<(Type, Type), Types>> = Default::default();
    for s in database::BINARY {
        if let Signature::Binary(lhs, name, rhs, type_, _) = s {
            r.entry(name)
                .and_modify(|e| {
                    e.entry((lhs, rhs))
                        .and_modify(|e| e.push(type_))
                        .or_insert_with(|| array_vec!([Type; 5] => type_));
                })
                .or_insert_with(|| HashMap::from([((lhs, rhs), array_vec!([Type; 5] => type_))]));
        }
    }
    r
}

fn _build_unary() -> HashMap<&'static str, HashMap<Type, Types>> {
    let mut r: HashMap<&'static str, HashMap<Type, Types>> = Default::default();
    for s in database::UNARY {
        if let Signature::Unary(name, rhs, type_, _) = s {
            r.entry(name)
                .and_modify(|e| {
                    e.entry(rhs)
                        .and_modify(|e| e.push(type_))
                        .or_insert_with(|| array_vec!([Type; 5] => type_));
                })
                .or_insert_with(|| HashMap::from([(rhs, array_vec!([Type; 5] => type_))]));
        }
    }
    r
}

fn _build_nullary() -> HashMap<&'static str, Type> {
    let mut r: HashMap<&'static str, Type> = Default::default();
    for s in database::NULLARY {
        if let Signature::Nullary(name, type_, _) = s {
            r.insert(name, type_);
        }
    }
    r
}

lazy_static::lazy_static! {

    pub static ref BINARY: HashMap<&'static str, HashMap<(Type, Type), Types>> = {
        _build_binary()
    };

    pub static ref UNARY: HashMap<&'static str, HashMap<Type, Types>> = {
        _build_unary()
    };

    pub static ref NULLARY: HashMap<&'static str, Type> = {
        _build_nullary()
    };
}

/// Takes the first element from the options if it is the only possible element in those options
/// and its only option only has one type
pub fn take_only<'a>(options: impl Iterator<Item = &'a Types>) -> Option<Type> {
    let mut current_option = None::<Type>;
    for option in options {
        assert!(!option.is_empty());
        if option.len() == 1 {
            if current_option.is_none() {
                current_option = Some(option[0]);
            } else {
                // there is more than one type => Anything (since we do not have Union type yet)
                return Some(Type::Anything);
            }
        } else {
            // by construction `option` contains unique types. Thus, more than one option exists => Anything (since we do not have Union type yet)
            return Some(Type::Anything);
        }
    }

    current_option
}
