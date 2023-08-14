use crate::types::*;

use super::{Output, Stack};

pub fn union_types(lhs: Option<Type>, rhs: Option<Type>) -> Option<Type> {
    union_output(lhs.map(|x| x.into()), rhs.map(|x| x.into())).map(|x| x.type_())
}

pub fn union_output(lhs: Option<Output>, rhs: Option<Output>) -> Option<Output> {
    match (lhs, rhs) {
        (Some(new), Some(old)) => (new.type_() == old.type_())
            .then_some(old)
            .or(Some(Type::Anything.into())),
        (None, Some(old)) => Some(old),
        (new, None) => new,
    }
}

pub fn union_stacks(lhs: Vec<Stack>, rhs: Vec<Stack>) -> Vec<Stack> {
    lhs.into_iter()
        .zip(rhs.into_iter())
        .map(|(mut lhs, rhs)| {
            for (key, (span, output)) in rhs.variables.into_iter() {
                lhs.variables
                    .entry(key.clone())
                    .and_modify(|(_, lhs)| {
                        *lhs = union_output(lhs.clone(), output.clone());
                    })
                    .or_insert((span, output));
            }
            lhs
        })
        .collect()
}
