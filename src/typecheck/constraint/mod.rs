use std::{cmp::Ordering, fmt::Debug};

use crate::{
    typecheck::environment::TypePredicate,
    typecheck::typ::type_id::TypeId,
    typecheck::typestore::{inference::InferenceError, Stack},
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintType {
    Equal(TypeId, TypeId),
    Subtype { subty: TypeId, superty: TypeId },
    Join { output: TypeId, inputs: Vec<TypeId> },
    Predicate(TypePredicate),
}

impl PartialOrd for ConstraintType {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // Equal < Subtype < Join < Predicate
        Some(match (self, other) {
            (ConstraintType::Equal(_, _), ConstraintType::Equal(_, _)) => Ordering::Equal,
            (ConstraintType::Equal(_, _), ConstraintType::Subtype { .. }) => Ordering::Less,
            (ConstraintType::Equal(_, _), ConstraintType::Join { .. }) => Ordering::Less,
            (ConstraintType::Equal(_, _), ConstraintType::Predicate(_)) => Ordering::Less,
            (ConstraintType::Subtype { .. }, ConstraintType::Equal(_, _)) => Ordering::Greater,
            (ConstraintType::Subtype { .. }, ConstraintType::Subtype { .. }) => Ordering::Equal,
            (ConstraintType::Subtype { .. }, ConstraintType::Join { .. }) => Ordering::Less,
            (ConstraintType::Subtype { .. }, ConstraintType::Predicate(_)) => Ordering::Less,
            (ConstraintType::Join { .. }, ConstraintType::Equal(_, _)) => Ordering::Greater,
            (ConstraintType::Join { .. }, ConstraintType::Subtype { .. }) => Ordering::Greater,
            (ConstraintType::Join { .. }, ConstraintType::Join { .. }) => Ordering::Equal,
            (ConstraintType::Join { .. }, ConstraintType::Predicate(_)) => Ordering::Less,
            (ConstraintType::Predicate(_), ConstraintType::Equal(_, _)) => Ordering::Greater,
            (ConstraintType::Predicate(_), ConstraintType::Subtype { .. }) => Ordering::Greater,
            (ConstraintType::Predicate(_), ConstraintType::Join { .. }) => Ordering::Greater,
            (ConstraintType::Predicate(_), ConstraintType::Predicate(_)) => Ordering::Equal,
        })
    }
}

impl Ord for ConstraintType {
    // Equal < Subtype < Join < Predicate
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]

pub struct Constraint<M>
where
    M: Debug + Clone + PartialEq + Eq,
{
    pub constraint: ConstraintType,
    pub meta: M,
}

impl<M> Constraint<M>
where
    M: Debug + Clone + PartialEq + Eq,
{
    pub fn new(constraint: ConstraintType, meta: &M) -> Constraint<M> {
        Constraint {
            constraint,
            meta: meta.clone(),
        }
    }

    pub fn output_subtyping(
        expected: &Stack,
        found: &Stack,
        meta: &M,
    ) -> Result<Vec<Self>, InferenceError<M>> {
        if expected.len() != found.len() {
            return Err(InferenceError::stacks_differ(
                expected.clone(),
                found.clone(),
                meta,
            ));
        }

        Ok(expected
            .iter()
            .zip(found.iter())
            .map(|(expected, found)| {
                Constraint::new(
                    ConstraintType::Subtype {
                        subty: *found,
                        superty: *expected,
                    },
                    meta,
                )
            })
            .collect())
    }
}

impl<M> PartialOrd for Constraint<M>
where
    M: Debug + Clone + PartialEq + Eq,
{
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        // Equal < Subtype < Join < Predicate
        self.constraint.partial_cmp(&other.constraint)
    }
}

impl<M> Ord for Constraint<M>
where
    M: Debug + Clone + PartialEq + Eq,
{
    // Equal < Subtype < Join < Predicate
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.partial_cmp(other).unwrap()
    }
}
