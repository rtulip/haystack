use std::{cmp::Ordering, fmt::Debug};

use crate::{
    typecheck::environment::TypePredicate,
    typecheck::typ::type_id::TypeId,
    typecheck::typestore::{inference::InferenceError, Stack},
};

use super::{expr::Var, typ::typ::Mutability};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Member {
    Named(String),
    Idx(usize),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintType {
    Equal(TypeId, TypeId),
    Subtype {
        subty: TypeId,
        superty: TypeId,
    },
    Join {
        output: TypeId,
        inputs: Vec<TypeId>,
    },
    Predicate(TypePredicate),
    Arity {
        expected: Vec<TypeId>,
        found: Vec<TypeId>,
    },
    HasEnoughArgs {
        expected: Vec<TypeId>,
        found: Vec<TypeId>,
    },
    HasMember {
        base_tid: TypeId,
        member: Member,
        member_tid: TypeId,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintOrigin {
    OutputSubtyping {
        expected: Stack,
        found: Stack,
    },
    FunctionCall {
        input: Stack,
        expected: Stack,
    },
    Identifier(TypeId),
    Branching {
        fresh_stack: Stack,
        stacks: Vec<Stack>,
    },
    TakeRef {
        var: Var,
        mutability: Mutability,
    },
    StructDef,
    EnumDef,
    UnboundVarInBody,
    ConflictingImpls,
    ImplsMustMatch,
    IfCondition,
    TypeInstance(TypeId),
    NamedMemberAccess(TypeId),
    WhileCondition,
    PlaceProjection,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Constraint<M>
where
    M: Debug + Clone + PartialEq + Eq,
{
    pub constraint: ConstraintType,
    pub origin: ConstraintOrigin,
    pub meta: M,
}

impl<M> Constraint<M>
where
    M: Debug + Clone + PartialEq + Eq,
{
    pub fn new(constraint: ConstraintType, origin: ConstraintOrigin, meta: &M) -> Constraint<M> {
        Constraint {
            constraint,
            origin,
            meta: meta.clone(),
        }
    }

    pub fn output_subtyping(expected_stack: &Stack, found_stack: &Stack, meta: &M) -> Vec<Self> {
        expected_stack
            .iter()
            .rev()
            .zip(found_stack.iter().rev())
            .map(|(expected, found)| {
                Constraint::new(
                    ConstraintType::Subtype {
                        subty: *found,
                        superty: *expected,
                    },
                    ConstraintOrigin::OutputSubtyping {
                        expected: expected_stack.clone(),
                        found: found_stack.clone(),
                    },
                    meta,
                )
            })
            .chain([Constraint::new(
                ConstraintType::Arity {
                    expected: expected_stack.clone(),
                    found: found_stack.clone(),
                },
                ConstraintOrigin::OutputSubtyping {
                    expected: expected_stack.clone(),
                    found: found_stack.clone(),
                },
                meta,
            )])
            .collect()
    }
}
