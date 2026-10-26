use std::fmt::Debug;

use crate::{
    typecheck::environment::{InterfaceImpl, TypeClassId},
    typecheck::expr::Var,
    typecheck::typestore::Stack,
    typecheck::{
        constraint::ConstraintOrigin,
        typ::{type_id::TypeId, type_var::TypeVar},
    },
};

#[derive(Debug, PartialEq, Eq, Clone)]
pub enum InferenceErrorKind {
    NotEnoughArgs {
        expected: Stack,
        stack: Stack,
    },
    StacksDiffer {
        expected: Stack,
        found: Stack,
    },
    TypeParameterConstrained {
        var: TypeVar,
        tid: TypeId,
    },
    UnboundVar(TypeVar),
    TypesIncompatible {
        expected: TypeId,
        found: TypeId,
    },
    MutableRefToFunction(Var),
    PredicateNotMet {
        cid: TypeClassId,
        args: Vec<TypeId>,
    },
    ConflictingImpls {
        cid: TypeClassId,
        a: InterfaceImpl,
        b: InterfaceImpl,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InferenceError<M>
where
    M: Debug + Clone + PartialEq + Eq,
{
    pub kind: InferenceErrorKind,
    pub origin: ConstraintOrigin,
    pub meta: M,
}

impl<M> InferenceError<M>
where
    M: Debug + Clone + PartialEq + Eq,
{
    pub fn not_enough_args(
        expected: Stack,
        stack: Stack,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Self {
        Self {
            kind: InferenceErrorKind::NotEnoughArgs { expected, stack },
            origin: origin.clone(),
            meta: meta.clone(),
        }
    }
    pub fn stacks_differ(
        expected: Stack,
        found: Stack,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Self {
        Self {
            kind: InferenceErrorKind::StacksDiffer { expected, found },
            origin: origin.clone(),
            meta: meta.clone(),
        }
    }
    pub fn type_param_constrained(
        var: TypeVar,
        tid: TypeId,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Self {
        Self {
            kind: InferenceErrorKind::TypeParameterConstrained { var, tid },
            origin: origin.clone(),
            meta: meta.clone(),
        }
    }
    pub fn unbound_var(var: TypeVar, origin: &ConstraintOrigin, meta: &M) -> Self {
        Self {
            kind: InferenceErrorKind::UnboundVar(var),
            origin: origin.clone(),
            meta: meta.clone(),
        }
    }
    pub fn types_incompatible(
        expected: TypeId,
        found: TypeId,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Self {
        Self {
            kind: InferenceErrorKind::TypesIncompatible { expected, found },
            origin: origin.clone(),
            meta: meta.clone(),
        }
    }
    pub fn mutable_ref_to_fn(var: Var, origin: &ConstraintOrigin, meta: &M) -> Self {
        Self {
            kind: InferenceErrorKind::MutableRefToFunction(var),
            origin: origin.clone(),
            meta: meta.clone(),
        }
    }
    pub fn predicate_not_met(
        cid: TypeClassId,
        args: Vec<TypeId>,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Self {
        Self {
            kind: InferenceErrorKind::PredicateNotMet { cid, args },
            origin: origin.clone(),
            meta: meta.clone(),
        }
    }
    pub fn conflicting_impls(
        cid: TypeClassId,
        a: InterfaceImpl,
        b: InterfaceImpl,
        origin: &ConstraintOrigin,
        meta: &M,
    ) -> Self {
        Self {
            kind: InferenceErrorKind::ConflictingImpls { cid, a, b },
            origin: origin.clone(),
            meta: meta.clone(),
        }
    }
}
