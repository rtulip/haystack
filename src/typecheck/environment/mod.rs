use std::{collections::HashMap, fmt::Debug};

use crate::{
    typecheck::expr::Var,
    typecheck::typestore::{inference::InferenceError, TypeStore},
    typecheck::{
        constraint::ConstraintOrigin,
        typ::{type_id::TypeId, type_var::TypeVar},
    },
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord, Default)]
pub struct TypeClassId(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Coherence {
    Unique,
    Overlapping,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum Variance {
    Covariant,
    Invariant,
    Contravariant,
}

impl Variance {
    pub fn compose(self, inner: Self) -> Self {
        match (self, inner) {
            (_, Variance::Invariant) | (Variance::Invariant, _) => Variance::Invariant,
            (Variance::Covariant, Variance::Covariant) => Variance::Covariant,
            (Variance::Covariant, Variance::Contravariant) => Variance::Contravariant,
            (Variance::Contravariant, Variance::Covariant) => Variance::Contravariant,
            (Variance::Contravariant, Variance::Contravariant) => Variance::Covariant,
        }
    }

    pub fn combine(left: Option<Self>, right: Option<Self>) -> Option<Self> {
        match (left, right) {
            (None, None) => None,
            (None, Some(v)) | (Some(v), None) => Some(v),
            (Some(left), Some(right)) => {
                if left == right {
                    Some(left)
                } else {
                    Some(Variance::Invariant)
                }
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Interface {
    pub vars: Vec<TypeVar>,
    pub coherence: Coherence,
}

#[derive(Debug, Clone)]
pub struct InterfaceFunction {
    pub var: Var,
    pub scheme: Scheme,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InterfaceImpl {
    pub cid: TypeClassId,
    pub vars: Vec<TypeVar>,
    pub args: Vec<TypeId>,
    pub predicates: Vec<TypePredicate>,
}

#[derive(Debug, Clone)]
pub struct QualifiedInterfaceImpl {
    pub cid: TypeClassId,
    pub args: Vec<TypeId>,
    pub predicates: Vec<TypePredicate>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TypePredicate {
    pub cid: TypeClassId,
    pub args: Vec<TypeId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Scheme {
    pub vars: Vec<TypeVar>,
    pub tid: TypeId,
    pub predicates: Vec<TypePredicate>,
}

impl Scheme {
    pub fn new<Vars, Preds>(vars: Vars, tid: TypeId, preds: Preds) -> Scheme
    where
        Vars: Into<Vec<TypeVar>>,
        Preds: Into<Vec<TypePredicate>>,
    {
        Scheme {
            vars: vars.into(),
            tid,
            predicates: preds.into(),
        }
    }
}

pub struct QualifiedType {
    pub tid: TypeId,
    pub predicates: Vec<TypePredicate>,
}

/// Environment
///
/// This is a map from all variables in scope to an Type Scheme.
#[derive(Debug, Clone)]
pub struct Environment {
    pub typeclasses: Vec<Interface>,
    pub impls: HashMap<TypeClassId, Vec<InterfaceImpl>>,
    pub idents: HashMap<Var, Scheme>,
    next_var: usize,
}

impl Environment {
    pub fn new() -> Environment {
        Environment {
            typeclasses: vec![],
            impls: HashMap::new(),
            idents: HashMap::new(),
            next_var: 0,
        }
    }

    pub fn fresh_var(&mut self) -> Var {
        let v = self.next_var;
        self.next_var += 1;
        Var(v)
    }

    pub fn add_typeclass(&mut self, iface: Interface) -> TypeClassId {
        let cid = TypeClassId(self.typeclasses.len());
        self.typeclasses.push(iface);

        cid
    }

    fn impls_overlap<M>(
        &self,
        typestore: &mut TypeStore,
        a: &InterfaceImpl,
        b: &InterfaceImpl,
        meta: &M,
    ) -> Result<(), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        assert!(a.cid == b.cid);
        assert!(a.args.len() == b.args.len());

        if self.typeclasses[a.cid.0].coherence == Coherence::Overlapping {
            return Ok(());
        }

        let q_a = typestore.freshen_impl(a);
        let q_b = typestore.freshen_impl(b);
        if q_a
            .args
            .into_iter()
            .zip(q_b.args.into_iter())
            .all(|(a, b)| {
                typestore
                    .unify_ty_ty(a, b, &ConstraintOrigin::ConflictingImpls, meta)
                    .is_ok()
            })
        {
            Err(InferenceError::conflicting_impls(
                a.cid,
                a.clone(),
                b.clone(),
                &ConstraintOrigin::ConflictingImpls,
                meta,
            ))
        } else {
            Ok(())
        }
    }

    pub fn add_impl<M>(
        &mut self,
        typestore: &mut TypeStore,
        cid: TypeClassId,
        iface_impl: InterfaceImpl,
        meta: &M,
    ) -> Result<(), InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        assert_eq!(cid, iface_impl.cid);

        if let Some(impls) = self.impls.get(&cid) {
            impls
                .iter()
                .map(|i| self.impls_overlap(typestore, i, &iface_impl, meta))
                .collect::<Result<Vec<_>, _>>()?;
        }

        if let Some(impls) = self.impls.get_mut(&cid) {
            assert!(impls
                .iter()
                .find(|impl_| impl_.args == iface_impl.args)
                .is_none());
            impls.push(iface_impl);
        } else {
            self.impls.insert(cid, vec![iface_impl]);
        }
        Ok(())
    }

    pub fn get_var(&self, var: &Var) -> Option<&Scheme> {
        self.idents.get(var)
    }

    pub fn bind_scheme(&mut self, var: Var, scheme: Scheme) -> Option<Scheme> {
        self.idents.insert(var, scheme)
    }

    pub fn bind(&mut self, var: Var, tid: TypeId) -> Option<Scheme> {
        self.idents.insert(
            var,
            Scheme {
                vars: vec![],
                tid,
                predicates: vec![],
            },
        )
    }

    pub fn get_impls(&self, cid: &TypeClassId) -> Option<&Vec<InterfaceImpl>> {
        self.impls.get(cid)
    }
}
