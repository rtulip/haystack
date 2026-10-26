use std::fmt::Debug;

use crate::{
    typecheck::environment::Variance,
    typecheck::typestore::{inference::InferenceError, TypeStore},
    typecheck::{
        constraint::ConstraintOrigin,
        typ::{
            type_id::{EnumId, StructId, TypeId},
            type_var::TypeVar,
        },
    },
};

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinType {
    U8,
    U64,
    Bool,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionType {
    pub input: Vec<TypeId>,
    pub output: Vec<TypeId>,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Mutability {
    Mutable,
    Const,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PointerType {
    pub tid: TypeId,
    pub mutability: Mutability,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructType {
    pub sid: StructId,
    pub args: Vec<TypeId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct StructDef {
    pub vars: Vec<TypeVar>,
    pub variances: Vec<Variance>,
    pub members: Vec<TypeId>,
}

impl StructDef {
    pub fn new<M>(
        typestore: &TypeStore<M>,
        vars: Vec<TypeVar>,
        members: Vec<TypeId>,
        meta: &M,
    ) -> Result<StructDef, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let variances = vars
            .iter()
            .map(|var| {
                members
                    .iter()
                    .fold(None, |result, tid| {
                        Variance::combine(result, typestore.get_variance(*tid, *var))
                    })
                    .ok_or(InferenceError::unbound_var(
                        *var,
                        &ConstraintOrigin::StructDef,
                        meta,
                    ))
            })
            .collect::<Result<Vec<_>, InferenceError<M>>>()?;

        Ok(StructDef {
            vars,
            variances,
            members,
        })
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnumDef {
    pub vars: Vec<TypeVar>,
    pub variances: Vec<Variance>,
    pub variants: Vec<Option<TypeId>>,
}

impl EnumDef {
    pub fn new<M>(
        typestore: &TypeStore<M>,
        vars: Vec<TypeVar>,
        variants: Vec<Option<TypeId>>,
        meta: &M,
    ) -> Result<EnumDef, InferenceError<M>>
    where
        M: Debug + Clone + PartialEq + Eq,
    {
        let variances = vars
            .iter()
            .map(|var| {
                variants
                    .iter()
                    .fold(None, |result, tid| {
                        let variance = if let Some(tid) = tid {
                            typestore.get_variance(*tid, *var)
                        } else {
                            None
                        };
                        Variance::combine(result, variance)
                    })
                    .ok_or(InferenceError::unbound_var(
                        *var,
                        &ConstraintOrigin::EnumDef,
                        meta,
                    ))
            })
            .collect::<Result<Vec<_>, InferenceError<M>>>()?;

        Ok(EnumDef {
            vars,
            variances,
            variants,
        })
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct EnumType {
    pub eid: EnumId,
    pub args: Vec<TypeId>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct VariantType {
    pub eid: EnumId,
    pub args: Vec<TypeId>,
    pub variant: usize,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum Type {
    TypeVar(TypeVar),
    RigidTypeVar(TypeVar),
    Builtin(BuiltinType),
    Function(FunctionType),
    Pointer(PointerType),
    Struct(StructType),
    Enum(EnumType),
    Tuple(Vec<TypeId>),
    Variant(VariantType),
    Type(TypeId),
}

impl Type {
    pub fn bool() -> Type {
        Type::Builtin(BuiltinType::Bool)
    }

    pub fn u8() -> Type {
        Type::Builtin(BuiltinType::U8)
    }

    pub fn u64() -> Type {
        Type::Builtin(BuiltinType::U64)
    }
}
