use std::{fmt::Debug, matches, panic};

use crate::typecheck::{
    constraint::ConstraintOrigin,
    environment::{TypeClassId, Variance},
    typ::{
        type_id::{EnumId, StructId, TypeId},
        type_var::TypeVar,
    },
    typestore::{inference::InferenceError, TypeStore},
};

pub type Stack = Vec<TypeId>;
#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum StackState {
    Reachable(Stack),
    Unreachable,
}

impl StackState {
    pub fn append_output(&mut self, output: StackState) {
        match (self, output) {
            (Self::Reachable(stack), Self::Reachable(output)) => {
                stack.extend(output);
            }

            (stack, Self::Unreachable) => {
                *stack = Self::Unreachable;
            }

            (Self::Unreachable, _) => (),
        }
    }

    pub fn split_off(&mut self, at: usize) -> Option<Stack> {
        match self {
            StackState::Reachable(stack) => Some(stack.split_off(at)),
            StackState::Unreachable => None,
        }
    }

    pub fn pop(&mut self) -> Option<TypeId> {
        match self {
            StackState::Reachable(stack) => stack.pop(),
            StackState::Unreachable => None,
        }
    }

    pub fn push(&mut self, tid: TypeId) {
        match self {
            StackState::Reachable(stack) => stack.push(tid),
            StackState::Unreachable => (),
        }
    }

    pub fn reachable(&self) -> &Stack {
        match self {
            StackState::Reachable(stack) => stack,
            StackState::Unreachable => panic!(),
        }
    }

    pub fn is_reachable(&self) -> bool {
        matches!(self, StackState::Reachable(_))
    }
}

impl<S> From<S> for StackState
where
    S: Into<Stack>,
{
    fn from(value: S) -> Self {
        StackState::Reachable(value.into())
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub enum BuiltinType {
    U8,
    U64,
    Bool,
    Char,
    CVarArgs,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunctionType {
    pub input: Vec<TypeId>,
    pub output: StackState,
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
pub struct Projection {
    pub cid: TypeClassId,
    pub args: Vec<TypeId>,
    pub idx: usize,
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
    Projection(Projection),
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

    pub fn char() -> Type {
        Type::Builtin(BuiltinType::Char)
    }

    pub fn is_c_element(&self) -> bool {
        match self {
            Type::TypeVar(_)
            | Type::RigidTypeVar(_)
            | Type::Builtin(BuiltinType::CVarArgs)
            | Type::Struct(_)
            | Type::Enum(_)
            | Type::Variant(_)
            | Type::Type(_)
            | Type::Projection(_)
            | Type::Tuple(_) => false,

            Type::Builtin(BuiltinType::U8)
            | Type::Builtin(BuiltinType::U64)
            | Type::Builtin(BuiltinType::Bool)
            | Type::Builtin(BuiltinType::Char)
            | Type::Function(_)
            | Type::Pointer(_) => true,
        }
    }
}
