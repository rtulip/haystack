mod constraint;
mod inference;
mod scheme;
use std::collections::HashMap;

use crate::{
    c::CType,
    expr::Literal,
    lex::LexType,
    stmt::{Function, TypeParameter},
};

pub use constraint::*;
pub use inference::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct TypeVar(pub usize);

impl From<usize> for TypeVar {
    fn from(value: usize) -> Self {
        TypeVar(value)
    }
}

impl Into<usize> for TypeVar {
    fn into(self) -> usize {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Mutability {
    Mutable,
    Constant,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionType {
    pub input: Vec<Type>,
    pub output: Vec<Type>,
    pub params: Vec<Type>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StructType {
    pub name: String,
    pub members: Vec<(String, Type)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct TaggedUnionType {
    pub name: String,
    pub variants: Vec<(String, Type)>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    U32,
    U8,
    Bool,
    Var(TypeVar),
    Pointer { ty: Box<Self>, mutable: Mutability },
    Func(FunctionType),
    Struct(StructType),
    TaggedUnion(TaggedUnionType),
    Tuple(Vec<Self>),
    Never,
}

impl Type {
    pub fn builtins() -> HashMap<&'static str, Type> {
        HashMap::from([
            ("u32", Type::U32),
            ("u8", Type::U8),
            ("bool", Type::Bool),
            ("!", Type::Never),
        ])
    }

    pub fn is_free(&self) -> bool {
        match self {
            Type::Var(_) => true,
            Type::Pointer { ty, mutable } => ty.is_free(),
            Type::Func(FunctionType { input, output, .. }) => {
                input.iter().chain(output.iter()).any(|ty| ty.is_free())
            }
            Type::U32 | Type::U8 | Type::Bool | Type::Never => false,
            Type::Struct(strukt) => strukt.members.iter().any(|(_, ty)| ty.is_free()),
            Type::TaggedUnion(tagged_union) => {
                tagged_union.variants.iter().any(|(_, ty)| ty.is_free())
            }
            Type::Tuple(tys) => tys.iter().any(|ty| ty.is_free()),
        }
    }

    pub fn free_vars(&self) -> Vec<TypeVar> {
        match self {
            Type::U32 => vec![],
            Type::U8 => vec![],
            Type::Bool => vec![],
            Type::Var(var) => vec![var.clone()],
            Type::Pointer { ty, mutable } => ty.free_vars(),
            Type::Func(_) => todo!(),
            Type::Struct(st) => st
                .members
                .iter()
                .map(|(_, ty)| ty.free_vars())
                .flatten()
                .collect(),
            Type::TaggedUnion(tu) => tu
                .variants
                .iter()
                .map(|(_, ty)| ty.free_vars())
                .flatten()
                .collect(),
            Type::Tuple(_) => todo!(),
            Type::Never => vec![],
        }
    }

    pub fn expect_function(self) -> FunctionType {
        match self {
            Type::Func(f) => f,
            ty => panic!("Expected a function, but found `{ty:?}`"),
        }
    }

    pub fn pointer(ty: Self) -> Self {
        Type::Pointer {
            ty: Box::new(ty),
            mutable: Mutability::Constant,
        }
    }

    pub fn string() -> Self {
        Type::Struct(StructType {
            name: "Str".into(),
            members: vec![
                ("size".into(), Type::U32),
                ("data".into(), Type::pointer(Type::U8)),
            ],
        })
    }

    pub fn access_inner(&self, inners: &[&str]) -> Result<&Self, ()> {
        assert!(!inners.is_empty());

        match self {
            Type::Struct(strukt) => {
                if let Some((_, ty)) = strukt.members.iter().find(|(id, _)| id == inners[0]) {
                    if inners.len() == 1 {
                        Ok(ty)
                    } else {
                        ty.access_inner(&inners[1..])
                    }
                } else {
                    Err(())
                }
            }
            Type::TaggedUnion(_) => todo!(),
            Type::Tuple(_) => todo!(),
            Type::Never
            | Type::U32
            | Type::U8
            | Type::Bool
            | Type::Var(_)
            | Type::Pointer { .. }
            | Type::Func(_) => Err(()),
        }
    }

    pub fn linear_substitution<'src>(
        mut self,
        free_vars: Vec<TypeParameter<'src>>,
        params: Vec<Type>,
    ) -> Result<Self, ()> {
        if free_vars.len() != params.len() {
            return Err(());
        }

        free_vars
            .iter()
            .zip(params.iter())
            .for_each(|(var, replace)| self.var_replace(&var.tyvar, replace));
        Ok(self)
    }

    fn var_replace(&mut self, var: &TypeVar, replace: &Self) {
        match self {
            Type::U32 | Type::U8 | Type::Bool => (),
            Type::Var(v) => {
                if v == var {
                    *self = replace.clone();
                }
            }
            Type::Pointer { ty, mutable } => ty.var_replace(var, replace),
            Type::Func(_) => todo!(),
            Type::Struct(s) => s
                .members
                .iter_mut()
                .for_each(|(_, ty)| ty.var_replace(var, replace)),
            Type::TaggedUnion(tu) => tu
                .variants
                .iter_mut()
                .for_each(|(_, ty)| ty.var_replace(var, replace)),
            Type::Tuple(_) => todo!(),
            Type::Never => todo!(),
        }
    }
}

pub type Stack = Vec<Type>;

impl<'src> From<&'_ Literal<'src>> for Type {
    fn from(value: &Literal<'src>) -> Self {
        match value {
            Literal::U32(_) => Type::U32,
            Literal::String(_) => Type::string(),
            Literal::Bool(_) => Type::Bool,
            Literal::U8(_) => Type::U8,
        }
    }
}

impl<'src, M, E> From<&'_ Function<'src, Type, M, E>> for Type {
    fn from(value: &'_ Function<'src, Type, M, E>) -> Self {
        Type::Func(FunctionType {
            input: value.input.clone(),
            output: value.output.clone(),
            params: value
                .type_vars
                .iter()
                .map(|tv| Type::from(&tv.tyvar))
                .collect(),
        })
    }
}

impl<'a> From<&'a TypeVar> for Type {
    fn from(value: &'a TypeVar) -> Self {
        Type::Var(value.clone())
    }
}

impl<'src> From<CType> for Type {
    fn from(value: CType) -> Self {
        match value {
            CType::U32 => Type::U32,
            CType::Bool => Type::Bool,
            CType::U8 => Type::U8,
            CType::Char => Type::U8,
            CType::Void => Type::Never,
            CType::Struct { name, elements } => Type::Struct(StructType {
                name: name,
                members: elements
                    .into_iter()
                    .map(|(id, ty)| (id, Type::from(ty)))
                    .collect(),
            }),
            CType::TaggedUnion { .. } => todo!(),
            CType::Tuple(_) => todo!(),
            CType::Pointer(ty) => Type::pointer(Type::from(*ty)),
            CType::Int => Type::U32,
            CType::Variadic => Type::Never,
            CType::Function { .. } => todo!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Var {
    Func(usize),
    Ident(usize),
}

impl Var {
    pub fn func(&self) -> &usize {
        match self {
            Var::Func(f) => &f,
            Var::Ident(_) => panic!(),
        }
    }
}
