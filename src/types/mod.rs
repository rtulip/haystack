mod constraint;
mod inference;
mod scheme;
use crate::{
    expr::Literal,
    passes::CType,
    stmt::{ExternDecl, Function},
};

pub use constraint::*;
pub use inference::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
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
pub enum Type {
    U32,
    U8,
    Bool,
    Var(TypeVar),
    Pointer { ty: Box<Self>, mutable: Mutability },
    Func(FunctionType),
    Struct(StructType),
    Never,
}

impl Type {
    pub fn is_free(&self) -> bool {
        match self {
            Type::Var(_) => true,
            Type::Pointer { ty, mutable } => ty.is_free(),
            Type::Func(FunctionType { input, output, .. }) => {
                input.iter().chain(output.iter()).any(|ty| ty.is_free())
            }
            Type::U32 | Type::U8 | Type::Bool | Type::Never => false,
            Type::Struct(_) => todo!(),
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

impl<'src, M, E> From<&'_ Function<'src, M, E>> for Type {
    fn from(value: &'_ Function<'src, M, E>) -> Self {
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

impl<'src> From<ExternDecl<'src>> for Type {
    fn from(value: ExternDecl<'src>) -> Self {
        Type::Func(FunctionType {
            input: value.input.into_iter().map(|ty| ty.into()).collect(),
            output: vec![value.output.into()],
            params: vec![],
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
            CType::Struct { .. } => todo!(),
            CType::Tuple(_) => todo!(),
            CType::Pointer(ty) => Type::pointer(Type::from(*ty)),
            CType::Int => Type::U32,
            CType::Variadic => Type::Never,
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
