mod constraint;
mod inference;
mod scheme;
use crate::expr::Literal;

pub use inference::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeVar(usize);

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
pub enum Type {
    U32,
    U8,
    Bool,
    Var(TypeVar),
    Pointer { ty: Box<Self>, mutable: Mutability },
    Func { input: Vec<Self>, output: Vec<Self> },
    String,
    Never,
}

pub type Stack = Vec<Type>;

impl<'src> From<&'_ Literal<'src>> for Type {
    fn from(value: &Literal<'src>) -> Self {
        match value {
            Literal::U32(_) => Type::U32,
            Literal::String(_) => Type::String,
            Literal::Bool(_) => Type::Bool,
            Literal::U8(_) => Type::U8,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Var {
    Func(usize),
    Ident(usize),
}
