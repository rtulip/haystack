mod constraint;
mod inference;
mod scheme;
use crate::{expr::Literal, stmt::Function};

pub use constraint::*;
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

impl Type {
    pub fn expect_function(self) -> (Vec<Self>, Vec<Self>) {
        match self {
            Type::Func { input, output } => (input, output),
            ty => panic!("Expected a function, but found `{ty:?}`"),
        }
    }
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

impl<'src, M, E> From<&'_ Function<'src, M, E>> for Type {
    fn from(value: &'_ Function<'src, M, E>) -> Self {
        Type::Func {
            input: value.input.clone(),
            output: value.output.clone(),
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
