use crate::{error::HayError, lex::token::Token};
use std::{convert::TryFrom, fmt::Display};

use super::{FunctionType, PointerType, Stack, Type, TypeVar};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BaseType {
    U64,
    U8,
    Char,
    Bool,
    Never,
}

impl BaseType {
    fn cast_u64(token: &Token, stack: &mut Stack) -> Result<(), HayError> {
        let fns = [
            FunctionType::new(vec![Type::u64()], vec![Type::u64()]),
            FunctionType::new(vec![Type::u8()], vec![Type::u64()]),
            FunctionType::new(vec![Type::char()], vec![Type::u64()]),
            FunctionType::new(vec![Type::bool()], vec![Type::u64()]),
            FunctionType::new(
                vec![Type::Pointer(PointerType {
                    mutable: false,
                    inner: Box::new(Type::TypeVar(TypeVar::new("T"))),
                })],
                vec![Type::u64()],
            ),
        ];

        FunctionType::unify_many(&fns, token, stack)
    }

    fn cast_char(token: &Token, stack: &mut Stack) -> Result<(), HayError> {
        let fns = [
            FunctionType::new(vec![Type::u64()], vec![Type::char()]),
            FunctionType::new(vec![Type::u8()], vec![Type::char()]),
            FunctionType::new(vec![Type::char()], vec![Type::char()]),
        ];

        FunctionType::unify_many(&fns, token, stack)
    }

    pub fn cast(&self, token: &Token, stack: &mut Stack) -> Result<(), HayError> {
        match self {
            BaseType::U64 => BaseType::cast_u64(token, stack),
            BaseType::Char => BaseType::cast_char(token, stack),
            _ => todo!("{self}"),
        }
    }
}

impl TryFrom<&str> for BaseType {
    type Error = ();
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "u64" => Ok(BaseType::U64),
            "u8" => Ok(BaseType::U8),
            "char" => Ok(BaseType::Char),
            "bool" => Ok(BaseType::Bool),
            "!" => Ok(BaseType::Never),
            _ => Err(()),
        }
    }
}

impl Display for BaseType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BaseType::U64 => write!(f, "u64"),
            BaseType::U8 => write!(f, "u8"),
            BaseType::Char => write!(f, "char"),
            BaseType::Bool => write!(f, "bool"),
            BaseType::Never => write!(f, "!"),
        }
    }
}
