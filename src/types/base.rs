use crate::error::HayError;
use std::{convert::TryFrom, fmt::Display};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BaseType {
    U64,
    U8,
    Char,
    Bool,
    Never,
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
