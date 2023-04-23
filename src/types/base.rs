use crate::error::HayError;
use std::convert::TryFrom;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BaseType {
    U64,
    U8,
    Char,
    Bool,
    Never,
}

impl TryFrom<&str> for BaseType {
    type Error = HayError;
    fn try_from(value: &str) -> Result<Self, Self::Error> {
        match value {
            "u64" => Ok(BaseType::U64),
            "u8" => Ok(BaseType::U8),
            "char" => Ok(BaseType::Char),
            "bool" => Ok(BaseType::Bool),
            "!" => Ok(BaseType::Never),
            _ => todo!("Err"),
        }
    }
}
