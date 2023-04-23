use std::fmt::Display;

use super::{Type, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RecordKind {
    Enum,
    Struct,
    EnumStruct,
    Tuple,
    Union,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct RecordType {
    pub kind: RecordKind,
    pub ident: Option<TypeId>,
    pub members: Vec<(String, Type)>,
}

impl Display for RecordKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            RecordKind::Enum => write!(f, "enum"),
            RecordKind::Struct => write!(f, "struct"),
            RecordKind::EnumStruct => write!(f, "enum struct"),
            RecordKind::Tuple => write!(f, "tuple"),
            RecordKind::Union => write!(f, "union"),
        }
    }
}
