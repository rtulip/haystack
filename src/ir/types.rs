use crate::ir::Stack;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    U64,
    Bool,
    Ptr,
    Placeholder {
        name: String,
    },
    Struct {
        name: String,
        members: Vec<Type>,
        idents: Vec<Option<String>>,
    },
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::U64 | Type::Bool | Type::Ptr => 1,
            Type::Placeholder { .. } => panic!("Size of Placeholder types are unknown"),
            Type::Struct {
                name: _, members, ..
            } => members.iter().map(|t| t.size()).sum(),
        }
    }

    pub fn str() -> Self {
        Type::Struct {
            name: String::from("Str"),
            members: vec![Type::U64, Type::Ptr],
            idents: vec![Some(String::from("size")), Some(String::from("data"))],
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::U64 => write!(f, "u64"),
            Type::Bool => write!(f, "bool"),
            Type::Ptr => write!(f, "ptr"),
            Type::Placeholder { name } => write!(f, "{name}"),
            Type::Struct {
                name,
                members: _,
                idents: _,
            } => write!(f, "{name}"),
        }
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Signature {
    pub inputs: Stack,
    pub outputs: Stack,
}
