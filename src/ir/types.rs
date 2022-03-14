use crate::ir::Stack;
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Clone, PartialEq, Eq, Hash)]
pub enum Type {
    U64,
    Bool,
    Placeholder {
        name: String,
    },
    StructType {
        name: String,
        members: Vec<Type>,
        idents: Vec<Option<String>>,
    },
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::U64 | Type::Bool => 1,
            Type::Placeholder { .. } => panic!("Size of Placeholder types are unknown"),
            Type::StructType {
                name: _, members, ..
            } => members.iter().map(|t| t.size()).sum(),
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::U64 => write!(f, "u64"),
            Type::Bool => write!(f, "bool"),
            Type::Placeholder { name } => write!(f, "{name}"),
            Type::StructType {
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
