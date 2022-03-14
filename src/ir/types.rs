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

// #[derive(Default, Serialize, Deserialize, Clone, PartialEq)]
// pub struct Type {
//     pub name: String,
//     pub ident: Option<String>,
// }

// impl Type {
//     pub fn u64_t() -> Type {
//         Type {
//             name: String::from("u64"),
//             ident: None,
//         }
//     }

//     pub fn bool_t() -> Type {
//         Type {
//             name: String::from("bool"),
//             ident: None,
//         }
//     }

//     pub fn primitives_names() -> Vec<String> {
//         vec![Type::u64_t().name, Type::bool_t().name]
//     }
// }

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

// impl std::fmt::Display for Type {
//     fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
//         match self.ident {
//             Some(ref s) => write!(f, "{}: {}", self.name, s),
//             None => write!(f, "{}", self.name),
//         }
//     }
// }

#[derive(Default, Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Signature {
    pub inputs: Stack,
    pub outputs: Stack,
}
