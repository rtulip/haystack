use crate::ir::Stack;
use serde::{Deserialize, Serialize};

#[derive(Default, Serialize, Deserialize, Clone, PartialEq)]
pub struct Type {
    pub name: String,
    pub ident: Option<String>,
}
impl Type {
    pub fn u64_t() -> Type {
        Type {
            name: String::from("u64"),
            ident: None,
        }
    }

    pub fn bool_t() -> Type {
        Type {
            name: String::from("bool"),
            ident: None,
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.name)
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ident {
            Some(ref s) => write!(f, "{}: {}", self.name, s),
            None => write!(f, "{}", self.name),
        }
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone, PartialEq)]
pub struct Signature {
    pub inputs: Stack,
    pub outputs: Stack,
}
