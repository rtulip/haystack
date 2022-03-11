use crate::ir::{Frame, Stack};
use serde::{Deserialize, Serialize};

#[derive(Default, Serialize, Deserialize, Clone)]
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

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct Signature {
    pub inputs: Stack,
    pub outputs: Stack,
}

impl Signature {
    pub fn into_stack(&self) -> (Stack, Frame) {
        let mut stack: Stack = vec![];
        let mut frame: Frame = vec![];
        if self.inputs.iter().any(|i| i.ident.is_some()) {
            self.inputs.iter().rev().for_each(|i| frame.push(i.clone()));
        } else {
            self.inputs.iter().for_each(|i| stack.push(i.clone()));
        }

        (stack, frame)
    }
}
