use std::collections::HashMap;

use crate::{ast::arg::UntypedArg, lex::token::Token};

use super::TypeId;

#[derive(Debug, Clone)]
pub struct InterfaceBaseType {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<UntypedArg>,
    pub types: HashMap<TypeId, Token>,
}

impl InterfaceBaseType {
    pub fn id(&self) -> TypeId {
        let mut name = format!("{}<", self.name.lexeme);
        for arg in &self.annotations[0..self.annotations.len() - 1] {
            name = format!("{name}{} ", arg.token.lexeme);
        }
        name = format!("{name}{}>", self.annotations.last().unwrap().token.lexeme);
        TypeId::new(name)
    }
}
