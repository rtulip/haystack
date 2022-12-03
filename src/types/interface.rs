use std::collections::HashMap;

use crate::lex::token::Token;

use super::TypeId;

#[derive(Debug, Clone)]
pub struct InterfaceBaseType {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<TypeId>,
    pub types: HashMap<TypeId, Token>,
    pub fns: Vec<String>,
}

impl InterfaceBaseType {
    pub fn id(&self) -> TypeId {
        let name = format!("{}", self.name.lexeme);
        TypeId::new(name)
    }
}
