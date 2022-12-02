use std::collections::HashMap;

use crate::{
    ast::arg::UntypedArg,
    error::HayError,
    lex::token::Token,
    types::{InterfaceBaseType, Type, TypeId, TypeMap},
};

use super::Stmt;

#[derive(Clone)]
pub struct InterfaceStmt {
    pub token: Token,
    pub name: Token,
    pub annotations: Vec<UntypedArg>,
    pub types: HashMap<TypeId, Token>,
    pub fns: Vec<Stmt>,
}

impl InterfaceStmt {
    pub fn add_to_global_scope(self, types: &mut TypeMap) -> Result<(), HayError> {
        let typ = Type::InterfaceBase(InterfaceBaseType {
            token: self.token,
            name: self.name.clone(),
            annotations: self.annotations,
            types: self.types,
        });
        let tid = typ.id();

        println!("tid: {tid}");

        if let Some(_) = types.insert(tid, typ) {
            return Err(HayError::new(
                format!(
                    "Interface name conflict: `{}` defined elsewhere.",
                    self.name.lexeme
                ),
                self.name.loc,
            ));
        }

        Ok(())
    }
}
