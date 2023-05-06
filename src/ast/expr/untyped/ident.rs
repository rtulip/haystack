use std::collections::HashMap;

use crate::{
    ast::stmt::{
        Functions, GlobalVars, InterfaceFunctionTable, Interfaces, StmtKind, UserDefinedTypes,
    },
    backend::Instruction,
    error::HayError,
    lex::token::Token,
    types::{Frame, Stack, Substitutions},
};

#[derive(Debug, Clone)]
pub struct IdentExpr {
    pub ident: Token,
}

impl IdentExpr {
    pub fn type_check(
        &self,
        stack: &mut Stack,
        frame: &Frame,
        global_vars: &GlobalVars,
        functions: &Functions,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
    ) -> Result<(), HayError> {
        if let Some((i, (_, typ))) = frame
            .iter()
            .enumerate()
            .rev()
            .find(|(_, (id, _))| &self.ident.lexeme == id)
        {
            stack.push(typ.clone());
            return Ok(());
            // return Ok(TypedExpr::Framed {
            //     frame: frame.clone(),
            //     idx: i,
            //     inner: None,
            // });
        }

        if let Some(iface_id) = interface_fn_table.get(&self.ident.lexeme) {
            let interface = interfaces.get(iface_id).expect("This sould be fine");
            interface.unify(&self.ident.lexeme, &self.ident, stack)?;
            return Ok(());
        }

        if let Some(func) = functions.get(&self.ident.lexeme) {
            func.typ.unify(&self.ident, stack)?;
            return Ok(());
        }

        if let Some(t) = global_vars.get(&self.ident.lexeme) {
            stack.push(t.clone());
            return Ok(());
        }

        todo!("{}", self.ident);
    }
}
