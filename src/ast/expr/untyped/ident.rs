use std::collections::HashMap;

use crate::{
    ast::{
        expr::{TypedCallExpr, TypedExpr, TypedGetFrameExpr, TypedGlobalExpr},
        stmt::{
            Functions, GlobalVars, InterfaceFunctionTable, Interfaces, StmtKind, UserDefinedTypes,
        },
    },
    backend::Instruction,
    error::HayError,
    lex::token::Token,
    types::{Frame, FreeVars, Stack, Substitutions},
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
        user_defined_types: &UserDefinedTypes,
        free_vars: Option<&FreeVars>,
        global_vars: &GlobalVars,
        functions: &Functions,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
    ) -> Result<TypedExpr, HayError> {
        if let Some((i, (_, typ))) = frame
            .iter()
            .enumerate()
            .rev()
            .find(|(_, (id, _))| &self.ident.lexeme == id)
        {
            stack.push(typ.clone());
            return Ok(TypedExpr::Framed(TypedGetFrameExpr {
                frame: frame.clone(),
                idx: i,
                inner: None,
            }));
        }

        if let Some(iface_id) = interface_fn_table.get(&self.ident.lexeme) {
            let interface = interfaces.get(iface_id).expect("This sould be fine");
            let subs = interface.unify(
                &self.ident,
                stack,
                user_defined_types,
                free_vars,
                interfaces,
                &self.ident.lexeme,
            )?;

            return Ok(TypedExpr::Call(TypedCallExpr {
                func: self.ident.lexeme.clone(),
                subs,
            }));
        }

        if let Some(func) = functions.get(&self.ident.lexeme) {
            let subs = func.typ.unify(&self.ident, stack)?;
            return Ok(TypedExpr::Call(TypedCallExpr {
                func: self.ident.lexeme.clone(),
                subs,
            }));
        }

        if let Some(t) = global_vars.get(&self.ident.lexeme) {
            stack.push(t.clone());
            return Ok(TypedExpr::Global(TypedGlobalExpr {
                ident: self.ident.lexeme.clone(),
            }));
        }

        todo!("{}", self.ident);
    }
}
