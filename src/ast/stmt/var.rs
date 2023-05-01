use std::collections::HashMap;

use crate::{
    ast::expr::VarExpr,
    // backend::{InitData, UninitData},
    error::HayError,
    lex::token::Token,
    types::TypeId,
};

use super::{GlobalVars, StmtKind, UserDefinedTypes};

#[derive(Debug, Clone)]
pub struct VarStmt {
    pub token: Token,
    pub expr: VarExpr,
}

impl VarStmt {
    pub fn add_to_global_env(
        self,
        user_defined_types: &UserDefinedTypes,
        global_vars: &mut GlobalVars,
    ) -> Result<(), HayError> {
        let mut frame = vec![];
        self.expr.type_check(&mut frame, user_defined_types)?;

        if let Some((id, t)) = frame.pop() {
            if let Some(prev) = global_vars.insert(id, t) {
                todo!()
            }
        } else {
            todo!()
        }

        Ok(())
    }
}
