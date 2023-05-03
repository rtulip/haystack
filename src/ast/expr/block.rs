use std::collections::HashMap;

use crate::{
    ast::stmt::{Functions, GlobalVars, InterfaceFunctionTable, Interfaces, UserDefinedTypes},
    error::HayError,
    lex::token::Token,
    types::{Frame, FreeVars, Stack, Substitutions},
};

use super::Expr;

#[derive(Debug, Clone)]
pub struct BlockExpr {
    pub open: Token,
    pub close: Token,
    pub exprs: Vec<Expr>,
}

impl BlockExpr {
    pub fn type_check(
        &self,
        stack: &mut Stack,
        frame: &mut Frame,
        user_defined_types: &UserDefinedTypes,
        free_vars: &FreeVars,
        global_vars: &GlobalVars,
        functions: &Functions,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
        subs: &mut Substitutions,
    ) -> Result<(), HayError> {
        for e in &self.exprs {
            println!("  {stack:?}");
            e.type_check(
                stack,
                frame,
                user_defined_types,
                free_vars,
                global_vars,
                functions,
                interfaces,
                interface_fn_table,
                subs,
            )?;
        }

        Ok(())
    }
}
