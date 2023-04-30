use std::collections::HashMap;

use crate::{
    ast::stmt::{Functions, InterfaceFunctionTable, Interfaces, UserDefinedTypes},
    error::HayError,
    lex::token::Token,
    types::{Frame, Stack, Substitutions},
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
        types: &UserDefinedTypes,
        stack: &mut Stack,
        frame: &mut Frame,
        functions: &Functions,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
        subs: &mut Substitutions,
    ) -> Result<(), HayError> {
        for e in &self.exprs {
            println!("  {stack:?}");
            e.type_check(
                types,
                stack,
                frame,
                functions,
                interfaces,
                interface_fn_table,
                subs,
            )?;
        }

        Ok(())
    }
}
