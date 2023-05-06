use std::collections::HashMap;

use crate::{
    ast::{
        expr::TypedExpr,
        stmt::{
            FunctionDescription, Functions, GlobalVars, InterfaceFunctionTable, Interfaces,
            UserDefinedTypes,
        },
    },
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
        function: &FunctionDescription,
        user_defined_types: &UserDefinedTypes,
        global_vars: &GlobalVars,
        functions: &Functions,
        interfaces: &Interfaces,
        interface_fn_table: &InterfaceFunctionTable,
        subs: &mut Substitutions,
    ) -> Result<TypedExpr, HayError> {
        for e in &self.exprs {
            e.type_check(
                stack,
                frame,
                function,
                user_defined_types,
                global_vars,
                functions,
                interfaces,
                interface_fn_table,
                subs,
            )?;
        }

        Ok(todo!())
    }
}
