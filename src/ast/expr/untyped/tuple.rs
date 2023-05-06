//! Tuple Expressions
//!
//! This is the expression to build tuple literals
//! For example:
//!
//! ```haystack
//! main() {
//!     [1 "Hello Wolrd"] as [tuple]
//! //  ~~~~~~~~~~~~~~~~~
//! //          |
//! // This is the tuple expression
//! }
//! ```
//!
use std::collections::HashMap;

use crate::{
    ast::stmt::{
        FunctionDescription, Functions, GlobalVars, InterfaceFunctionTable, Interfaces, StmtKind,
        UserDefinedTypes,
    },
    error::HayError,
    lex::token::Token,
    types::{Frame, FreeVars, RecordType, Stack, Substitutions, Type},
};

use super::Expr;

#[derive(Debug, Clone)]
pub struct TupleExpr {
    /// Token of the `as` keyword
    pub token: Token,
    /// The non-empty list of identifiers.
    pub exprs: Vec<Expr>,
}

impl TupleExpr {
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
    ) -> Result<(), HayError> {
        let mut sub_stack = vec![];
        for e in &self.exprs {
            e.type_check(
                &mut sub_stack,
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

        let tuple = RecordType::tuple(sub_stack);
        stack.push(Type::Record(tuple));
        Ok(())
    }
}
