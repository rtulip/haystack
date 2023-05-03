use std::collections::HashMap;

use crate::{
    ast::stmt::{
        FunctionDescription, Functions, GlobalVars, InterfaceFunctionTable, Interfaces, StmtKind,
        UserDefinedTypes,
    },
    error::HayError,
    lex::token::Token,
    types::{Frame, FreeVars, FunctionType, Stack, Substitutions, Type},
};

use super::Expr;

#[derive(Debug, Clone)]
pub struct ExprWhile {
    /// The token of the `while` keyword
    pub token: Token,
    /// The condition expressions before the body
    pub cond: Vec<Expr>,
    /// The body of the `while` loop.
    pub body: Box<Expr>,
}

impl ExprWhile {
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
        let stack_before = stack.clone();
        let frame_before = frame.clone();
        // Evaluate up to the body
        let mut typed_cond = vec![];
        for expr in &self.cond {
            typed_cond.push(expr.type_check(
                stack,
                frame,
                function,
                user_defined_types,
                global_vars,
                functions,
                interfaces,
                interface_fn_table,
                subs,
            )?);
        }

        if *frame != frame_before {
            return Err(HayError::new_type_err(
                "Frame cannot change within the while loop condition.",
                self.token.loc.clone(),
            ));
        }

        if stack.contains(&Type::never()) {
            *frame = frame_before;
            return Ok(());
        }

        FunctionType::new(vec![Type::bool()], vec![]).unify(&self.token, stack)?;
        let stack_after_check = stack.clone();

        self.body.type_check(
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

        if !stack.contains(&Type::never())
            && !stack
                .iter()
                .zip(stack_before.iter())
                .all(|(t1, t2)| t1 == t2)
        {
            return Err(HayError::new(
                "While loop must not change stack between iterations.",
                self.token.loc.clone(),
            )
            .with_hint(format!("Stack before loop: {stack_before:?}"))
            .with_hint(format!("Stack after loop:  {stack:?}")));
        }

        *frame = frame_before;
        *stack = stack_after_check;

        Ok(())
    }
}
