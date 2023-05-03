use std::collections::HashMap;

use crate::{
    ast::stmt::{
        Functions, GlobalVars, InterfaceFunctionTable, Interfaces, StmtKind, UserDefinedTypes,
    },
    error::HayError,
    lex::token::Token,
    types::{Frame, FreeVars, FunctionType, Stack, Substitutions, Type},
};

use super::Expr;

#[derive(Debug, Clone)]
pub struct IfExpr {
    /// Token of the `If` keyword
    pub token: Token,
    /// A list of expressions to execute if true.
    pub then: Box<Expr>,
    /// A list of expressions for each else-if case
    pub otherwise: Vec<ExprElseIf>,
    /// An optional final `else` case.
    pub finally: Option<Box<Expr>>,
}

impl IfExpr {
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
        let func = FunctionType::new(vec![Type::bool()], vec![]);
        func.unify(&self.token, stack)?;

        let initial_frame = frame.clone();
        let mut otherwise_stack = stack.clone();

        let mut end_stacks = vec![];
        let then_end_tok = self.then.token().clone();
        self.then.type_check(
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

        if !stack.contains(&Type::never()) {
            end_stacks.push((self.token.clone(), stack.clone()));
        }

        for case in &self.otherwise {
            let case_token = case.token.clone();
            *stack = otherwise_stack.clone();
            *frame = initial_frame.clone();

            case.type_check(
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

            otherwise_stack = stack.clone();

            if !stack.contains(&Type::never()) {
                end_stacks.push((case_token, stack.clone()));
            }
        }

        if let Some(finally) = &self.finally {
            *stack = otherwise_stack;
            *frame = initial_frame.clone();
            let tok = finally.token().clone();

            finally.type_check(
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

            if !stack.contains(&Type::never()) {
                end_stacks.push((tok, stack.clone()));
            }
        } else {
            *stack = otherwise_stack.clone();
            end_stacks.push((then_end_tok, otherwise_stack));
        }

        let mut resulting_stack = vec![];
        if !end_stacks.is_empty() {
            resulting_stack = end_stacks[0].1.clone();

            // if end_stacks
            //     .iter()
            //     .any(|(_, s)| s.len() != resulting_stack.len())
            //     || (1..end_stacks.len()).any(|i| {
            //         !end_stacks[i]
            //             .1
            //             .iter()
            //             .zip(resulting_stack.iter_mut())
            //             .all(|(t, r)| {
            //                 match (
            //                     t == r,
            //                     &t.supertype(types) == r,
            //                     t.supertype(types) == r.supertype(types),
            //                 ) {
            //                     (true, _, _) => true,
            //                     (_, true, _) => true,
            //                     (_, _, true) => {
            //                         *r = r.supertype(types);
            //                         true
            //                     }
            //                     _ => false,
            //                 }
            //             })
            //     })
            // {
            //     let mut err = HayError::new_type_err(
            //         "If block creates stacks of diferent shapes",
            //         self.token.loc,
            //     )
            //     .with_hint("Each branch of if block must evaluate to the same stack layout.");

            //     for (i, (tok, stk)) in end_stacks.iter().enumerate() {
            //         err = err.with_hint(format!("{} Branch {}: {:?}", tok.loc, i + 1, stk));
            //     }

            //     return Err(err);
            // }
        }

        *frame = initial_frame;
        if !end_stacks.is_empty() {
            *stack = resulting_stack;
        }
        // Ok(TypedExpr::If {
        //     then: Box::new(typed_then),
        //     otherwise: typed_otherwise,
        //     finally: typed_finally,
        // })

        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct ExprElseIf {
    /// Token of the `else` keyword
    pub token: Token,
    /// The expressions to evaluate before the next `if`.
    pub condition: Vec<Expr>,
    /// The body of the `else` expression
    pub block: Expr,
}

impl ExprElseIf {
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
        let mut typed_condition = vec![];
        for expr in &self.condition {
            typed_condition.push(expr.type_check(
                stack,
                frame,
                user_defined_types,
                free_vars,
                global_vars,
                functions,
                interfaces,
                interface_fn_table,
                subs,
            ));
        }

        FunctionType::new(vec![Type::bool()], vec![]).unify(&self.token, stack)?;

        // let stack_after_check = stack.clone();

        self.block.type_check(
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

        Ok(())
    }
}
