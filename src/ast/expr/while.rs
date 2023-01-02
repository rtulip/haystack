use std::collections::HashMap;

use crate::{
    ast::stmt::StmtKind,
    error::HayError,
    lex::token::Token,
    types::{Frame, FramedType, Signature, Stack, Type, TypeId, TypeMap, UncheckedFunction},
};

use super::{Expr, TypedExpr};

#[derive(Debug, Clone)]
pub struct ExprWhile {
    /// The token of the `while` keyword
    pub token: Token,
    /// The condition expressions before the body
    pub cond: Vec<Expr>,
    /// The body of the `while` loop.
    pub body: Vec<Expr>,
}

impl ExprWhile {
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        func: &UncheckedFunction,
        global_env: &mut HashMap<String, (StmtKind, Signature)>,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let stack_before = stack.clone();
        let frame_before = frame.clone();
        // Evaluate up to the body
        let mut typed_cond = vec![];
        for expr in self.cond {
            typed_cond.push(expr.type_check(stack, frame, func, global_env, types, generic_map)?);
        }

        if *frame != frame_before {
            return Err(HayError::new_type_err(
                "Frame cannot change within the while loop condition.",
                self.token.loc,
            )
            .with_hint(format!(
                "Frame Before: {}",
                FramedType::frame_to_string(&frame_before)
            ))
            .with_hint(format!(
                "Frame After : {}",
                FramedType::frame_to_string(frame)
            )));
        }

        if stack.contains(&Type::Never.id()) {
            *frame = frame_before;
            return Ok(TypedExpr::While {
                cond: typed_cond,
                body: vec![],
            });
        }

        Signature::new(vec![Type::Bool.id()], vec![]).evaluate(&self.token, stack, types)?;

        let stack_after_check = stack.clone();

        let mut typed_body = vec![];
        for expr in self.body {
            typed_body.push(expr.type_check(stack, frame, func, global_env, types, generic_map)?);
        }

        if !stack.contains(&Type::Never.id())
            && (stack.len() != stack_before.len()
                || stack.iter().zip(&stack_before).any(|(t1, t2)| t1 != t2))
        {
            return Err(HayError::new(
                "While loop must not change stack between iterations.",
                self.token.loc,
            )
            .with_hint(format!("Stack before loop: {stack_before:?}"))
            .with_hint(format!("Stack after loop:  {stack:?}")));
        }

        *frame = frame_before;
        *stack = stack_after_check;

        Ok(TypedExpr::While {
            cond: typed_cond,
            body: typed_body,
        })
    }
}
