use std::collections::HashMap;

use crate::{
    ast::stmt::StmtKind,
    error::HayError,
    lex::token::Token,
    types::{Frame, Signature, Stack, Type, TypeId, TypeMap, UncheckedFunction},
};

use super::{Expr, TypedExpr};

#[derive(Debug, Clone)]
pub struct ExprIf {
    /// Token of the `If` keyword
    pub token: Token,
    /// A list of expressions to execute if true.
    pub then: Vec<Expr>,
    /// A list of expressions for each else-if case
    pub otherwise: Vec<ExprElseIf>,
    /// An optional final `else` case.
    pub finally: Option<Vec<Expr>>,
}

impl ExprIf {
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        func: &UncheckedFunction,
        global_env: &mut HashMap<String, (StmtKind, Signature)>,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let sig = Signature::new(vec![Type::Bool.id()], vec![]);
        sig.evaluate(&self.token, stack, types)?;
        let initial_frame = frame.clone();
        let mut otherwise_stack = stack.clone();

        let mut end_stacks = vec![];

        let mut typed_then = vec![];
        let then_end_tok = match self.then.iter().last() {
            Some(e) => e.token().clone(),
            None => self.token.clone(),
        };
        for e in self.then {
            typed_then.push(e.type_check(stack, frame, func, global_env, types, generic_map)?);
        }

        if !stack.contains(&Type::Never.id()) {
            end_stacks.push((self.token.clone(), stack.clone()));
        }

        let mut typed_otherwise = vec![];
        for case in self.otherwise {
            let case_token = case.token.clone();
            *stack = otherwise_stack.clone();
            *frame = initial_frame.clone();

            let (typed_expr, stack_after_check) =
                case.type_check(stack, frame, types, func, global_env, generic_map)?;

            otherwise_stack = stack_after_check;

            typed_otherwise.push(typed_expr);

            if !stack.contains(&Type::Never.id()) {
                end_stacks.push((case_token, stack.clone()));
            }
        }

        let mut typed_finally = None;
        if let Some(finally) = self.finally {
            let mut tmp = vec![];
            *stack = otherwise_stack.clone();
            *frame = initial_frame.clone();
            if !finally.is_empty() {
                let first_tok = finally[0].token().clone();

                for e in finally {
                    tmp.push(e.type_check(stack, frame, func, global_env, types, generic_map)?);
                }
                typed_finally = Some(tmp);

                if !stack.contains(&Type::Never.id()) {
                    end_stacks.push((first_tok, stack.clone()));
                }
            } else {
                *stack = otherwise_stack.clone();
                end_stacks.push((then_end_tok, otherwise_stack));
            }
        } else {
            *stack = otherwise_stack.clone();
            end_stacks.push((then_end_tok, otherwise_stack));
        }

        let mut resulting_stack = vec![];

        if !end_stacks.is_empty() {
            resulting_stack = end_stacks[0].1.clone();
            if end_stacks
                .iter()
                .any(|(_, s)| s.len() != resulting_stack.len())
                || (1..end_stacks.len()).into_iter().any(|i| {
                    !end_stacks[i]
                        .1
                        .iter()
                        .zip(resulting_stack.iter_mut())
                        .all(|(t, r)| {
                            match (
                                t == r,
                                &t.supertype(types) == r,
                                t.supertype(types) == r.supertype(types),
                            ) {
                                (true, _, _) => true,
                                (_, true, _) => true,
                                (_, _, true) => {
                                    *r = r.supertype(types);
                                    true
                                }
                                _ => false,
                            }
                        })
                })
            {
                let mut err = HayError::new_type_err(
                    "If block creates stacks of diferent shapes",
                    self.token.loc,
                )
                .with_hint("Each branch of if block must evaluate to the same stack layout.");

                for (i, (tok, stk)) in end_stacks.iter().enumerate() {
                    err = err.with_hint(format!("{} Branch {}: {:?}", tok.loc, i + 1, stk));
                }

                return Err(err);
            }
        }

        *frame = initial_frame;
        if !end_stacks.is_empty() {
            *stack = resulting_stack;
        }
        Ok(TypedExpr::If {
            then: typed_then,
            otherwise: typed_otherwise,
            finally: typed_finally,
        })
    }
}

#[derive(Debug, Clone)]
pub struct ExprElseIf {
    /// Token of the `else` keyword
    pub token: Token,
    /// The expressions to evaluate before the next `if`.
    pub condition: Vec<Expr>,
    /// The body of the `else` expression
    pub block: Vec<Expr>,
}

impl ExprElseIf {
    fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        types: &mut TypeMap,
        func: &UncheckedFunction,
        global_env: &mut HashMap<String, (StmtKind, Signature)>,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<(TypedExpr, Stack), HayError> {
        let mut typed_condition = vec![];
        for expr in self.condition {
            typed_condition.push(expr.type_check(
                stack,
                frame,
                func,
                global_env,
                types,
                generic_map,
            )?);
        }

        Signature::new(vec![Type::Bool.id()], vec![]).evaluate(&self.token, stack, types)?;

        let stack_after_check = stack.clone();

        let mut typed_block = vec![];
        for expr in self.block {
            typed_block.push(expr.type_check(
                stack,
                frame,
                func,
                global_env,
                types,
                generic_map,
            )?);
        }

        Ok((
            TypedExpr::ElseIf {
                condition: typed_condition,
                block: typed_block,
            },
            stack_after_check,
        ))
    }
}
