use std::collections::HashMap;

use crate::{
    ast::{arg::IdentArg, stmt::StmtKind},
    error::HayError,
    lex::token::Token,
    types::{Frame, FramedType, Signature, Stack, TypeId, TypeMap, UncheckedFunction},
};

use super::{Expr, TypedExpr};

#[derive(Debug, Clone)]
pub struct ExprAs {
    /// Token of the `as` keyword
    pub token: Token,
    /// The non-empty list of identifiers.
    pub idents: Vec<IdentArg>,
    /// The optional temporary scope.
    pub block: Option<Vec<Expr>>,
}

impl ExprAs {
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        func: &UncheckedFunction,
        global_env: &HashMap<String, (StmtKind, Signature)>,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let initial_frame = frame.clone();
        if stack.len() < self.idents.len() {
            let e = HayError::new_type_err(
                "Insufficient elements on the stack to bind",
                self.token.loc,
            )
            .with_hint(format!(
                "Expected {} elements to bind to idents: {:?}",
                self.idents.len(),
                self.idents
                    .iter()
                    .map(|arg| &arg.token.lexeme)
                    .collect::<Vec<&String>>()
            ))
            .with_hint(format!("Found: {:?}", stack));

            return Err(e);
        }

        let mut typed_args = vec![];
        self.idents.iter().rev().for_each(|arg| {
            let t = stack.pop().unwrap();
            frame.push((
                arg.token.lexeme.clone(),
                FramedType {
                    origin: arg.token.clone(),
                    typ: t.clone(),
                    mutable: arg.mutable.is_some(),
                },
            ));
            typed_args.push(t);
        });

        let mut typed_block = None;
        if let Some(blk) = self.block {
            let mut tmp = vec![];
            for e in blk {
                tmp.push(e.type_check(stack, frame, func, global_env, types, generic_map)?);
            }

            for _ in 0..self.idents.len() {
                frame.pop();
            }

            typed_block = Some(tmp);
            *frame = initial_frame;
        }

        Ok(TypedExpr::As {
            args: typed_args,
            block: typed_block,
        })
    }
}