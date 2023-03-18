//! As Expressions
//!
//! This is the expression which binds elements from the stack to an identifier
//! For example:
//!
//! ```haystack
//! main() {
//!     1 "Hello World" as [one greeting]
//! //                  ~~~~~~~~~~~~~~~~~
//! //                          |
//! //              This is the as expression
//! }
//! ```
//!
use std::collections::HashMap;

use crate::{
    ast::{
        arg::{IdentArg, IdentArgKind},
        stmt::StmtKind,
    },
    error::HayError,
    lex::token::Token,
    types::{Frame, FramedType, Signature, Stack, Type, TypeId, TypeMap, UncheckedFunction},
};

use super::{Expr, TypedExpr};

#[derive(Debug, Clone)]
pub struct AsExpr {
    /// Token of the `as` keyword
    pub token: Token,
    /// The non-empty list of identifiers.
    pub idents: Vec<IdentArg>,
    /// The optional temporary scope.
    pub block: Option<Vec<Expr>>,
}

impl AsExpr {
    /// Converts an untyped as expression into a Typed Expression.
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        func: &UncheckedFunction,
        global_env: &mut HashMap<String, (StmtKind, Signature)>,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        // Save the initial state of the frame -- needed to return the frame
        // to its original state if "args: {args:?}, inner: {args:?}"there's a block.
        let initial_frame = frame.clone();

        // Make sure there's enough items on the stack. For example, this would
        // fail: `fn main() { 1 as [one two] }`
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
                    .map(|arg| arg.kind.to_string())
                    .collect::<Vec<_>>()
            ))
            .with_hint(format!("Found: {stack:?}"));

            return Err(e);
        }

        // Move the elements from the stack to the frame and track what types
        // are being moved.
        let mut typed_args = vec![];
        AsExpr::build_typed_args(
            &self.idents.iter().rev().collect(),
            &mut typed_args,
            stack,
            types,
            frame,
        )?;

        // Type check the block if there is one.
        let typed_block;
        if let Some(blk) = self.block {
            let mut tmp = vec![];
            for e in blk {
                tmp.push(e.type_check(stack, frame, func, global_env, types, generic_map)?);
            }

            typed_block = Some(tmp);
            *frame = initial_frame;
        } else {
            typed_block = None;
        }

        Ok(TypedExpr::As {
            args: typed_args,
            block: typed_block,
        })
    }

    fn build_typed_args(
        idents: &Vec<&IdentArg>,
        typed_args: &mut Vec<TypeId>,
        stack: &mut Stack,
        types: &mut TypeMap,
        frame: &mut Frame,
    ) -> Result<(), HayError> {
        for arg in idents {
            let t = stack.pop().unwrap();

            match &arg.kind {
                IdentArgKind::Single { token } => {
                    frame.push((
                        token.lexeme.clone(),
                        FramedType {
                            origin: token.clone(),
                            typ: t.clone(),
                            mutable: arg.mutable.is_some(),
                        },
                    ));
                    typed_args.push(t);
                }
                IdentArgKind::Tuple { args } => match types.get(&t) {
                    Some(Type::Tuple {
                        inner,
                        idents: None,
                    }) => {
                        if args.len() != inner.len() {
                            return Err(HayError::new_type_err(
                                "Incorrect number of arguments to destructure tuple",
                                arg.kind.token().loc.clone(),
                            )
                            .with_hint(format!(
                                "Found idents: {:?}",
                                args.iter()
                                    .map(|arg| arg.kind.to_string())
                                    .collect::<Vec<_>>()
                            ))
                            .with_hint(format!("For Tuple of type: {t}")));
                        }

                        AsExpr::build_typed_args(
                            &args.iter().rev().collect(),
                            typed_args,
                            &mut inner.clone(),
                            types,
                            frame,
                        )?;
                    }
                    Some(Type::Tuple {
                        inner: _,
                        idents: Some(_),
                    }) => todo!(),
                    _ => {
                        return Err(HayError::new_type_err(
                            format!("Non-tuple type `{t}` cannot be destructured"),
                            arg.kind.token().loc.clone(),
                        ))
                    }
                },
            }
        }

        Ok(())
    }
}
