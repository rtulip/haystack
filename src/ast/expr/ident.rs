use std::collections::HashMap;

use crate::{
    ast::stmt::StmtKind,
    error::HayError,
    lex::token::Token,
    types::{Frame, Signature, Stack, TypeId, TypeMap},
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct ExprIdent {
    pub ident: Token,
}

impl ExprIdent {
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &Frame,
        types: &mut TypeMap,
        global_env: &HashMap<String, (StmtKind, Signature)>,
    ) -> Result<TypedExpr, HayError> {
        if let Some((kind, sig)) = global_env.get(&self.ident.lexeme) {
            let typed_expr = if let Some(map) = sig.evaluate(&self.ident, stack, types)? {
                assert!(matches!(kind, StmtKind::Function));
                let gen_fn_tid = TypeId::new(&self.ident.lexeme);
                let monomorphised = gen_fn_tid.assign(&self.ident, &map, types)?;
                Ok(TypedExpr::Call {
                    func: monomorphised.0,
                })
            } else {
                match kind {
                    StmtKind::Var => Ok(TypedExpr::Global {
                        ident: self.ident.lexeme,
                    }),
                    StmtKind::Function => Ok(TypedExpr::Call {
                        func: self.ident.lexeme,
                    }),
                }
            };

            return typed_expr;
        }

        if let Some((i, (_, tid))) = frame
            .iter()
            .enumerate()
            .find(|(_, (id, _))| &self.ident.lexeme == id)
        {
            stack.push(tid.typ.clone());
            return Ok(TypedExpr::Framed {
                frame: frame.clone(),
                idx: i,
                inner: None,
            });
        }

        Err(HayError::new_type_err(
            format!("Unrecognized word `{}`", self.ident.lexeme),
            self.ident.loc.clone(),
        ))
    }
}
