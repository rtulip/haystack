use std::collections::HashMap;

use crate::{
    ast::stmt::StmtKind,
    error::HayError,
    lex::token::Token,
    types::{Frame, Signature, Stack, Type, TypeId, TypeMap},
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
        global_env: &mut HashMap<String, (StmtKind, Signature)>,
    ) -> Result<TypedExpr, HayError> {
        match global_env.get(&self.ident.lexeme) {
            Some((StmtKind::Function, sig)) => {
                let typed_expr = if let Some(map) = sig.evaluate(&self.ident, stack, types)? {
                    let gen_fn_tid = TypeId::new(&self.ident.lexeme);
                    let monomorphised = gen_fn_tid.assign(&self.ident, &map, types)?;
                    Ok(TypedExpr::Call {
                        func: monomorphised.0,
                    })
                } else {
                    Ok(TypedExpr::Call {
                        func: self.ident.lexeme,
                    })
                };

                return typed_expr;
            }
            Some((StmtKind::Var, sig)) => {
                sig.evaluate(&self.ident, stack, types)?;
                return Ok(TypedExpr::Global {
                    ident: self.ident.lexeme,
                });
            }
            Some((StmtKind::InterfaceFunction(base), _)) => {
                let interface = match types.get(base).unwrap() {
                    Type::InterfaceBase(base) => base.clone(),
                    _ => unreachable!(),
                };

                return Ok(TypedExpr::Call {
                    func: interface.resolve(&self, stack, types, global_env)?,
                });
            }
            None => (),
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
