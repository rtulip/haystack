use std::collections::HashMap;

use crate::{
    ast::{arg::UntypedArg, stmt::StmtKind},
    error::HayError,
    lex::token::Token,
    types::{Signature, Stack, TypeId, TypeMap},
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct ExprAnnotatedCall {
    /// The token for the entire annotated call.
    pub token: Token,
    /// The base identifier token
    pub base: Token,
    /// The list of annotations
    pub annotations: Vec<UntypedArg>,
}

impl ExprAnnotatedCall {
    pub fn type_check(
        self,
        stack: &mut Stack,
        global_env: &HashMap<String, (StmtKind, Signature)>,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let (_, mut sig) = global_env
            .get(&self.base.lexeme)
            .unwrap_or_else(|| panic!("Should have found function: {}", self.base))
            .clone();

        let (annotations, tid) = if self
            .annotations
            .iter()
            .any(|arg| TypeId::new(&arg.token.lexeme).is_generic(types))
        {
            if let Some(map) = generic_map {
                let ann = self
                    .annotations
                    .iter()
                    .map(|arg| {
                        let tid = TypeId::new(&arg.token.lexeme);
                        tid.assign(&self.token, map, types)
                    })
                    .collect::<Vec<Result<TypeId, HayError>>>();

                let gen_fn_tid = TypeId::new(&self.base.lexeme);
                let func = gen_fn_tid.assign(&self.token, map, types)?;

                (ann, func)
            } else {
                return Err(HayError::new_type_err(
                    "Unresolved generic types in annotations",
                    self.token.loc.clone(),
                )
                .with_hint(format!(
                    "Found annotations: {:?}",
                    self.annotations
                        .iter()
                        .map(|arg| &arg.token.lexeme)
                        .collect::<Vec<&String>>()
                ))
                .with_hint(format!(
                    "         Of which: {:?} are generic",
                    self.annotations
                        .iter()
                        .map(|arg| TypeId::new(&arg.token.lexeme))
                        .filter(|tid| tid.is_generic(types))
                        .map(|t| t.0)
                        .collect::<Vec<String>>()
                )));
            }
        } else {
            let ann = self
                .annotations
                .iter()
                .map(|arg| Ok(TypeId::new(&arg.token.lexeme)))
                .collect::<Vec<Result<TypeId, HayError>>>();
            let gen_fn_tid = TypeId::new(&self.base.lexeme);
            let func = if let Ok(func) = types
                .get(&gen_fn_tid)
                .unwrap_or_else(|| panic!("bad generic_fn_tid: {gen_fn_tid}"))
                .try_generic_function(&self.token)
            {
                let map: HashMap<TypeId, TypeId> = HashMap::from_iter(
                    func.generics
                        .iter()
                        .zip(&self.annotations)
                        .map(|(k, v)| (k.clone(), TypeId::new(&v.token.lexeme))),
                );

                gen_fn_tid.assign(&self.token, &map, types)?
            } else {
                return Err(HayError::new_type_err(
                    format!(
                        "Cannot provide annotations to non generic function `{}`",
                        self.base.lexeme
                    ),
                    self.token.loc,
                ));
            };

            (ann, func)
        };

        for ann in &annotations {
            if let Err(e) = ann {
                return Err(e.clone());
            }
        }

        let annotations = annotations
            .into_iter()
            .map(|a| if let Ok(tid) = a { tid } else { unreachable!() })
            .collect::<Vec<TypeId>>();

        sig.assign(&self.token, &annotations, types)?;

        if let Some(_map) = sig.evaluate(&self.token, stack, types)? {
            todo!("Make a concrete version of the call")
        }

        Ok(TypedExpr::Call { func: tid.0 })
    }
}
