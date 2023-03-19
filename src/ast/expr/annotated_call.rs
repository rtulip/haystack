use std::collections::HashMap;

use crate::{
    ast::{arg::UntypedArg, stmt::StmtKind},
    error::HayError,
    lex::token::Token,
    types::{Signature, Stack, TypeId, TypeMap, Variance},
};

use super::TypedExpr;

/// Type-Annotated Function Call Expression
///
/// This expression represents calling a function with explicit type
/// annotations. This is needed in instances where types cannot be inferred.
///
/// For Example:
/// ```haystack
/// This is an annotated Expression call to `Opt.None`
/// Opt.None::<u64>
/// ```
///
#[derive(Debug, Clone)]
pub struct AnnotatedCallExpr {
    /// The token for the entire annotated call.
    pub token: Token,
    /// The base identifier token
    pub base: Token,
    /// The list of annotations
    pub annotations: Vec<UntypedArg>,
}

impl AnnotatedCallExpr {
    /// Converts an annotated call expression into a typed expression
    pub fn type_check(
        self,
        stack: &mut Stack,
        global_env: &HashMap<String, (StmtKind, Signature)>,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let mut sig = match global_env.get(&self.base.lexeme) {
            Some((_, sig)) => sig.clone(),
            None => {
                return Err(HayError::new_type_err(
                    format!("Unrecognized function `{}`", self.base.lexeme),
                    self.token.loc,
                ))
            }
        };

        // If there are any generic annotations, they need to be resolved from
        // the `generic_map`, this will map to a new monomorphised function.
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
            // No generic type annotations, thus need to find the monomorphised call.
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

        // Check and update the stack and frame.
        sig.assign(&self.token, &annotations, types)?;
        assert!(sig
            .evaluate(&self.token, stack, types, Variance::Covariant)?
            .is_none());

        Ok(TypedExpr::Call { func: tid.0 })
    }
}
