use std::collections::HashMap;

use crate::{
    ast::{
        arg::UntypedArg,
        expr::{TypedCallExpr, TypedExpr},
        stmt::{Functions, Interfaces, StmtKind, UserDefinedTypes},
    },
    error::HayError,
    lex::token::Token,
    types::{Stack, Substitutions},
};

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
    pub fn type_check(
        &self,
        stack: &mut Stack,
        user_defined_types: &UserDefinedTypes,
        interfaces: &Interfaces,
        functions: &Functions,
    ) -> Result<TypedExpr, HayError> {
        if let Some(f) = functions.get(&self.base.lexeme) {
            let x = UntypedArg::into_typed_args(
                self.annotations.clone(),
                user_defined_types,
                interfaces,
                f.free_vars.as_ref(),
            )?
            .into_iter()
            .map(|arg| arg.typ)
            .collect();

            let subs = Substitutions::new(&self.token, f.ordered_free_vars.clone().unwrap(), x)?;
            let function_typ = f.typ.substitute(&self.token, &subs)?;
            function_typ.unify(&self.token, stack)?;

            Ok(TypedExpr::Call(TypedCallExpr {
                func: f.name.lexeme.clone(),
                subs,
                impl_id: None,
            }))
        } else {
            todo!()
        }
    }
}
