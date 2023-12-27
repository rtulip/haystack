use crate::{
    parser::token::Token,
    types::{FnTy, Stack, Substitution, Ty, Types},
};

use super::ApplicationError;

#[derive(Debug, Clone)]
pub enum TyInstanceExpr<'src> {
    Enum { base: &'src str, variant: &'src str },
}

impl<'src> TyInstanceExpr<'src> {
    pub fn apply(
        self,
        token: &Token<'src>,
        stack: Stack<'src>,
        types: &Types<'src>,
    ) -> Result<(Stack<'src>, Substitution<'src>), ApplicationError<'src>> {
        match self {
            TyInstanceExpr::Enum { base, variant } => FnTy::new(
                [],
                [Ty::EnumInstance(
                    types
                        .get(base)
                        .expect("Type identifier should be known")
                        .clone()
                        .get_enum(),
                    variant,
                )],
            )
            .apply(token, stack),
        }
    }
}
