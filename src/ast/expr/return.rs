use crate::{
    error::HayError,
    lex::token::Token,
    types::{Stack, Type, TypeId, UncheckedFunction},
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct ExprReturn {
    pub token: Token,
}

impl ExprReturn {
    pub fn type_check(
        self,
        stack: &mut Stack,
        func: &UncheckedFunction,
    ) -> Result<TypedExpr, HayError> {
        let stack_expected = func
            .outputs
            .iter()
            .map(|arg| &arg.typ)
            .collect::<Vec<&TypeId>>();
        let stack_real = stack.iter().collect::<Vec<&TypeId>>();

        if stack_real != stack_expected {
            return Err(
                HayError::new_type_err("Early return type check failure.", self.token.loc)
                    .with_hint(format!(
                        "Function `{}` expects return type(s): {:?}",
                        func.name.lexeme, stack_expected
                    ))
                    .with_hint(format!("Found the following stack: {stack_real:?}")),
            );
        }

        stack.push(Type::Never.id());

        Ok(TypedExpr::Return)
    }
}
