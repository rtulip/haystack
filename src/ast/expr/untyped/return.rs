use crate::{
    ast::{expr::TypedExpr, stmt::FunctionDescription},
    error::HayError,
    lex::token::Token,
    types::{Stack, Type},
};

#[derive(Debug, Clone)]
pub struct ReturnExpr {
    pub token: Token,
}

impl ReturnExpr {
    pub fn type_check(
        &self,
        stack: &mut Stack,
        function: &FunctionDescription,
    ) -> Result<TypedExpr, HayError> {
        if stack != &function.typ.output {
            return Err(HayError::new_type_err(
                "Early return type check failure.",
                self.token.loc.clone(),
            )
            .with_hint(format!(
                "Function `{}` expects return type(s): {:?}",
                &function.name.lexeme, &function.typ.output
            ))
            .with_hint(format!("Found the following stack: {stack:?}")));
        }

        stack.push(Type::never());
        Ok(TypedExpr::Return)
    }
}
