use crate::{
    ast::stmt::UserDefinedTypes,
    error::HayError,
    lex::token::Token,
    types::{Frame, FreeVars, PointerType, Type},
};

#[derive(Debug, Clone)]
pub struct VarExpr {
    /// The token of the `var` keyword
    pub token: Token,
    /// The token of the type of the var
    pub typ: Token,
    /// The token of the name of the var.
    pub ident: Token,
}

impl VarExpr {
    pub fn type_check(
        &self,
        frame: &mut Frame,
        user_defined_types: &UserDefinedTypes,
        free_vars: &FreeVars,
    ) -> Result<(), HayError> {
        let typ = Type::from_token(&self.typ, user_defined_types, free_vars)?;
        frame.push((
            self.ident.lexeme.clone(),
            Type::Pointer(PointerType {
                mutable: true,
                inner: Box::new(typ),
            }),
        ));

        Ok(())
    }
}
