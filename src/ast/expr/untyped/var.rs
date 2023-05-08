use crate::{
    ast::{
        expr::{TypedExpr, TypedVarExpr},
        stmt::{FunctionDescription, Interfaces, UserDefinedTypes},
    },
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
        interfaces: &Interfaces,
        function: Option<&FunctionDescription>,
    ) -> Result<TypedExpr, HayError> {
        let typ = Type::from_token(
            &self.typ,
            user_defined_types,
            interfaces,
            match function {
                Some(function) => function.free_vars.as_ref(),
                None => None,
            },
        )?;
        let size = typ.size(&self.token)?;
        let width = typ.width();

        frame.push((
            self.ident.lexeme.clone(),
            Type::Pointer(PointerType {
                mutable: true,
                inner: Box::new(typ),
            }),
        ));

        let data = if let Some((dimension, typ)) = self.typ.dimension()? {
            let inner_typ = Type::from_type_token(
                &self.token,
                &typ,
                user_defined_types,
                interfaces,
                match function {
                    Some(function) => function.free_vars.as_ref(),
                    None => None,
                },
            )?;
            Some((inner_typ.size(&self.token)? * dimension, inner_typ.width()))
        } else {
            None
        };

        Ok(TypedExpr::Var(TypedVarExpr { size, width, data }))
    }
}
