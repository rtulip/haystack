use crate::{
    error::HayError,
    lex::token::Token,
    types::{Frame, FramedType, TypeId, TypeMap},
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct ExprVar {
    /// The token of the `var` keyword
    pub token: Token,
    /// The token of the type of the var
    pub typ: Token,
    /// The token of the name of the var.
    pub ident: Token,
}

impl ExprVar {
    pub fn type_check(self, frame: &mut Frame, types: &mut TypeMap) -> Result<TypedExpr, HayError> {
        let typ_id = TypeId::from_token(&self.typ, types, &vec![])?;
        let typ_size = typ_id.size(types)?;
        let typ_width = typ_id.width();
        if types.get(&typ_id).is_none() {
            return Err(HayError::new(
                format!("Unrecognized type `{typ_id}`"),
                self.typ.loc.clone(),
            ));
        }

        let id = typ_id.ptr_of(true, types);

        let origin = self.ident.clone();
        frame.push((
            self.ident.lexeme,
            FramedType {
                origin,
                typ: id,
                mutable: false,
            },
        ));

        let data = if let Some((dimension, tt)) = self.typ.dimension()? {
            let inner_typ = TypeId::from_type_token(&self.typ, &tt, types, &vec![])?;
            Some((inner_typ.size(types)? * dimension, inner_typ.width()))
        } else {
            None
        };

        Ok(TypedExpr::Var {
            size: typ_size,
            width: typ_width,
            data,
        })
    }
}
