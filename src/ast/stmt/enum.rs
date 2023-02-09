use crate::{
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId, TypeMap, VariantType},
};

#[derive(Debug, Clone)]
pub struct EnumStmt {
    pub token: Token,
    pub name: Token,
    pub variants: Vec<Token>,
}

impl EnumStmt {
    pub fn add_to_global_scope(self, types: &mut TypeMap) -> Result<(), HayError> {
        let tid = TypeId::new(&self.name.lexeme);
        let t = Type::Enum {
            token: self.token,
            name: self.name.clone(),
            variants: self.variants.clone(),
        };
        match types.insert(tid.clone(), t) {
            None => (),
            Some(_) => {
                return Err(HayError::new(
                    format!("Name conflict. `{}` defined elsewhere", self.name.lexeme),
                    self.name.loc,
                ))
            }
        }

        for variant in self.variants {
            let t = Type::Variant(VariantType {
                base: tid.clone(),
                variant: variant.lexeme,
            });
            assert!(types.insert(t.id(), t).is_none())
        }

        Ok(())
    }
}
