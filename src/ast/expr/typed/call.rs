use crate::{
    error::HayError,
    lex::token::Token,
    types::{Substitutions, Type},
};

pub struct TypedCallExpr {
    pub func: String,
    pub subs: Substitutions,
}

impl TypedCallExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        println!("{} {:?}", self.func, self.subs);

        for (_, sub) in (&mut self.subs).into_iter() {
            if matches!(sub, Type::TypeVar(_)) {
                todo!()
            }

            *sub = sub.clone().substitute(token, subs)?;
        }

        Ok(())
    }
}
