use crate::{
    backend::{InitDataMap, Instruction},
    error::HayError,
    lex::token::Token,
    types::{FunctionType, Substitutions, Type},
};

#[derive(Debug, Clone)]
pub struct TypedCallExpr {
    pub func: String,
    pub subs: Substitutions,
}

impl TypedCallExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        for (_, sub) in (&mut self.subs).into_iter() {
            if matches!(sub, Type::TypeVar(_)) {
                todo!()
            }

            *sub = sub.clone().substitute(token, subs)?;
        }

        Ok(())
    }

    pub fn into_instructions(
        &self,
        init_data: &mut InitDataMap,
    ) -> (Vec<Instruction>, Vec<(String, Substitutions)>) {
        let id = FunctionType::name(&self.func, &self.subs);
        (
            vec![Instruction::Call(id)],
            vec![(self.func.clone(), self.subs.clone())],
        )
    }
}
