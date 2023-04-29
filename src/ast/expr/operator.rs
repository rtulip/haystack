use crate::{
    ast::stmt::{StmtKind, UserDefinedTypes},
    error::HayError,
    lex::token::{Operator, Token},
    types::{Frame, FunctionType, Stack, Substitutions, Type},
};

#[derive(Debug, Clone)]
pub struct OperatorExpr {
    pub op: Operator,
    pub token: Token,
}

impl OperatorExpr {
    pub fn type_check(
        &self,
        types: &UserDefinedTypes,
        stack: &mut Stack,
        frame: &mut Frame,
        subs: &mut Substitutions,
    ) -> Result<(), HayError> {
        match &self.op {
            Operator::GreaterEqual => {
                let fs = vec![
                    FunctionType::new(vec![Type::u64(), Type::u64()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::u8(), Type::u8()], vec![Type::bool()]),
                    FunctionType::new(vec![Type::char(), Type::char()], vec![Type::bool()]),
                ];

                FunctionType::unify_many(&fs, &self.token, stack, subs)?;
            }
            op => todo!("{op:?}"),
        }

        Ok(())
    }
}
