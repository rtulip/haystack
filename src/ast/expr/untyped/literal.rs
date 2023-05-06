use crate::{
    ast::stmt::{TypeDescription, UserDefinedTypes},
    backend::{InitData, InitDataMap, Instruction},
    error::HayError,
    lex::token::{Literal, Token},
    types::{Stack, Type, TypeId},
};

#[derive(Debug, Clone)]
pub struct LiteralExpr {
    pub literal: Literal,
    pub token: Token,
}

impl LiteralExpr {
    pub fn type_check(&self, stack: &mut Stack, types: &UserDefinedTypes) -> Result<(), HayError> {
        match self.literal {
            Literal::Bool(_) => stack.push(Type::bool()),
            Literal::Char(_) => stack.push(Type::char()),
            Literal::String(_) => {
                let typ = types.get(&TypeId::new("Str")).unwrap();
                match typ {
                    TypeDescription::Record(record) => stack.push(Type::Record(record.typ.clone())),
                    &TypeDescription::PreDeclaration(_) => todo!(),
                }
            }
            Literal::U64(_) => stack.push(Type::u64()),
            Literal::U8(_) => stack.push(Type::u8()),
        }

        Ok(())
    }

    pub fn into_instructions(
        &self,
        init_data: &mut InitDataMap,
    ) -> Result<Vec<Instruction>, HayError> {
        match &self.literal {
            Literal::String(s) => {
                let n = init_data.len();
                let s = Instruction::escape_string(s);
                let str_len = s.len() as u64;
                let str_id = format!("str_{n}");
                init_data.insert(str_id.clone(), InitData::String(s));
                Ok(vec![
                    Instruction::PushU64(str_len),
                    Instruction::PushGlobal { id: str_id },
                ])
            }
            Literal::Bool(b) => Ok(vec![Instruction::PushU64(*b as u64)]),
            Literal::Char(c) => Ok(vec![Instruction::PushU64(*c as u64)]),
            Literal::U64(n) => Ok(vec![Instruction::PushU64(*n as u64)]),
            Literal::U8(n) => Ok(vec![Instruction::PushU64(*n as u64)]),
        }
    }
}
