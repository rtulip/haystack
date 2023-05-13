use crate::{
    backend::Instruction,
    error::HayError,
    lex::token::Token,
    types::{RecordKind, RecordType, Substitutions, Type, VariantType},
};

use super::TypedCallExpr;

#[derive(Debug, Clone)]
pub struct TypedCastExpr {
    pub typ: Type,
}

impl TypedCastExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        self.typ = self.typ.clone().substitute(token, subs)?;
        Ok(())
    }

    pub fn into_instructions(&self) -> (Vec<Instruction>, Vec<TypedCallExpr>) {
        match &self.typ {
            Type::Variant(VariantType {
                typ: box Type::Record(record),
                variant,
            }) if record.kind == RecordKind::EnumStruct => {
                let max_member_size = Type::Record(record.clone()).size_unchecked() - 1;
                let (idx, variant_member) = record
                    .members
                    .iter()
                    .enumerate()
                    .find(|(_, m)| &m.ident == variant)
                    .unwrap();
                let variant_size = variant_member.typ.size_unchecked();

                let mut instrs = vec![];
                for _ in 0..(max_member_size - variant_size) {
                    instrs.push(Instruction::PushU64(0));
                }

                instrs.push(Instruction::PushU64(idx as u64));
                (instrs, vec![])
            }
            _ => (vec![], vec![]),
        }
    }
}
