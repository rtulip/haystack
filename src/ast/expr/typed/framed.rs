use crate::{
    backend::Instruction,
    error::HayError,
    lex::token::Token,
    types::{Frame, RecordKind, RecordType, Substitutions, Type},
};

use super::TypedCallExpr;

#[derive(Debug, Clone)]
pub struct TypedGetFrameExpr {
    pub frame: Frame,
    pub idx: usize,
    pub inner: Option<Vec<String>>,
}

impl TypedGetFrameExpr {
    pub fn substitute(&mut self, _token: &Token, _subs: &Substitutions) -> Result<(), HayError> {
        Ok(())
    }

    pub fn into_instructions(&self) -> (Vec<Instruction>, Vec<TypedCallExpr>) {
        assert!(self.idx < self.frame.len());
        let mut offset = 0;
        for (_, t) in &self.frame[self.idx + 1..] {
            assert!(!t.is_generic());
            offset += t.size_unchecked();
        }

        let mut typ = &self.frame[self.idx].1;
        let bytes = if let Some(inner) = &self.inner {
            if inner.is_empty() {
                todo!()
                // match types.get(typ).unwrap() {
                //     Type::Record {
                //         kind: RecordKind::EnumStruct,
                //         ..
                //     } => {
                //         offset += typ.size(types).unwrap() - 1;
                //         1
                //     }
                //     _ => unreachable!(
                //         "Internal Error: framed inner should only be empty on Enum Struct types"
                //     ),
                // }
            } else {
                for inner in inner {
                    typ = match typ {
                        Type::Record(RecordType {
                            kind: RecordKind::Struct | RecordKind::Tuple,
                            members,
                            ..
                        }) => {
                            let idx = members
                                .iter()
                                .enumerate()
                                .find(|(_, m)| &m.ident == inner)
                                .unwrap()
                                .0;

                            for m in &members[0..idx] {
                                offset += m.typ.size_unchecked();
                            }

                            &members[idx].typ
                        }
                        t => todo!("{t}"),
                    };
                }
                typ.size_unchecked()
            }
        } else {
            typ.size_unchecked()
        };

        (
            vec![Instruction::PushFromFrame {
                offset_from_end: offset,
                bytes,
            }],
            vec![],
        )
    }
}
