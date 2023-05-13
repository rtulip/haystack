use crate::{
    backend::{InitData, InitDataMap, Instruction},
    error::HayError,
    lex::token::Token,
    types::Substitutions,
};

use super::{TypedCallExpr, TypedExpr};

#[derive(Debug, Clone)]
pub struct TypedIfExpr {
    pub then: Box<TypedExpr>,
    pub otherwise: Vec<TypedElseIfExpr>,
    pub finally: Option<Box<TypedExpr>>,
}

#[derive(Debug, Clone)]
pub struct TypedElseIfExpr {
    pub condition: Vec<TypedExpr>,
    pub block: Box<TypedExpr>,
}

impl TypedIfExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        self.then.substitute(token, subs)?;
        for e in &mut self.otherwise {
            e.substitute(token, subs)?;
        }

        if let Some(finally) = &mut self.finally {
            finally.substitute(token, subs)?;
        }

        Ok(())
    }

    pub fn into_instructions(
        &self,
        init_data: &mut InitDataMap,
        jump_count: &mut usize,
    ) -> (Vec<Instruction>, Vec<TypedCallExpr>) {
        let mut n_jumps = 0;
        let mut ops = vec![];
        let mut then_ops = vec![];
        let mut calls = vec![];
        then_ops.push(Instruction::StartBlock);
        {
            let (instrs, tmp_calls) = self.then.into_instructions(init_data, jump_count);
            then_ops.extend(instrs);
            calls.extend(tmp_calls);
        }

        then_ops.push(Instruction::EndBlock {
            bytes_to_free: Instruction::count_framed_bytes(&then_ops),
        });

        let mut otherwise_ops = vec![];
        for other in &self.otherwise {
            let mut cnd_ops = vec![];
            let mut blk_ops = vec![];
            cnd_ops.push(Instruction::StartBlock);
            for e in &other.condition {
                let (instrs, tmp_calls) = e.into_instructions(init_data, jump_count);
                cnd_ops.extend(instrs);
                calls.extend(tmp_calls);
            }
            cnd_ops.push(Instruction::EndBlock {
                bytes_to_free: Instruction::count_framed_bytes(&cnd_ops),
            });

            {
                let (instrs, tmp_calls) = other.block.into_instructions(init_data, jump_count);
                blk_ops.extend(instrs);
                calls.extend(tmp_calls);
            }

            blk_ops.push(Instruction::EndBlock {
                bytes_to_free: Instruction::count_framed_bytes(&blk_ops),
            });

            otherwise_ops.push((cnd_ops, blk_ops));

            n_jumps += 1;
        }

        let mut finally_ops = vec![];
        if let Some(finally) = &self.finally {
            finally_ops.push(Instruction::StartBlock);
            let (instrs, tmp_calls) = finally.into_instructions(init_data, jump_count);
            finally_ops.extend(instrs);
            calls.extend(tmp_calls);

            finally_ops.push(Instruction::EndBlock {
                bytes_to_free: Instruction::count_framed_bytes(&finally_ops),
            });
            n_jumps += 1;
        }
        ops.push(Instruction::JumpFalse {
            dest_id: *jump_count,
        });
        ops.append(&mut then_ops);
        ops.push(Instruction::Jump {
            dest_id: *jump_count + n_jumps,
        });
        ops.push(Instruction::JumpDest { id: *jump_count });
        *jump_count += 1;

        for (mut cond, mut block) in otherwise_ops {
            n_jumps -= 1;

            ops.append(&mut cond);
            ops.push(Instruction::JumpFalse {
                dest_id: *jump_count,
            });
            ops.append(&mut block);
            ops.push(Instruction::Jump {
                dest_id: *jump_count + n_jumps,
            });
            ops.push(Instruction::JumpDest { id: *jump_count });
            *jump_count += 1;
        }

        if !finally_ops.is_empty() {
            n_jumps -= 1;
            ops.append(&mut finally_ops);
            ops.push(Instruction::Jump {
                dest_id: *jump_count + n_jumps,
            });
            ops.push(Instruction::JumpDest { id: *jump_count });

            *jump_count += 1;
        }

        (ops, calls)
    }
}

impl TypedElseIfExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        for e in &mut self.condition {
            e.substitute(token, subs)?;
        }
        self.block.substitute(token, subs)
    }
}
