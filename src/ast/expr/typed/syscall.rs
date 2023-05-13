use crate::backend::Instruction;

use super::TypedCallExpr;

#[derive(Debug, Clone)]
pub struct TypedSyscallExpr {
    pub n: usize,
}

impl TypedSyscallExpr {
    pub fn into_instructions(&self) -> (Vec<Instruction>, Vec<TypedCallExpr>) {
        (vec![Instruction::Syscall(self.n)], vec![])
    }
}
