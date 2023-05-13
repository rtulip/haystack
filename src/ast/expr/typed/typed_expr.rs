use crate::{
    backend::{InitDataMap, Instruction},
    error::HayError,
    lex::token::Token,
    types::Substitutions,
};

use super::{
    TypedAsExpr, TypedBlockExpr, TypedCallExpr, TypedCastExpr, TypedGetAddressOfFramedExpr,
    TypedGetFrameExpr, TypedGlobalExpr, TypedIfExpr, TypedLiteralExpr, TypedOperatorExpr,
    TypedReadExpr, TypedSizeOfExpr, TypedSyscallExpr, TypedVarExpr, TypedWhileExpr, TypedWriteExpr,
};

#[derive(Debug, Clone)]
pub enum TypedExpr {
    Block(TypedBlockExpr),
    Literal(TypedLiteralExpr),
    Var(TypedVarExpr),
    Framed(TypedGetFrameExpr),
    Operator(TypedOperatorExpr),
    Call(TypedCallExpr),
    If(TypedIfExpr),
    Read(TypedReadExpr),
    Write(TypedWriteExpr),
    As(TypedAsExpr),
    While(TypedWhileExpr),
    Cast(TypedCastExpr),
    AddrFramed(TypedGetAddressOfFramedExpr),
    Global(TypedGlobalExpr),
    Never,
    Return,
    SizeOf(TypedSizeOfExpr),
    Syscall(TypedSyscallExpr),
}

impl TypedExpr {
    pub fn substitute(&mut self, token: &Token, subs: &Substitutions) -> Result<(), HayError> {
        match self {
            TypedExpr::Block(block) => block.substitute(token, subs),
            TypedExpr::Literal(literal) => literal.substitute(token, subs),
            TypedExpr::Var(_) => todo!(),
            TypedExpr::Framed(framed) => framed.substitute(token, subs),
            TypedExpr::Operator(op) => op.substitute(token, subs),
            TypedExpr::Call(call) => call.substitute(token, subs),
            TypedExpr::If(if_expr) => if_expr.substitute(token, subs),
            TypedExpr::Read(read) => read.substitute(token, subs),
            TypedExpr::Write(write) => write.substitute(token, subs),
            TypedExpr::As(as_expr) => as_expr.substitute(token, subs),
            TypedExpr::While(whlie_expr) => whlie_expr.substitute(token, subs),
            TypedExpr::Cast(cast) => cast.substitute(token, subs),
            TypedExpr::AddrFramed(addr) => addr.substitute(token, subs),
            TypedExpr::Global(global) => global.substitute(token, subs),
            TypedExpr::Never => Ok(()),
            TypedExpr::Return => Ok(()),
            TypedExpr::SizeOf(size_of) => size_of.substitute(token, subs),
            TypedExpr::Syscall(_) => Ok(()),
        }
    }

    pub fn into_instructions(
        &self,
        init_data: &mut InitDataMap,
        jump_count: &mut usize,
    ) -> (Vec<Instruction>, Vec<TypedCallExpr>) {
        match self {
            TypedExpr::Block(block) => block.into_instructions(init_data, jump_count),
            TypedExpr::Literal(literal) => literal.into_instructions(init_data),
            TypedExpr::Var(_) => todo!(),
            TypedExpr::Framed(framed) => framed.into_instructions(),
            TypedExpr::Operator(op) => op.into_instructions(),
            TypedExpr::Call(call) => call.into_instructions(),
            TypedExpr::If(if_expr) => if_expr.into_instructions(init_data, jump_count),
            TypedExpr::Read(_) => todo!(),
            TypedExpr::Write(_) => todo!(),
            TypedExpr::As(as_expr) => as_expr.into_instructions(),
            TypedExpr::While(_) => todo!(),
            TypedExpr::Cast(cast) => cast.into_instructions(),
            TypedExpr::AddrFramed(_) => todo!(),
            TypedExpr::Global(_) => todo!(),
            TypedExpr::Never => (vec![], vec![]),
            TypedExpr::Return => (vec![Instruction::Return], vec![]),
            TypedExpr::SizeOf(_) => todo!(),
            TypedExpr::Syscall(syscall) => syscall.into_instructions(),
        }
    }
}
