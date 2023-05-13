use crate::{
    backend::{InitDataMap, Instruction},
    error::HayError,
    lex::token::Token,
    types::Substitutions,
};

use super::{
    TypedAsExpr, TypedBlockExpr, TypedCallExpr, TypedGetAddressOfFramedExpr, TypedGetFrameExpr,
    TypedGlobalExpr, TypedIfExpr, TypedLiteralExpr, TypedOperatorExpr, TypedReadExpr,
    TypedSizeOfExpr, TypedSyscallExpr, TypedVarExpr, TypedWhileExpr, TypedWriteExpr,
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
    Cast,
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
            TypedExpr::Cast => Ok(()),
            TypedExpr::AddrFramed(addr) => addr.substitute(token, subs),
            TypedExpr::Global(global) => global.substitute(token, subs),
            TypedExpr::Never => Ok(()),
            TypedExpr::Return => todo!(),
            TypedExpr::SizeOf(size_of) => size_of.substitute(token, subs),
            TypedExpr::Syscall(_) => todo!(),
        }
    }

    pub fn into_instructions(
        &self,
        init_data: &mut InitDataMap,
    ) -> (Vec<Instruction>, Vec<(TypedCallExpr)>) {
        match self {
            TypedExpr::Block(block) => block.into_instructions(init_data),
            TypedExpr::Literal(literal) => literal.into_instructions(init_data),
            TypedExpr::Var(_) => todo!(),
            TypedExpr::Framed(framed) => framed.into_instructions(),
            TypedExpr::Operator(_) => todo!(),
            TypedExpr::Call(call) => call.into_instructions(init_data),
            TypedExpr::If(_) => todo!(),
            TypedExpr::Read(_) => todo!(),
            TypedExpr::Write(_) => todo!(),
            TypedExpr::As(_) => todo!(),
            TypedExpr::While(_) => todo!(),
            TypedExpr::Cast => todo!(),
            TypedExpr::AddrFramed(_) => todo!(),
            TypedExpr::Global(_) => todo!(),
            TypedExpr::Never => todo!(),
            TypedExpr::Return => todo!(),
            TypedExpr::SizeOf(_) => todo!(),
            TypedExpr::Syscall(_) => todo!(),
        }
    }
}
