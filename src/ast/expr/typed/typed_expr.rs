use crate::{error::HayError, lex::token::Token, types::Substitutions};

use super::{
    TypedAsExpr, TypedBlockExpr, TypedCallExpr, TypedGetAddressOfFramedExpr, TypedGetFrameExpr,
    TypedGlobalExpr, TypedIfExpr, TypedLiteralExpr, TypedOperatorExpr, TypedReadExpr,
    TypedSizeOfExpr, TypedSyscallExpr, TypedVarExpr, TypedWhileExpr, TypedWriteExpr,
};

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
            TypedExpr::Var(TypedVarExpr) => todo!(),
            TypedExpr::Framed(framed) => framed.substitute(token, subs),
            TypedExpr::Operator(op) => op.substitute(token, subs),
            TypedExpr::Call(call) => call.substitute(token, subs),
            TypedExpr::If(if_expr) => if_expr.substitute(token, subs),
            TypedExpr::Read(read) => read.substitute(token, subs),
            TypedExpr::Write(write) => write.substitute(token, subs),
            TypedExpr::As(as_expr) => as_expr.substitute(token, subs),
            TypedExpr::While(TypedWhileExpr) => todo!(),
            TypedExpr::Cast => Ok(()),
            TypedExpr::AddrFramed(TypedGetAddressOfFramedExpr) => todo!(),
            TypedExpr::Global(global) => global.substitute(token, subs),
            TypedExpr::Never => todo!(),
            TypedExpr::Return => todo!(),
            TypedExpr::SizeOf(TypedSizeOfExpr) => todo!(),
            TypedExpr::Syscall(TypedSyscallExpr) => todo!(),
        }
    }
}
