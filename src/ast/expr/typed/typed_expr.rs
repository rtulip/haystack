use super::{
    TypedAsExpr, TypedBlockExpr, TypedCallExpr, TypedGetAddressOfFramedExpr, TypedGetFrameExpr,
    TypedGlobalExpr, TypedIfExpr, TypedLiteralExpr, TypedOperatorExpr, TypedReadExpr,
    TypedSizeOfExpr, TypedVarExpr, TypedWhileExpr, TypedWriteExpr,
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
}
