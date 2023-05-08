use super::{
    TypedAsExpr, TypedBlockExpr, TypedCallExpr, TypedGetAddressOfFramedExpr, TypedGetFrameExpr,
    TypedIfExpr, TypedLiteralExpr, TypedOperatorExpr, TypedReadExpr, TypedVarExpr, TypedWhileExpr,
    TypedWriteExpr,
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
}
