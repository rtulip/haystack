use super::{
    TypedAsExpr, TypedBlockExpr, TypedCallExpr, TypedGetFrameExpr, TypedIfExpr, TypedLiteralExpr,
    TypedOperatorExpr, TypedReadExpr, TypedVarExpr, TypedWhileExpr,
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
    As(TypedAsExpr),
    While(TypedWhileExpr),
}