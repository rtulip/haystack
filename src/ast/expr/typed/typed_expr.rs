use super::{
    TypedBlockExpr, TypedCallExpr, TypedGetFrameExpr, TypedIfExpr, TypedLiteralExpr,
    TypedOperatorExpr, TypedReadExpr, TypedVarExpr,
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
}
