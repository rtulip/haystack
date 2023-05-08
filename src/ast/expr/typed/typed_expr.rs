use super::{TypedGetFrameExpr, TypedLiteralExpr, TypedOperatorExpr, TypedVarExpr};

pub enum TypedExpr {
    Literal(TypedLiteralExpr),
    Var(TypedVarExpr),
    Framed(TypedGetFrameExpr),
    Operator(TypedOperatorExpr),
}
