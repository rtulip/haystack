use super::{TypedGetFrameExpr, TypedLiteralExpr, TypedVarExpr};

pub enum TypedExpr {
    Literal(TypedLiteralExpr),
    Var(TypedVarExpr),
    Framed(TypedGetFrameExpr),
}
