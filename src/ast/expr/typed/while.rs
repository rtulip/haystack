use super::TypedExpr;

pub struct TypedWhileExpr {
    pub cond: Vec<TypedExpr>,
    pub body: Box<TypedExpr>,
}
