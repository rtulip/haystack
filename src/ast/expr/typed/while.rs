use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct TypedWhileExpr {
    pub cond: Vec<TypedExpr>,
    pub body: Box<TypedExpr>,
}
