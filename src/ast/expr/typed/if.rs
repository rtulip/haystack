use super::TypedExpr;

pub struct TypedIfExpr {
    pub then: Box<TypedExpr>,
    pub otherwise: Vec<TypedExpr>,
    pub finally: Option<Box<TypedExpr>>,
}
