use super::TypedExpr;

pub struct TypedIfExpr {
    pub then: Box<TypedExpr>,
    pub otherwise: Vec<TypedElseIfExpr>,
    pub finally: Option<Box<TypedExpr>>,
}

pub struct TypedElseIfExpr {
    pub condition: Vec<TypedExpr>,
    pub block: Box<TypedExpr>,
}
