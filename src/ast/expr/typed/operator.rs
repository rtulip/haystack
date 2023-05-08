use crate::{lex::token::Operator, types::Type};

pub struct TypedOperatorExpr {
    pub op: Operator,
    pub typ: Option<Type>,
}
