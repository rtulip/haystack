use crate::types::Substitutions;

pub struct TypedCallExpr {
    pub func: String,
    pub subs: Substitutions,
}
