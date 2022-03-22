use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Operator {
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equals,
    NotEquals,
    Mod,
    Read,
    Write,
}
