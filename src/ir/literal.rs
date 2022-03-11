use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Literal {
    Int(u64),
    String(String),
    Bool(bool),
}
