use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Keyword {
    Function,
    Var,
    If,
    Else,
    While,
    Struct,
    Cast,
    Split,
}
