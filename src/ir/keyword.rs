use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Keyword {
    Function,
    Var,
    As,
    If,
    Else,
    While,
    Struct,
    Union,
    Enum,
    Cast,
    Syscall,
    Include,
    SizeOf,
    Pub,
}
