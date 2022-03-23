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
    Split,
    Syscall,
    Include,
    SizeOf,
}
