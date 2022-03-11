use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Marker {
    OpenBrace,
    CloseBrace,
    OpenBracket,
    CloseBracket,
    OpenParen,
    CloseParen,
    Colon,
    DoubleColon,
    Arrow,
}
