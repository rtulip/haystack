use crate::ir::{keyword::Keyword, literal::Literal, marker::Marker, operator::Operator};
use crate::lex::logos_lex::LogosToken;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum TokenKind {
    Keyword(Keyword),
    Operator(Operator),
    Literal(Literal),
    Marker(Marker),
    Word(String),
    Comment(String),
    EndOfFile,
}

impl std::default::Default for TokenKind {
    fn default() -> Self {
        TokenKind::Comment(String::from("Default"))
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct Loc {
    pub file: String,
    pub row: usize,
    pub col: usize,
}

#[derive(Clone, Serialize, Deserialize, Default, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub loc: Loc,
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.row, self.col)
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self.kind)
    }
}
impl std::fmt::Debug for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.loc, self)
    }
}

impl From<(LogosToken, &str)> for TokenKind {
    fn from(pair: (LogosToken, &str)) -> Self {
        match pair {
            // Keywords
            (LogosToken::FunctionKeyword, _) => TokenKind::Keyword(Keyword::Function),
            (LogosToken::VarKeyword, _) => TokenKind::Keyword(Keyword::Var),
            (LogosToken::IfKeyword, _) => TokenKind::Keyword(Keyword::If),
            (LogosToken::ElseKeyword, _) => TokenKind::Keyword(Keyword::Else),
            (LogosToken::WhileKeyword, _) => TokenKind::Keyword(Keyword::While),
            (LogosToken::StructKeyword, _) => TokenKind::Keyword(Keyword::Struct),
            (LogosToken::CastKeyword, _) => TokenKind::Keyword(Keyword::Cast),
            (LogosToken::SplitKeyword, _) => TokenKind::Keyword(Keyword::Split),
            (LogosToken::SyscallKeyword, _) => TokenKind::Keyword(Keyword::Syscall),
            (LogosToken::IncludeKeyword, _) => TokenKind::Keyword(Keyword::Include),
            // Markers
            (LogosToken::OpenBrace, _) => TokenKind::Marker(Marker::OpenBrace),
            (LogosToken::CloseBrace, _) => TokenKind::Marker(Marker::CloseBrace),
            (LogosToken::OpenBracket, _) => TokenKind::Marker(Marker::OpenBracket),
            (LogosToken::CloseBracket, _) => TokenKind::Marker(Marker::CloseBracket),
            (LogosToken::OpenParen, _) => TokenKind::Marker(Marker::OpenParen),
            (LogosToken::CloseParen, _) => TokenKind::Marker(Marker::CloseParen),
            (LogosToken::Colon, _) => TokenKind::Marker(Marker::Colon),
            (LogosToken::DoubleColon, _) => TokenKind::Marker(Marker::DoubleColon),
            (LogosToken::Arrow, _) => TokenKind::Marker(Marker::Arrow),
            // Literals
            (LogosToken::U64, s) => TokenKind::Literal(Literal::Int(s.parse::<u64>().unwrap())),
            (LogosToken::String, s) => {
                TokenKind::Literal(Literal::String(s[1..s.len() - 1].to_string()))
            }
            (LogosToken::True, _) => TokenKind::Literal(Literal::Bool(true)),
            (LogosToken::False, _) => TokenKind::Literal(Literal::Bool(false)),
            // Operators
            (LogosToken::Add, _) => TokenKind::Operator(Operator::Add),
            (LogosToken::Sub, _) => TokenKind::Operator(Operator::Sub),
            (LogosToken::Mul, _) => TokenKind::Operator(Operator::Mul),
            (LogosToken::Div, _) => TokenKind::Operator(Operator::Div),
            (LogosToken::LessThan, _) => TokenKind::Operator(Operator::LessThan),
            (LogosToken::LessEqual, _) => TokenKind::Operator(Operator::LessEqual),
            (LogosToken::GreaterThan, _) => TokenKind::Operator(Operator::GreaterThan),
            (LogosToken::GreaterEqual, _) => TokenKind::Operator(Operator::GreaterEqual),
            (LogosToken::Equals, _) => TokenKind::Operator(Operator::Equals),
            (LogosToken::NotEquals, _) => TokenKind::Operator(Operator::NotEquals),
            // Word
            (LogosToken::Word, s) => TokenKind::Word(s.to_string()),
            (LogosToken::Comment, s) => TokenKind::Comment(s.to_string()),
            // Error
            (LogosToken::Error, s) => panic!("Unrecognized Token: {s}"),
        }
    }
}
