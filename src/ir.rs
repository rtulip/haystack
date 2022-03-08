use crate::lex::logos_lex::{last_loc, LogosToken};
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
}

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Literal {
    Int(u64),
    String(String),
    Bool(bool),
}

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

#[derive(Debug, PartialEq, Clone, Serialize, Deserialize)]
pub enum Keyword {
    Function,
    Var,
}

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

pub fn eof_tok() -> Token {
    Token {
        kind: TokenKind::EndOfFile,
        loc: unsafe { last_loc() },
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Loc {
    pub file: String,
    pub row: usize,
    pub col: usize,
}

#[derive(Clone, Serialize, Deserialize)]
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

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum Op {
    PushInt(u64),
    PushBool(bool),
    PushString(String),
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
    Print,
    Word(String),
    MakeIdent(String),
    PushIdent(usize),
    Call(String),
    PrepareFunc(Function),
    Return,
}

impl From<Token> for Op {
    fn from(token: Token) -> Op {
        match token {
            Token {
                kind: TokenKind::Literal(Literal::Int(x)),
                ..
            } => Op::PushInt(x),
            Token {
                kind: TokenKind::Literal(Literal::Bool(b)),
                ..
            } => Op::PushBool(b),
            Token {
                kind: TokenKind::Literal(Literal::String(s)),
                ..
            } => Op::PushString(s),
            Token {
                kind: TokenKind::Operator(Operator::Add),
                ..
            } => Op::Add,
            Token {
                kind: TokenKind::Operator(Operator::Sub),
                ..
            } => Op::Sub,
            Token {
                kind: TokenKind::Operator(Operator::Mul),
                ..
            } => Op::Mul,
            Token {
                kind: TokenKind::Operator(Operator::Div),
                ..
            } => Op::Div,
            Token {
                kind: TokenKind::Operator(Operator::LessThan),
                ..
            } => Op::LessThan,
            Token {
                kind: TokenKind::Operator(Operator::LessEqual),
                ..
            } => Op::LessEqual,
            Token {
                kind: TokenKind::Operator(Operator::GreaterThan),
                ..
            } => Op::GreaterThan,
            Token {
                kind: TokenKind::Operator(Operator::GreaterEqual),
                ..
            } => Op::GreaterEqual,
            Token {
                kind: TokenKind::Operator(Operator::Equals),
                ..
            } => Op::Equals,
            Token {
                kind: TokenKind::Operator(Operator::NotEquals),
                ..
            } => Op::NotEquals,
            Token {
                kind: TokenKind::Comment(c),
                ..
            } => panic!("Cannot convert comment to op: {:?}", c),
            Token {
                kind: TokenKind::Keyword(kw),
                ..
            } => match kw {
                Keyword::Function => panic!("Cannot convert function keyword into an op"),
                Keyword::Var => todo!("Var keyword isn't implemented yet"),
            },
            Token {
                kind: TokenKind::Marker(m),
                ..
            } => panic!("Cannot convert marker to op: {:?}", m),
            Token {
                kind: TokenKind::Word(word),
                ..
            } => match word.as_str() {
                "print" => Op::Print,
                _ => Op::Word(word),
            },
            Token {
                kind: TokenKind::EndOfFile,
                ..
            } => panic!("Cannot convert end of file into an op!"),
        }
    }
}

#[derive(Default, Serialize, Deserialize, Clone)]
pub struct Type {
    pub name: String,
    pub ident: Option<String>,
}
impl Type {
    pub fn u64_t() -> Type {
        Type {
            name: String::from("u64"),
            ident: None,
        }
    }

    pub fn bool_t() -> Type {
        Type {
            name: String::from("bool"),
            ident: None,
        }
    }
}

impl std::fmt::Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.ident {
            Some(ref s) => write!(f, "{}: {}", self.name, s),
            None => write!(f, "{}", self.name),
        }
    }
}

#[derive(Default, Debug, Serialize, Deserialize, Clone)]
pub struct Signature {
    pub inputs: Vec<Type>,
    pub outputs: Vec<Type>,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Function {
    pub name: String,
    pub token: Token,
    pub gen: Vec<Type>,
    pub sig: Signature,
    pub ops: Vec<Op>,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Program {
    pub functions: Vec<Function>,
}
