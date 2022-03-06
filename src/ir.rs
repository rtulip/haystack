use crate::lexer::logos_lex::LogosToken;
use serde::{Deserialize, Serialize};

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(u64),
    String(String),
    Bool(bool),
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Keyword {
    Function,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Keyword(Keyword),
    Operator(Operator),
    Literal(Literal),
    Marker(Marker),
    Word(String),
    Comment(String),
}

impl From<(LogosToken, &str)> for Token {
    fn from(pair: (LogosToken, &str)) -> Self {
        match pair {
            // Keywords
            (LogosToken::FunctionKeyword, _) => Token::Keyword(Keyword::Function),
            // Markers
            (LogosToken::OpenBrace, _) => Token::Marker(Marker::OpenBrace),
            (LogosToken::CloseBrace, _) => Token::Marker(Marker::CloseBrace),
            (LogosToken::OpenBracket, _) => Token::Marker(Marker::OpenBracket),
            (LogosToken::CloseBracket, _) => Token::Marker(Marker::CloseBracket),
            (LogosToken::OpenParen, _) => Token::Marker(Marker::OpenParen),
            (LogosToken::CloseParen, _) => Token::Marker(Marker::CloseParen),
            (LogosToken::Colon, _) => Token::Marker(Marker::Colon),
            (LogosToken::DoubleColon, _) => Token::Marker(Marker::DoubleColon),
            (LogosToken::Arrow, _) => Token::Marker(Marker::Arrow),
            // Literals
            (LogosToken::Int, s) => Token::Literal(Literal::Int(s.parse::<u64>().unwrap())),
            (LogosToken::String, s) => {
                Token::Literal(Literal::String(s[1..s.len() - 1].to_string()))
            }
            (LogosToken::True, _) => Token::Literal(Literal::Bool(true)),
            (LogosToken::False, _) => Token::Literal(Literal::Bool(false)),
            // Operators
            (LogosToken::Add, _) => Token::Operator(Operator::Add),
            (LogosToken::Sub, _) => Token::Operator(Operator::Sub),
            (LogosToken::Mul, _) => Token::Operator(Operator::Mul),
            (LogosToken::Div, _) => Token::Operator(Operator::Div),
            (LogosToken::LessThan, _) => Token::Operator(Operator::LessThan),
            (LogosToken::LessEqual, _) => Token::Operator(Operator::LessEqual),
            (LogosToken::GreaterThan, _) => Token::Operator(Operator::GreaterThan),
            (LogosToken::GreaterEqual, _) => Token::Operator(Operator::GreaterEqual),
            (LogosToken::Equals, _) => Token::Operator(Operator::Equals),
            (LogosToken::NotEquals, _) => Token::Operator(Operator::NotEquals),
            // Word
            (LogosToken::Word, s) => Token::Word(s.to_string()),
            (LogosToken::Comment, s) => Token::Comment(s.to_string()),
            // Error
            (LogosToken::Error, s) => panic!("Unrecognized Token: {s}"),
        }
    }
}

#[derive(Debug, Serialize, Deserialize)]
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
}

impl From<Token> for Op {
    fn from(token: Token) -> Op {
        match token {
            Token::Literal(Literal::Int(x)) => Op::PushInt(x),
            Token::Literal(Literal::Bool(b)) => Op::PushBool(b),
            Token::Literal(Literal::String(s)) => Op::PushString(s),
            Token::Operator(Operator::Add) => Op::Add,
            Token::Operator(Operator::Sub) => Op::Sub,
            Token::Operator(Operator::Mul) => Op::Mul,
            Token::Operator(Operator::Div) => Op::Div,
            Token::Operator(Operator::LessThan) => Op::LessThan,
            Token::Operator(Operator::LessEqual) => Op::LessEqual,
            Token::Operator(Operator::GreaterThan) => Op::GreaterThan,
            Token::Operator(Operator::GreaterEqual) => Op::GreaterEqual,
            Token::Operator(Operator::Equals) => Op::Equals,
            Token::Operator(Operator::NotEquals) => Op::NotEquals,
            Token::Comment(c) => panic!("Cannot convert comment to op: {:?}", c),
            Token::Keyword(kw) => panic!("Cannot convert keyword to op: {:?}", kw),
            Token::Marker(m) => panic!("Cannot convert marker to op: {:?}", m),
            Token::Word(word) => match word.as_str() {
                "print" => Op::Print,
                _ => Op::Word(word),
            },
        }
    }
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Type {
    pub name: String,
    pub ident: Option<String>,
}

#[derive(Default, Debug, Serialize, Deserialize)]
pub struct Signature {
    pub inputs: Vec<Type>,
    pub outputs: Vec<Type>,
}

#[derive(Debug, Serialize, Deserialize)]
pub struct Function {
    pub name: String,
    pub gen: Vec<Type>,
    pub sig: Signature,
    pub ops: Vec<Op>,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Program {
    pub functions: Vec<Function>,
}
