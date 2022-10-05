use crate::error::HayError;
use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Loc {
    pub file: String,
    pub line: usize,
    pub span: Range<usize>,
}

impl Loc {
    pub fn new<S>(file: S, line: usize, start: usize, end: usize) -> Self
    where
        S: Into<String>,
    {
        Loc {
            file: file.into(),
            line: line,
            span: Range { start, end },
        }
    }
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.span.start)
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Marker {
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Colon,
    DoubleColon,
    Arrow,
}

impl std::fmt::Display for Marker {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`")?;
        match self {
            Marker::LeftBrace => write!(f, "{{")?,

            Marker::RightBrace => write!(f, "}}")?,
            Marker::LeftParen => write!(f, "(")?,
            Marker::RightParen => write!(f, ")")?,
            Marker::LeftBracket => write!(f, "[")?,
            Marker::RightBracket => write!(f, "]")?,
            Marker::Colon => write!(f, ":")?,
            Marker::DoubleColon => write!(f, "::")?,
            Marker::Arrow => write!(f, "->")?,
        }
        write!(f, "`")
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operator {
    Plus,
    Minus,
    Star,
    Slash,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equal,
    BangEqual,
    Modulo,
    Read,
    Write,
}

impl std::fmt::Display for Operator {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`")?;
        match self {
            Operator::Plus => write!(f, "+")?,
            Operator::Minus => write!(f, "-")?,
            Operator::Star => write!(f, "*")?,
            Operator::Slash => write!(f, "/")?,
            Operator::LessThan => write!(f, "<")?,
            Operator::LessEqual => write!(f, "<=")?,
            Operator::GreaterThan => write!(f, ">")?,
            Operator::GreaterEqual => write!(f, ">=")?,
            Operator::Equal => write!(f, "=")?,
            Operator::BangEqual => write!(f, "!=")?,
            Operator::Modulo => write!(f, "%")?,
            Operator::Read => write!(f, "@")?,
            Operator::Write => write!(f, "!")?,
        }
        write!(f, "`")
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
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
    Impl,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`")?;
        match self {
            Keyword::Function => write!(f, "fn")?,
            Keyword::Var => write!(f, "var")?,
            Keyword::As => write!(f, "as")?,
            Keyword::If => write!(f, "if")?,
            Keyword::Else => write!(f, "else")?,
            Keyword::While => write!(f, "while")?,
            Keyword::Struct => write!(f, "struct")?,
            Keyword::Union => write!(f, "union")?,
            Keyword::Enum => write!(f, "enum")?,
            Keyword::Cast => write!(f, "cast")?,
            Keyword::Syscall => write!(f, "syscall")?,
            Keyword::Include => write!(f, "include")?,
            Keyword::SizeOf => write!(f, "sizeOf")?,
            Keyword::Pub => write!(f, "pub")?,
            Keyword::Impl => write!(f, "impl")?,
        }
        write!(f, "`")
    }
}

impl Keyword {
    pub fn keywords() -> HashMap<&'static str, TokenKind> {
        let mut map = HashMap::new();
        map.insert("fn", TokenKind::Keyword(Keyword::Function));
        map.insert("var", TokenKind::Keyword(Keyword::Var));
        map.insert("as", TokenKind::Keyword(Keyword::As));
        map.insert("if", TokenKind::Keyword(Keyword::If));
        map.insert("else", TokenKind::Keyword(Keyword::Else));
        map.insert("while", TokenKind::Keyword(Keyword::While));
        map.insert("struct", TokenKind::Keyword(Keyword::Struct));
        map.insert("union", TokenKind::Keyword(Keyword::Union));
        map.insert("enum", TokenKind::Keyword(Keyword::Enum));
        map.insert("cast", TokenKind::Keyword(Keyword::Cast));
        map.insert("syscall", TokenKind::Keyword(Keyword::Syscall));
        map.insert("include", TokenKind::Keyword(Keyword::Include));
        map.insert("sizeOf", TokenKind::Keyword(Keyword::SizeOf));
        map.insert("pub", TokenKind::Keyword(Keyword::Pub));
        map.insert("impl", TokenKind::Keyword(Keyword::Impl));
        map.insert("true", TokenKind::Literal(Literal::Bool(true)));
        map.insert("false", TokenKind::Literal(Literal::Bool(false)));
        map.insert("syscall", TokenKind::Syscall(0));
        map
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Bool(bool),
    Char(char),
    String(String),
    U8(u8),
    U64(u64),
}

#[derive(Debug, Clone)]
pub enum TypeToken {
    Pointer(Box<TypeToken>),
    Parameterized {
        base: String,
        inner: Vec<Box<TypeToken>>,
    },
    Array {
        base: Box<TypeToken>,
        size: u64,
    },
    Base(String),
}

impl std::fmt::Display for TypeToken {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeToken::Base(s) => write!(f, "{s}"),
            TypeToken::Pointer(p) => write!(f, "*{p}"),
            TypeToken::Parameterized { base, inner } => {
                write!(f, "{base}<")?;
                for i in 0..inner.len() - 1 {
                    write!(f, "{} ", inner[i])?;
                }
                write!(f, "{}>", inner.last().unwrap())
            }
            TypeToken::Array { base, size } => write!(f, "{base}[{size}]"),
        }
    }
}

#[derive(Debug, Clone)]
pub enum TokenKind {
    Ident(String),
    Marker(Marker),
    Operator(Operator),
    Keyword(Keyword),
    Literal(Literal),
    Syscall(u8),
    Type(TypeToken),
    EoF,
}

impl TokenKind {
    pub fn string() -> Self {
        TokenKind::Literal(Literal::String(String::from("")))
    }

    pub fn ident() -> Self {
        TokenKind::Ident(String::from(""))
    }

    pub fn u64() -> Self {
        TokenKind::Literal(Literal::U64(0))
    }
}

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TokenKind::Marker(m) => write!(f, "{m}"),
            TokenKind::Operator(o) => write!(f, "{o}"),
            TokenKind::Keyword(kw) => write!(f, "{kw}"),
            TokenKind::Literal(lit) => match lit {
                Literal::Bool(_) => write!(f, "`bool`"),
                Literal::Char(_) => write!(f, "`char`"),
                Literal::String(_) => write!(f, "`Str`"),
                Literal::U64(_) => write!(f, "`u64`"),
                Literal::U8(_) => write!(f, "`u8`"),
            },
            TokenKind::Ident(id) => write!(f, "an identifier ({id})"),
            TokenKind::EoF => write!(f, "end of file"),
            TokenKind::Syscall(_) => write!(f, "{:?}", self),
            TokenKind::Type(t) => write!(f, "{t}"),
        }
    }
}

impl std::cmp::PartialEq for TokenKind {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TokenKind::Ident(_), TokenKind::Ident(_)) => true,
            (TokenKind::Marker(m), TokenKind::Marker(o)) => m == o,
            (TokenKind::Operator(op), TokenKind::Operator(o)) => op == o,
            (TokenKind::Keyword(kw), TokenKind::Keyword(o)) => kw == o,
            (TokenKind::Literal(l), TokenKind::Literal(o)) => match (l, o) {
                (Literal::Bool(_), Literal::Bool(_)) => true,
                (Literal::Char(_), Literal::Char(_)) => true,
                (Literal::String(_), Literal::String(_)) => true,
                (Literal::U64(_), Literal::U64(_)) => true,
                (Literal::U8(_), Literal::U8(_)) => true,
                _ => false,
            },
            (TokenKind::EoF, TokenKind::EoF) => true,
            (TokenKind::Syscall(_), TokenKind::Syscall(_)) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: String,
    pub loc: Loc,
}

impl Token {
    pub fn new<S1, S2>(
        kind: TokenKind,
        lexeme: S1,
        file: S2,
        line: usize,
        start: usize,
        end: usize,
    ) -> Token
    where
        S1: Into<String>,
        S2: Into<String>,
    {
        Token {
            kind,
            lexeme: lexeme.into(),
            loc: Loc {
                file: file.into(),
                line,
                span: Range { start, end },
            },
        }
    }

    pub fn string(&self) -> Result<String, HayError> {
        match &self.kind {
            TokenKind::Literal(Literal::String(s)) => Ok(s.clone()),
            _ => HayError::new(
                format!("Failed to destructure {} into String", self.kind),
                self.loc.clone(),
            ),
        }
    }

    pub fn typ(&self) -> Result<TypeToken, HayError> {
        match &self.kind {
            TokenKind::Type(t) => Ok(t.clone()),
            _ => HayError::new(
                format!("Failed to destructure {} into Type", self.kind),
                self.loc.clone(),
            ),
        }
    }

    pub fn ident(&self) -> Result<String, HayError> {
        match &self.kind {
            TokenKind::Ident(s) => Ok(s.clone()),
            _ => HayError::new(
                format!("Failed to destructure {} into an identifier", self.kind),
                self.loc.clone(),
            ),
        }
    }

    pub fn u64(&self) -> Result<u64, HayError> {
        match &self.kind {
            TokenKind::Literal(Literal::U64(n)) => Ok(*n),
            _ => HayError::new(
                format!("Failed to destructure {} into a u64", self.kind),
                self.loc.clone(),
            ),
        }
    }

    pub fn keyword(&self) -> Result<Keyword, HayError> {
        match &self.kind {
            TokenKind::Keyword(kw) => Ok(*kw),
            _ => HayError::new(
                format!("Failed to destructure {} into a keyword", self.kind),
                self.loc.clone(),
            ),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]: ", self.loc)?;
        match &self.kind {
            TokenKind::EoF => write!(f, "EOF"),
            TokenKind::Ident(ident) => write!(f, "{ident}"),
            TokenKind::Keyword(_) => write!(f, "{}", self.lexeme),
            TokenKind::Literal(literal) => match literal {
                Literal::Bool(b) => write!(f, "{b}"),
                Literal::String(_) => write!(f, "{}", self.lexeme),
                Literal::Char(_) => write!(f, "{}", self.lexeme),
                Literal::U64(u) => write!(f, "{u}"),
                Literal::U8(u) => write!(f, "{u}u8"),
            },
            TokenKind::Marker(m) => write!(f, "{m}"),
            TokenKind::Operator(op) => write!(f, "{op}"),
            TokenKind::Syscall(n) => write!(f, "syscall({n})"),
            TokenKind::Type(t) => write!(f, "{t}"),
        }
    }
}