use std::collections::HashMap;
use std::ops::Range;

#[derive(Debug, Clone)]
pub struct Loc {
    pub file: String,
    pub line: usize,
    pub span: Range<usize>,
}

impl std::fmt::Display for Loc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.span.start)
    }
}

#[derive(Debug, Clone, Copy)]
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
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
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
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, Copy)]
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
        write!(f, "{:?}", self)
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
pub enum TokenKind {
    Identifier(String),
    Marker(Marker),
    Operator(Operator),
    Keyword(Keyword),
    Literal(Literal),
    EoF,
}

#[derive(Debug, Clone)]
pub struct Token {
    kind: TokenKind,
    lexeme: String,
    loc: Loc,
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
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}]: ", self.loc)?;
        match &self.kind {
            TokenKind::EoF => write!(f, "EOF"),
            TokenKind::Identifier(ident) => write!(f, "{ident}"),
            TokenKind::Keyword(kw) => write!(f, "{kw}"),
            TokenKind::Literal(literal) => match literal {
                Literal::Bool(b) => write!(f, "{b}"),
                Literal::String(_) => write!(f, "{}", self.lexeme),
                Literal::Char(_) => write!(f, "{}", self.lexeme),
                Literal::U64(u) => write!(f, "{u}"),
                Literal::U8(u) => write!(f, "{u}u8"),
            },
            TokenKind::Marker(m) => write!(f, "{m}"),
            TokenKind::Operator(op) => write!(f, "{op}"),
        }
    }
}
