use std::collections::HashMap;
use std::fmt::Display;
use std::ops::Range;

#[derive(Debug, Clone, Default)]
pub struct Loc<S> {
    pub file: S,
    pub line: usize,
    pub span: Range<usize>,
}

impl<S> Loc<S> {
    pub fn new(file: S, line: usize, start: usize, end: usize) -> Self {
        Loc {
            file,
            line,
            span: Range { start, end },
        }
    }
}

impl<S> std::fmt::Display for Loc<S>
where
    S: Display,
{
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}:{}", self.file, self.line, self.span.start)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Marker {
    LeftBrace,
    RightBrace,
    LeftParen,
    RightParen,
    LeftBracket,
    RightBracket,
    Colon,
    DoubleColon,
    Dot,
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
            Marker::Dot => write!(f, ".")?,
            Marker::Arrow => write!(f, "->")?,
        }
        write!(f, "`")
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
    Ampersand,
    Pipe,
    Caret,
    ShiftRight,
    ShiftLeft,
}

impl PartialEq for Operator {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Operator::Plus, Operator::Plus)
            | (Operator::Minus, Operator::Minus)
            | (Operator::Star, Operator::Star)
            | (Operator::Slash, Operator::Slash)
            | (Operator::LessThan, Operator::LessThan)
            | (Operator::LessEqual, Operator::LessEqual)
            | (Operator::GreaterThan, Operator::GreaterThan)
            | (Operator::GreaterEqual, Operator::GreaterEqual)
            | (Operator::Equal, Operator::Equal)
            | (Operator::BangEqual, Operator::BangEqual)
            | (Operator::Modulo, Operator::Modulo)
            | (Operator::Read, Operator::Read)
            | (Operator::Write, Operator::Write)
            | (Operator::Ampersand, Operator::Ampersand)
            | (Operator::Pipe, Operator::Pipe)
            | (Operator::Caret, Operator::Caret)
            | (Operator::ShiftRight, Operator::ShiftRight)
            | (Operator::ShiftLeft, Operator::ShiftLeft) => true,
            (Operator::Plus, _)
            | (Operator::Minus, _)
            | (Operator::Star, _)
            | (Operator::Slash, _)
            | (Operator::LessThan, _)
            | (Operator::LessEqual, _)
            | (Operator::GreaterThan, _)
            | (Operator::GreaterEqual, _)
            | (Operator::Equal, _)
            | (Operator::BangEqual, _)
            | (Operator::Modulo, _)
            | (Operator::Read, _)
            | (Operator::Write, _)
            | (Operator::Ampersand, _)
            | (Operator::Pipe, _)
            | (Operator::Caret, _)
            | (Operator::ShiftRight, _)
            | (Operator::ShiftLeft, _) => false,
        }
    }
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
            Operator::Equal => write!(f, "==")?,
            Operator::BangEqual => write!(f, "!=")?,
            Operator::Modulo => write!(f, "%")?,
            Operator::Read => write!(f, "@")?,
            Operator::Write => write!(f, "!")?,
            Operator::Ampersand => write!(f, "&")?,
            Operator::Pipe => write!(f, "|")?,
            Operator::Caret => write!(f, "^")?,
            Operator::ShiftRight => write!(f, ">>")?,
            Operator::ShiftLeft => write!(f, "<<")?,
        }
        write!(f, "`")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Function,
    As,
    If,
    Else,
    While,
    Do,
    Struct,
    Enum,
    UncheckedCast,
    Include,
    SizeOf,
    Impl,
    Mut,
    Interface,
    Requires,
    Match,
    Pack,
    Unpack,
    Extern,
}

impl std::fmt::Display for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "`")?;
        match self {
            Keyword::Function => write!(f, "fn")?,
            Keyword::As => write!(f, "as")?,
            Keyword::If => write!(f, "if")?,
            Keyword::Else => write!(f, "else")?,
            Keyword::While => write!(f, "while")?,
            Keyword::Do => write!(f, "do")?,
            Keyword::Struct => write!(f, "struct")?,
            Keyword::Enum => write!(f, "enum")?,
            Keyword::UncheckedCast => write!(f, "__unchecked_cast")?,
            Keyword::Include => write!(f, "include")?,
            Keyword::SizeOf => write!(f, "sizeOf")?,
            Keyword::Impl => write!(f, "impl")?,
            Keyword::Mut => write!(f, "mut")?,
            Keyword::Interface => write!(f, "interface")?,
            Keyword::Requires => write!(f, "requires")?,
            Keyword::Match => write!(f, "match")?,
            Keyword::Pack => write!(f, "pack")?,
            Keyword::Unpack => write!(f, "unpack")?,
            Keyword::Extern => write!(f, "extern")?,
        }
        write!(f, "`")
    }
}

impl Keyword {
    pub fn keywords() -> HashMap<&'static str, TokenKind<'static>> {
        let mut map = HashMap::new();
        map.insert("fn", TokenKind::Keyword(Keyword::Function));
        map.insert("as", TokenKind::Keyword(Keyword::As));
        map.insert("if", TokenKind::Keyword(Keyword::If));
        map.insert("else", TokenKind::Keyword(Keyword::Else));
        map.insert("while", TokenKind::Keyword(Keyword::While));
        map.insert("do", TokenKind::Keyword(Keyword::Do));
        map.insert("struct", TokenKind::Keyword(Keyword::Struct));
        map.insert("enum", TokenKind::Keyword(Keyword::Enum));
        map.insert(
            "__unchecked_cast",
            TokenKind::Keyword(Keyword::UncheckedCast),
        );
        map.insert("include", TokenKind::Keyword(Keyword::Include));
        map.insert("sizeof", TokenKind::Keyword(Keyword::SizeOf));
        map.insert("impl", TokenKind::Keyword(Keyword::Impl));
        map.insert("mut", TokenKind::Keyword(Keyword::Mut));
        map.insert("interface", TokenKind::Keyword(Keyword::Interface));
        map.insert("requires", TokenKind::Keyword(Keyword::Requires));
        map.insert("match", TokenKind::Keyword(Keyword::Match));
        map.insert("unpack", TokenKind::Keyword(Keyword::Unpack));

        map.insert("true", TokenKind::Literal(Literal::Bool(true)));
        map.insert("false", TokenKind::Literal(Literal::Bool(false)));

        map
    }
}

#[derive(Debug, Clone)]
pub enum Literal<'src> {
    Bool(bool),
    Char(char),
    String(&'src str),
    U8(u8),
    U64(u64),
}

#[derive(Debug, Clone, Default)]
pub enum TokenKind<'src> {
    Ident(&'src str),
    Marker(Marker),
    Operator(Operator),
    Keyword(Keyword),
    Literal(Literal<'src>),
    Whitespace,
    #[default]
    EoF,
}

impl<'src> TokenKind<'src> {
    pub fn string() -> Self {
        TokenKind::Literal(Literal::String(""))
    }

    pub fn ident() -> Self {
        TokenKind::Ident("")
    }

    pub fn u64() -> Self {
        TokenKind::Literal(Literal::U64(0))
    }
}

impl<'src> std::fmt::Display for TokenKind<'src> {
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
            TokenKind::Whitespace => write!(f, "` `"),
        }
    }
}

impl<'src> std::cmp::PartialEq for TokenKind<'src> {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TokenKind::Ident(_), TokenKind::Ident(_)) => true,
            (TokenKind::Marker(m), TokenKind::Marker(o)) => m == o,
            (TokenKind::Operator(op), TokenKind::Operator(o)) => op == o,
            (TokenKind::Keyword(kw), TokenKind::Keyword(o)) => kw == o,
            (TokenKind::Literal(l), TokenKind::Literal(o)) => matches!(
                (l, o),
                (Literal::Bool(_), Literal::Bool(_))
                    | (Literal::Char(_), Literal::Char(_))
                    | (Literal::String(_), Literal::String(_))
                    | (Literal::U64(_), Literal::U64(_))
                    | (Literal::U8(_), Literal::U8(_))
            ),
            (TokenKind::EoF, TokenKind::EoF) => true,
            (TokenKind::Whitespace, TokenKind::Whitespace) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, Default)]
pub struct Token<'src> {
    pub kind: TokenKind<'src>,
    pub lexeme: &'src str,
    pub loc: Loc<&'src str>,
}

impl<'src> Token<'src> {
    pub fn new(
        kind: TokenKind<'src>,
        lexeme: &'src str,
        file: &'src str,
        line: usize,
        start: usize,
        end: usize,
    ) -> Token<'src> {
        Token {
            kind,
            lexeme,
            loc: Loc {
                file,
                line,
                span: Range { start, end },
            },
        }
    }
}

impl<'src> std::fmt::Display for Token<'src> {
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
            TokenKind::Whitespace => Ok(()),
        }
    }
}

mod tests {

    #[allow(unused_imports)]
    use super::*;

    #[test]
    fn test_operator_display() {
        assert_eq!(format!("{}", Operator::Plus), format!("`+`"));
        assert_eq!(format!("{}", Operator::Minus), format!("`-`"));
        assert_eq!(format!("{}", Operator::Star), format!("`*`"));
        assert_eq!(format!("{}", Operator::Slash), format!("`/`"));
        assert_eq!(format!("{}", Operator::LessThan), format!("`<`"));
        assert_eq!(format!("{}", Operator::LessEqual), format!("`<=`"));
        assert_eq!(format!("{}", Operator::GreaterThan), format!("`>`"));
        assert_eq!(format!("{}", Operator::GreaterEqual), format!("`>=`"));
        assert_eq!(format!("{}", Operator::Equal), format!("`==`"));
        assert_eq!(format!("{}", Operator::BangEqual), format!("`!=`"));
        assert_eq!(format!("{}", Operator::Modulo), format!("`%`"));
        assert_eq!(format!("{}", Operator::Read), format!("`@`"));
        assert_eq!(format!("{}", Operator::Write), format!("`!`"));
    }

    #[test]
    fn test_keyword_display() {
        assert_eq!(format!("{}", Keyword::Else), format!("`else`"));
        assert_eq!(format!("{}", Keyword::While), format!("`while`"));
        assert_eq!(format!("{}", Keyword::Union), format!("`union`"));
        assert_eq!(format!("{}", Keyword::Include), format!("`include`"));
    }

    #[test]
    fn test_tokenkind_display() {
        assert_eq!(
            format!("{}", TokenKind::Operator(Operator::Plus)),
            format!("`+`")
        );

        assert_eq!(
            format!("{}", TokenKind::Literal(Literal::Bool(true))),
            format!("`bool`")
        );

        assert_eq!(
            format!("{}", TokenKind::Literal(Literal::U8(0))),
            format!("`u8`")
        );

        assert_eq!(format!("{}", TokenKind::Syscall(1)), format!("Syscall(1)"));
    }

    #[test]
    fn test_bad_token_destructure() {
        let tok = Token::new(TokenKind::EoF, "", "", 0, 0, 0);

        assert!(tok.string().is_err());
        assert!(tok.typ().is_err());
        assert!(tok.ident().is_err());
        assert!(tok.u64().is_err());
        assert!(tok.keyword().is_err());
    }

    #[test]
    fn test_token_display() {
        let mut tok = Token::new(TokenKind::EoF, "lexeme", "file", 1, 1, 2);

        assert_eq!(format!("{}", tok), String::from("[file:1:1]: EOF"));
        tok.kind = TokenKind::Ident(String::from("ident"));
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: ident"));
        tok.kind = TokenKind::Keyword(Keyword::While);
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: lexeme"));
        tok.kind = TokenKind::Literal(Literal::Bool(true));
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: true"));
        tok.kind = TokenKind::Literal(Literal::String(String::new()));
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: lexeme"));
        tok.kind = TokenKind::Literal(Literal::Char(' '));
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: lexeme"));
        tok.kind = TokenKind::Literal(Literal::U64(0));
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: 0"));
        tok.kind = TokenKind::Literal(Literal::U8(0));
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: 0u8"));
        tok.kind = TokenKind::Marker(Marker::Arrow);
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: `->`"));
        tok.kind = TokenKind::Operator(Operator::Plus);
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: `+`"));
        tok.kind = TokenKind::Syscall(1);
        assert_eq!(format!("{}", tok), String::from("[file:1:1]: syscall(1)"));
    }

    #[test]
    fn test_syscall_partial_eq() {
        assert_eq!(TokenKind::Syscall(1), TokenKind::Syscall(2));
    }

    #[test]
    fn test_dimension_error() {
        assert!(Token::new(TokenKind::EoF, "lexeme", "file", 0, 0, 0)
            .dimension()
            .is_err());
    }
}
