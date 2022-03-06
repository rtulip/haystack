use logos::Logos;

#[derive(Logos, Debug, PartialEq)]
pub enum LogosToken {
    // Keywords
    #[token("fn")]
    FunctionKeyword,

    // Markers
    #[token("{")]
    OpenBrace,
    #[token("}")]
    CloseBrace,
    #[token("[")]
    OpenBracket,
    #[token("]")]
    CloseBracket,
    #[token("(")]
    OpenParen,
    #[token(")")]
    CloseParen,
    #[token(":")]
    Colon,
    #[token("::")]
    DoubleColon,
    #[token("->")]
    Arrow,

    // Literals
    #[regex(r"[0-9]+")]
    Int,
    #[regex("\"[^\"]*\"")]
    String,
    #[token("true")]
    True,
    #[token("false")]
    False,

    // Operators
    #[token("+")]
    Add,
    #[token("-")]
    Sub,
    #[token("*")]
    Mul,
    #[token("/")]
    Div,
    #[token("<")]
    LessThan,
    #[token("<=")]
    LessEqual,
    #[token(">")]
    GreaterThan,
    #[token(">=")]
    GreaterEqual,
    #[token("==")]
    Equals,
    #[token("!=")]
    NotEquals,

    #[regex(r"[_a-zA-Z][^ \n\t:{}\[\]()<>]*")]
    Word,

    #[regex(r"//.*\n")]
    Comment,

    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,
}
