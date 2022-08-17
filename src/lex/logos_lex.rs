use crate::ir::token::{Loc, Token, TokenKind};
use logos::{Lexer, Logos};

pub fn into_token(lex: &mut Lexer<LogosToken>, loc: &mut Loc) -> Option<Token> {
    if let Some(kind) = lex.next() {
        let slice = lex.slice();
        slice.chars().for_each(|c| match c {
            '\n' => {
                loc.row += 1;
                loc.col = 1;
            }
            _ => loc.col += 1,
        });
        match kind {
            LogosToken::Error => Some(Token {
                kind: TokenKind::Comment(slice.to_string()),
                loc: loc.clone(),
            }),
            _ => Some(Token {
                kind: TokenKind::from((kind, slice)),
                loc: loc.clone(),
            }),
        }
    } else {
        None
    }
}

#[derive(Logos, Debug, PartialEq)]
pub enum LogosToken {
    // Keywords
    #[token("fn")]
    FunctionKeyword,
    #[token("var")]
    VarKeyword,
    #[token("as")]
    AsKeyword,
    #[token("if")]
    IfKeyword,
    #[token("else")]
    ElseKeyword,
    #[token("while")]
    WhileKeyword,
    #[token("struct")]
    StructKeyword,
    #[token("union")]
    UnionKeyword,
    #[token("enum")]
    EnumKeyword,
    #[token("cast")]
    CastKeyword,
    #[token("syscall")]
    SyscallKeyword,
    #[token("include")]
    IncludeKeyword,
    #[token("sizeOf")]
    SizeOfKeyword,
    #[token("pub")]
    PubKeyword,

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
    U64,
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
    #[token("%")]
    Mod,

    #[token("@")]
    Read,
    #[token("!")]
    Write,

    #[regex(r"[_a-zA-Z][^ \n\t:{}\[\]()<>]*")]
    Word,

    #[regex(r"//[^\n]*")]
    Comment,

    #[error]
    #[regex(r"[ \t\n\f]+")]
    Error,
}
