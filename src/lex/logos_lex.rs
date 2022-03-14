use crate::ir::token::{Loc, Token, TokenKind};
use logos::{Lexer, Logos};

static mut LOC: Loc = Loc {
    file: String::new(),
    row: 1,
    col: 0,
};

pub unsafe fn into_token<P: AsRef<std::path::Path> + std::fmt::Display>(
    lex: &mut Lexer<LogosToken>,
    path: P,
) -> Option<Token> {
    // unsafe
    LOC.file = path.to_string();

    if let Some(kind) = lex.next() {
        let slice = lex.slice();
        let tok = match kind {
            LogosToken::Error => Some(Token {
                kind: TokenKind::Comment(slice.to_string()),
                loc: LOC.clone(),
            }),
            _ => Some(Token {
                kind: TokenKind::from((kind, slice)),
                loc: Loc {
                    file: LOC.file.clone(),
                    row: LOC.row,
                    col: LOC.col + 1,
                },
            }),
        };
        slice.chars().for_each(|c| match c {
            '\n' => unsafe {
                LOC.row += 1;
                LOC.col = 0;
            },
            _ => unsafe {
                LOC.col += 1;
            },
        });
        tok
    } else {
        None
    }
}

pub unsafe fn last_loc() -> Loc {
    LOC.clone()
}

#[derive(Logos, Debug, PartialEq)]
pub enum LogosToken {
    // Keywords
    #[token("fn")]
    FunctionKeyword,
    #[token("var")]
    VarKeyword,
    #[token("if")]
    IfKeyword,
    #[token("else")]
    ElseKeyword,
    #[token("while")]
    WhileKeyword,
    #[token("struct")]
    StructKeyword,
    #[token("cast")]
    CastKeyword,
    #[token("split")]
    SplitKeyword,
    #[token("syscall")]
    SyscallKeyword,

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

    #[regex(r"[_a-zA-Z][^ \n\t:{}\[\]()<>]*")]
    Word,

    #[regex(r"//[^\n]*")]
    Comment,

    #[error]
    #[regex(r"[ \t\n\f]+")]
    Error,
}
