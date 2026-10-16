use crate::lex::token::{Literal, Token};

pub enum ParseExprKind<'src> {
    Ident(&'src str),
    Literal(Literal<'src>),
    Block(Vec<ParseExpr<'src>>),
}

pub struct ParseExpr<'src> {
    pub kind: ParseExprKind<'src>,
    pub token: Token<'src>,
}
