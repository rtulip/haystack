use crate::{
    lex::token::{Literal, Token},
    parser::typ::ParseType,
};

pub enum ParseExprKind<'src> {
    Ident(&'src str),
    Literal(Literal<'src>),
    Block(Vec<ParseExpr<'src>>),
    Type(ParseType<'src>),
    Tuple(Vec<ParseExpr<'src>>),
    Pack,
}

pub struct ParseExpr<'src> {
    pub kind: ParseExprKind<'src>,
    pub token: Token<'src>,
}
