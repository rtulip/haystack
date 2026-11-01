use crate::{
    lex::token::{Literal, Token},
    parser::typ::ParseType,
};

pub struct ParseIfElseBranch<'src> {
    pub token: Token<'src>,
    pub condition: Vec<ParseExpr<'src>>, 
    pub body: Vec<ParseExpr<'src>>
}

pub struct ParseIfExpr<'src> {
    pub then: Vec<ParseExpr<'src>>,
    pub otherwise: Vec<ParseIfElseBranch<'src>>,
    pub finally: Vec<ParseExpr<'src>>,
}

pub enum ParseExprKind<'src> {
    Ident(&'src str),
    Literal(Literal<'src>),
    Block(Vec<ParseExpr<'src>>),
    Type(ParseType<'src>),
    Tuple(Vec<ParseExpr<'src>>),
    Pack,
    Call,
    UncheckedCast,
    As(Vec<Token<'src>>),
    TakeRef { mutable: bool, ident: &'src str },
    If(ParseIfExpr<'src>),
}

pub struct ParseExpr<'src> {
    pub kind: ParseExprKind<'src>,
    pub token: Token<'src>,
}
