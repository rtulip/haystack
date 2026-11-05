use crate::{
    lex::token::{Intrinsic, Literal, Token},
    parser::typ::ParseType,
};

#[derive(Debug)]
pub struct ParseIfElseBranch<'src> {
    pub token: Token<'src>,
    pub condition: Vec<ParseExpr<'src>>,
    pub body: Vec<ParseExpr<'src>>,
}

#[derive(Debug)]
pub struct ParseIfExpr<'src> {
    pub then: Vec<ParseExpr<'src>>,
    pub otherwise: Vec<ParseIfElseBranch<'src>>,
    pub finally: Vec<ParseExpr<'src>>,
}

#[derive(Debug)]

pub struct ParseMatchExpr<'src> {
    pub cases: Vec<(ParseType<'src>, ParseExpr<'src>)>,
    pub finally: Option<Box<ParseExpr<'src>>>,
}

#[derive(Debug)]
pub enum ParseExprKind<'src> {
    Ident(&'src str),
    Member { base: &'src str, member: &'src str },
    Literal(Literal<'src>),
    Block(Vec<ParseExpr<'src>>),
    Type(ParseType<'src>),
    Tuple(Vec<ParseExpr<'src>>),
    Pack,
    Call,
    Intrinsic(Intrinsic),
    As(Vec<Token<'src>>),
    TakeRef { mutable: bool, ident: &'src str },
    If(ParseIfExpr<'src>),
    Match(ParseMatchExpr<'src>),
}

#[derive(Debug)]
pub struct ParseExpr<'src> {
    pub kind: ParseExprKind<'src>,
    pub token: Token<'src>,
}
