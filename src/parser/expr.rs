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
pub struct ParsePlace<'src> {
    pub base: &'src str,
    pub projections: Vec<&'src str>,
}

impl<'src> ParsePlace<'src> {
    pub fn new(base: &'src str, projections: Vec<&'src str>) -> Self {
        Self { base, projections }
    }

    pub fn ident(base: &'src str) -> Self {
        Self {
            base,
            projections: vec![],
        }
    }
}

#[derive(Debug)]
pub enum ParseExprKind<'src> {
    Place(ParsePlace<'src>),
    Literal(Literal<'src>),
    Block(Vec<ParseExpr<'src>>),
    Type(ParseType<'src>),
    Tuple(Vec<ParseExpr<'src>>),
    Pack,
    Unpack,
    Call,
    Intrinsic(Intrinsic),
    As(Vec<Token<'src>>),
    TakeRef {
        place: ParsePlace<'src>,
        mutable: bool,
    },
    If(ParseIfExpr<'src>),
    Match(ParseMatchExpr<'src>),
    While {
        condition: Vec<ParseExpr<'src>>,
        body: Box<ParseExpr<'src>>,
    },
    SizeOf,
    Read,
    Write,
}

#[derive(Debug)]
pub struct ParseExpr<'src> {
    pub kind: ParseExprKind<'src>,
    pub token: Token<'src>,
}
