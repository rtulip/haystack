use crate::{
    lex::token::Token,
    parser::{expr::ParseExpr, signature::Signature, typ::ParseType},
};

pub struct FunctionDefinition<'src> {
    pub name: Token<'src>,
    pub params: Vec<ParseType<'src>>,
    pub signature: Signature<'src>,
    pub expr: ParseExpr<'src>,
}

pub struct StructDefinition<'src> {
    pub name: Token<'src>,
    pub params: Vec<ParseType<'src>>,
    pub members: Vec<(ParseType<'src>, Token<'src>)>,
}

pub struct EnumDefinition<'src> {
    pub name: Token<'src>,
    pub params: Vec<ParseType<'src>>,
    pub variants: Vec<(Option<ParseType<'src>>, Token<'src>)>,
}

pub enum Statememt<'src> {
    FunctionDefinition(FunctionDefinition<'src>),
    StructDefinition(StructDefinition<'src>),
    EnumDefinition(EnumDefinition<'src>),
}
