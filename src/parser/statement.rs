use crate::parser::{expr::ParseExpr, signature::Signature, typ::ParseType};

pub struct FunctionDefinition<'src> {
    pub params: Vec<ParseType<'src>>,
    pub signature: Signature<'src>,
    pub expr: ParseExpr<'src>,
}

pub enum Statememt<'src> {
    FunctionDefinition(FunctionDefinition<'src>)
}
