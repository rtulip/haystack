use crate::{lex::token::Token, parser::typ::ParseType};

#[derive(Debug, Clone)]
pub struct Argument<'src> {
    pub typ: ParseType<'src>,
    pub ident: Option<Token<'src>>,
}
