use crate::lex::token::Token;

#[derive(Debug)]
pub enum ParseType<'src> {
    Simple(Token<'src>),
}
