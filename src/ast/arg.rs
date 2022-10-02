use crate::lex::token::Token;
use crate::types::Type;
pub struct Arg {
    pub token: Token,
    pub typ: Option<Type>,
    pub ident: Option<Token>,
}
