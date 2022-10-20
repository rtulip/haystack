use crate::lex::token::Token;
#[derive(Debug, Clone)]
pub struct Arg<TypeState> {
    pub token: Token,
    pub typ: TypeState,
    pub ident: Option<Token>,
}
