use crate::lex::token::Token;

///
#[derive(Debug, Clone)]
pub struct Arg<TypeState> {
    pub token: Token,
    pub ident: Option<Token>,
    pub typ: TypeState,
}
