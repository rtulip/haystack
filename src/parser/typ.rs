use std::fmt::Display;

use crate::lex::token::Token;

#[derive(Debug)]
pub enum ParseType<'src> {
    Simple(Token<'src>),
}

impl<'src> ParseType<'src> {
    pub fn common_form(&self) -> String {
        match self {
            ParseType::Simple(token) => String::from(token.lexeme),
        }
    }

    pub fn token(&self) -> Token<'src> {
        match self {
            ParseType::Simple(token) => token.clone(),
        }
    }
}

impl<'src> Display for ParseType<'src> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.common_form())
    }
}
