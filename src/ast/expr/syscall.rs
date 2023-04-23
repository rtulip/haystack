use crate::{error::HayError, lex::token::Token};

#[derive(Debug, Clone)]
pub struct ExprSyscall {
    pub n: usize,
    pub token: Token,
}

impl ExprSyscall {}
