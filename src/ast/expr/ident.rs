use std::collections::HashMap;

use crate::{ast::stmt::StmtKind, error::HayError, lex::token::Token};

#[derive(Debug, Clone)]
pub struct ExprIdent {
    pub ident: Token,
}

impl ExprIdent {}
