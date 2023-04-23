use std::collections::HashMap;

use crate::{
    error::HayError,
    lex::token::{Literal, Token},
};

#[derive(Debug, Clone)]
pub struct ExprSizeOf {
    /// The token of the `sizeOf` keyword
    pub token: Token,
    /// The token of the type
    pub typ: Token,
}

impl ExprSizeOf {}
