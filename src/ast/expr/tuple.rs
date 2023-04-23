//! Tuple Expressions
//!
//! This is the expression to build tuple literals
//! For example:
//!
//! ```haystack
//! main() {
//!     [1 "Hello Wolrd"] as [tuple]
//! //  ~~~~~~~~~~~~~~~~~
//! //          |
//! // This is the tuple expression
//! }
//! ```
//!
use std::collections::HashMap;

use crate::{ast::stmt::StmtKind, error::HayError, lex::token::Token};

use super::Expr;

#[derive(Debug, Clone)]
pub struct TupleExpr {
    /// Token of the `as` keyword
    pub token: Token,
    /// The non-empty list of identifiers.
    pub exprs: Vec<Expr>,
}

impl TupleExpr {}
