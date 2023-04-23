//! As Expressions
//!
//! This is the expression which binds elements from the stack to an identifier
//! For example:
//!
//! ```haystack
//! main() {
//!     1 "Hello World" as [one greeting]
//! //                  ~~~~~~~~~~~~~~~~~
//! //                          |
//! //              This is the as expression
//! }
//! ```
//!
use crate::{
    ast::arg::{IdentArg, IdentArgKind},
    error::HayError,
    lex::token::Token,
};

#[derive(Debug, Clone)]
pub struct AsExpr {
    /// Token of the `as` keyword
    pub token: Token,
    /// The non-empty list of identifiers.
    pub idents: Vec<IdentArg>,
}
