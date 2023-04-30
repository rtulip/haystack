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
    types::{Frame, Stack},
};

#[derive(Debug, Clone)]
pub struct AsExpr {
    /// Token of the `as` keyword
    pub token: Token,
    /// The non-empty list of identifiers.
    pub idents: Vec<IdentArg>,
}

impl AsExpr {
    pub fn type_check(&self, stack: &mut Stack, frame: &mut Frame) -> Result<(), HayError> {
        // Make sure there's enough items on the stack. For example, this would
        // fail: `fn main() { 1 as [one two] }`
        if stack.len() < self.idents.len() {
            let e = HayError::new_type_err(
                "Insufficient elements on the stack to bind",
                self.token.loc.clone(),
            )
            .with_hint(format!(
                "Expected {} elements to bind to idents: {:?}",
                self.idents.len(),
                self.idents
                    .iter()
                    .map(|arg| arg.kind.to_string())
                    .collect::<Vec<_>>()
            ))
            .with_hint(format!("Found: {stack:?}"));

            return Err(e);
        }

        // Move the elements from the stack to the frame and track what types
        // are being moved.

        for id in &self.idents {
            let t = stack.pop().unwrap();
            match &id.kind {
                IdentArgKind::Single { token } => frame.push((token.lexeme.clone(), t)),
                IdentArgKind::Tuple { .. } => todo!(),
            }
        }

        Ok(())
    }
}
