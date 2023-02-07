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
use std::collections::HashMap;

use crate::{
    ast::stmt::StmtKind,
    error::HayError,
    lex::token::Token,
    types::{Frame, Signature, Stack, Type, TypeId, TypeMap, UncheckedFunction},
};

use super::{Expr, TypedExpr};

#[derive(Debug, Clone)]
pub struct TupleExpr {
    /// Token of the `as` keyword
    pub token: Token,
    /// The non-empty list of identifiers.
    pub exprs: Vec<Expr>,
}

impl TupleExpr {
    /// Converts an untyped as expression into a Typed Expression.
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        func: &UncheckedFunction,
        global_env: &mut HashMap<String, (StmtKind, Signature)>,
        types: &mut TypeMap,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let mut typed_exprs = vec![];

        let mut inner_stack = vec![];

        for e in self.exprs.clone() {
            typed_exprs.push(e.type_check(
                &mut inner_stack,
                frame,
                func,
                global_env,
                types,
                generic_map,
            )?);
        }

        let tuple = Type::Tuple {
            inner: inner_stack.clone(),
        };

        let tid = tuple.id();
        stack.push(tid);

        let t = TypedExpr::Tuple { exprs: typed_exprs };

        Ok(t)
    }
}
