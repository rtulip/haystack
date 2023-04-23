use std::collections::HashMap;

use crate::{
    ast::{arg::UntypedArg, stmt::StmtKind},
    error::HayError,
    lex::token::Token,
};

/// Type-Annotated Function Call Expression
///
/// This expression represents calling a function with explicit type
/// annotations. This is needed in instances where types cannot be inferred.
///
/// For Example:
/// ```haystack
/// This is an annotated Expression call to `Opt.None`
/// Opt.None::<u64>
/// ```
///
#[derive(Debug, Clone)]
pub struct AnnotatedCallExpr {
    /// The token for the entire annotated call.
    pub token: Token,
    /// The base identifier token
    pub base: Token,
    /// The list of annotations
    pub annotations: Vec<UntypedArg>,
}
