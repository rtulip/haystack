use crate::{error::HayError, lex::token::Token};

/// Accessor Expressions
///
/// This expression represents accessing inner types of framed types or enums.
/// For example:
///
/// ```haystack
/// fn Str.size(&Str: self) -> [u64] { self::size @ }
/// //                                      ^
/// //                                      |
/// //                       This is an accessor expression
/// ```
#[derive(Debug, Clone)]
pub struct AccessorExpr {
    /// The entire token of the accessor expression.
    pub token: Token,
    /// The top level identifier
    pub ident: Token,
    /// The list of inner types
    /// __Note:__ This list should be non-empty.
    pub inner: Vec<Token>,
}
