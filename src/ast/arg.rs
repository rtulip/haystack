use crate::lex::token::Token;

/// A structure to represent an Argument.
///
/// This is used by certain expressions, such as [`crate::ast::expr::Expr::As`]
/// and some statements such as [`crate::ast::stmt::Stmt::Function`].
///
/// This is generic over Typestate which represents the type information known
/// about this argument at different stages of compilation.
///
/// For example consider this function definition in `Haystack`:
///
/// `fn foo(u64: bar) -> [bool] { ... }`
///
/// The input argument `u64: bar` gets parsed into:
/// ```
/// use crate::types::type_state::Untyped;
/// Arg<Untyped> {
///     // Not actually how tokens are created, just illustrates the idea.
///     token: Token::new("u64: bar", ...),
///     ident: Some(Token::new("bar", ...)),
///     typ:   Untyped
/// }
/// ```
///
/// and the output `[bool]` would be parsed as:
/// ```
/// use crate::types::type_state::Untyped;
/// Arg<Untyped> {
///     token: Token::new("bool", ...),
///     ident: None,
///     typ: Untyped
/// }
/// ```
///
#[derive(Debug, Clone)]
pub struct Arg<TypeState> {
    /// The token of the argument
    pub token: Token,
    /// An optional identifier.
    pub ident: Option<Token>,
    /// Type information related to the argument
    pub typ: TypeState,
}
