use std::collections::BTreeMap;

use crate::{
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId},
};

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
pub struct UntypedArg {
    /// The token of the argument
    pub token: Token,
    /// An optional mutable keyword,
    pub mutable: Option<Token>,
    /// An optional identifier.
    pub ident: Option<Token>,
}

#[derive(Debug, Clone)]
pub struct TypedArg {
    pub token: Token,
    pub mutable: Option<Token>,
    pub ident: Option<Token>,
    pub typ: TypeId,
}

#[derive(Debug, Clone)]
pub struct IdentArg {
    pub token: Token,
    pub mutable: Option<Token>,
}

impl UntypedArg {
    pub fn resolve(
        args: Vec<UntypedArg>,
        types: &mut BTreeMap<TypeId, Type>,
        local_types: &Vec<TypeId>,
    ) -> Result<Vec<TypedArg>, HayError> {
        let mut out = vec![];

        for arg in args {
            let typ = TypeId::from_token(&arg.token, types, local_types)?;
            out.push(TypedArg {
                token: arg.token,
                mutable: arg.mutable,
                ident: arg.ident,
                typ,
            });
        }

        Ok(out)
    }
}
