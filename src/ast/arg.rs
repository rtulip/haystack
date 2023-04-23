use std::collections::BTreeMap;

use crate::{
    error::HayError,
    lex::token::Token,
    types::{Type, TypeId},
};

use super::stmt::UserDefinedTypes;

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

impl UntypedArg {
    pub fn into_typed_arg(
        self,
        user_defined_types: &UserDefinedTypes,
    ) -> Result<TypedArg, HayError> {
        let typ = Type::from_token(&self.token, user_defined_types)?;

        Ok(TypedArg {
            token: self.token,
            mutable: self.mutable,
            ident: self.ident,
            typ,
        })
    }

    pub fn into_typed_args(
        args: Vec<Self>,
        user_defined_types: &UserDefinedTypes,
    ) -> Result<Vec<TypedArg>, HayError> {
        let mut out = vec![];
        for arg in args {
            out.push(arg.into_typed_arg(user_defined_types)?);
        }

        Ok(out)
    }
}

#[derive(Debug, Clone)]
pub struct TypedArg {
    pub token: Token,
    pub mutable: Option<Token>,
    pub ident: Option<Token>,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub enum IdentArgKind {
    Single { token: Token },
    Tuple { args: Vec<IdentArg> },
}

impl IdentArgKind {
    pub fn token(&self) -> &Token {
        match self {
            IdentArgKind::Single { token } => token,
            IdentArgKind::Tuple { args } => args.first().unwrap().kind.token(),
        }
    }
}

impl ToString for IdentArgKind {
    fn to_string(&self) -> String {
        match self {
            IdentArgKind::Single { token } => token.lexeme.clone(),
            IdentArgKind::Tuple { args } => {
                let strings = args
                    .iter()
                    .map(|arg| arg.kind.to_string())
                    .collect::<Vec<_>>();
                let mut out = String::from("[");
                for s in &strings[0..strings.len() - 1] {
                    out = format!("{out}{s} ");
                }
                out = format!("{out}{}]", strings.last().unwrap());

                out
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct IdentArg {
    pub kind: IdentArgKind,
    pub mutable: Option<Token>,
}
