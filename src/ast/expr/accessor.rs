use crate::{
    error::HayError,
    lex::token::Token,
    types::{Frame, RecordKind, Stack, Substitutions, Type},
};

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

impl AccessorExpr {
    pub fn type_check(
        &self,
        stack: &mut Stack,
        frame: &Frame,
        subs: &mut Substitutions,
    ) -> Result<(), HayError> {
        if let Some((i, (_, typ))) = frame
            .iter()
            .enumerate()
            .rev()
            .find(|(_, (k, _))| k == &self.ident.lexeme)
        {
            let typ = typ.get_inner_accessors(&self.token, &self.inner)?;
            stack.push(typ);
        }

        Ok(())
    }
}
