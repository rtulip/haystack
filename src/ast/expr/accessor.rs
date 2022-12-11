use crate::{
    error::HayError,
    lex::token::Token,
    types::{Frame, Stack, Type, TypeId, TypeMap, UncheckedFunction},
};

use super::TypedExpr;

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
    /// Converts a
    pub fn type_check(
        self,
        stack: &mut Stack,
        frame: &mut Frame,
        func: &UncheckedFunction,
        types: &mut TypeMap,
    ) -> Result<TypedExpr, HayError> {
        // Three cases for an accessor
        // 1. Ident is a framed record or pointer
        // 2. Ident is an enum found in types.
        // 3. Ident is unrecognized.

        if let Some((i, (_, ft))) = frame
            .iter()
            .enumerate()
            .find(|(_, (k, _))| k == &self.ident.lexeme)
        {
            match types.get(&ft.typ).unwrap() {
                Type::Record { .. } => {
                    // find the type of the inner accessor
                    let final_tid =
                        ft.typ
                            .get_inner_accessors(self.token, &self.inner, func, types)?;

                    stack.push(final_tid);
                    Ok(TypedExpr::Framed {
                        frame: frame.clone(),
                        idx: i,
                        inner: Some(self.inner.iter().map(|t| t.lexeme.clone()).collect()),
                    })
                }
                Type::Pointer {
                    inner: pointer_inner_tid,
                    mutable: pointer_inner_mut,
                } => {
                    let inner_tid = pointer_inner_tid.get_inner_accessors(
                        self.token,
                        &self.inner,
                        func,
                        types,
                    )?;

                    stack.push(inner_tid.ptr_of(*pointer_inner_mut, types));
                    Ok(TypedExpr::FramedPointerOffset {
                        frame: frame.clone(),
                        idx: i,
                        inner: self.inner.iter().map(|t| t.lexeme.clone()).collect(),
                    })
                }
                _ => Err(HayError::new_type_err(
                    format!("Cannot access into non-record type `{ft}`"),
                    self.token.loc,
                )),
            }
        } else if let Some(Type::Enum { variants, .. }) =
            types.get(&TypeId::new(&self.ident.lexeme))
        {
            if self.inner.len() != 1 {
                return Err(HayError::new(
                    "Cannot have multiple inner accessor for an enum type.",
                    self.token.loc,
                )
                .with_hint(format!(
                    "Found accessors: {:?}",
                    self.inner
                        .iter()
                        .map(|t| &t.lexeme)
                        .collect::<Vec<&String>>()
                )));
            }

            if !variants.iter().any(|v| v.lexeme == self.inner[0].lexeme) {
                return Err(HayError::new(
                    format!("Unknown enum variant `{}`", self.inner[0].lexeme),
                    self.token.loc,
                )
                .with_hint(format!(
                    "Enum {} has variants: {:?}",
                    self.ident.lexeme,
                    variants.iter().map(|t| &t.lexeme).collect::<Vec<&String>>()
                )));
            }

            stack.push(TypeId::new(&self.ident.lexeme));

            Ok(TypedExpr::Enum {
                typ: TypeId::new(&self.ident.lexeme),
                variant: variants
                    .iter()
                    .find(|t| t.lexeme == self.inner[0].lexeme)
                    .unwrap()
                    .lexeme
                    .clone(),
            })
        } else {
            Err(HayError::new(
                format!("Unknown identifier `{}`", self.ident.lexeme),
                self.token.loc,
            ))
        }
    }
}
