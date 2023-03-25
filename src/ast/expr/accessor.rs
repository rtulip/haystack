use crate::{
    error::HayError,
    lex::token::Token,
    types::{Frame, RecordKind, Stack, Type, TypeId, TypeMap, UncheckedFunction, VariantType},
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
    /// Converts an untyped Accessor Expression into an [`TypedExpr`]
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

        let tid = TypeId::new(&self.ident.lexeme);

        if let Some((i, (_, ft))) = frame
            .iter()
            .enumerate()
            .rev()
            .find(|(_, (k, _))| k == &self.ident.lexeme)
        {
            match types.get(&ft.typ).unwrap() {
                Type::Variant(VariantType { .. }) => {
                    if self.inner.is_empty() {
                        let mut copied_frame = frame.clone();
                        copied_frame[i].1.typ = ft.typ.supertype(types);
                        stack.push(Type::U64.id());
                        Ok(TypedExpr::Framed {
                            frame: copied_frame,
                            idx: i,
                            inner: Some(vec![]),
                        })
                    } else {
                        let concrete_base = ft.typ.supertype(types);
                        let final_tid = concrete_base.get_inner_accessors(
                            self.token,
                            &self.inner,
                            func,
                            types,
                        )?;
                        let mut copied_frame = frame.clone();
                        copied_frame[i].1.typ = concrete_base;
                        stack.push(final_tid);
                        Ok(TypedExpr::Framed {
                            frame: copied_frame,
                            idx: i,
                            inner: Some(self.inner.iter().map(|t| t.lexeme.clone()).collect()),
                        })
                    }
                }
                Type::Record {
                    kind: RecordKind::EnumStruct,
                    ..
                } => {
                    if self.inner.is_empty() {
                        stack.push(Type::U64.id());
                        Ok(TypedExpr::Framed {
                            frame: frame.clone(),
                            idx: i,
                            inner: Some(vec![]),
                        })
                    } else {
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
                }
                Type::Record { .. } | Type::Tuple { .. } => {
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
        } else if let Some(Type::Enum { variants, .. }) = types.get(&tid) {
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

            let variant_typ = Type::Variant(VariantType {
                base: TypeId::new(&self.ident.lexeme),
                variant: self.inner[0].lexeme.clone(),
            });

            stack.push(variant_typ.id());

            Ok(TypedExpr::Enum {
                typ: TypeId::new(&self.ident.lexeme),
                variant: variants
                    .iter()
                    .find(|t| t.lexeme == self.inner[0].lexeme)
                    .unwrap()
                    .lexeme
                    .clone(),
            })
        } else if let Some(Type::Record {
            members,
            kind: RecordKind::EnumStruct,
            ..
        }) = types.get(&tid)
        {
            if self.inner.len() != 1 {
                return Err(HayError::new(
                    "Cannot have multiple inner accessor for an enum struct type.",
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

            let (idx, m) = match members
                .iter()
                .enumerate()
                .find(|(_, m)| m.ident.lexeme == self.inner[0].lexeme)
            {
                Some((i, m)) => (i, m),
                None => {
                    return Err(HayError::new(
                        format!("Unknown enum struct variant `{}`", self.inner[0].lexeme),
                        self.token.loc,
                    )
                    .with_hint(format!(
                        "Enum struct `{}` has variants: {:?}",
                        self.ident.lexeme,
                        members
                            .iter()
                            .map(|m| &m.ident.lexeme)
                            .collect::<Vec<&String>>()
                    )))
                }
            };

            if m.typ != TypeId::new("[]") {
                return Err(HayError::new(
                    format!(
                        "Enum struct `{}` variant `{}` requires a cast",
                        self.ident.lexeme, self.inner[0].lexeme
                    ),
                    self.token.loc,
                )
                .with_hint(format!(
                    "Variant `{}` has a type {}.",
                    self.inner[0].lexeme, m.typ
                ))
                .with_hint(format!("Try using `cast({})` instead.", self.token.lexeme)));
            }

            let variant_typ = Type::Variant(VariantType {
                base: tid.clone(),
                variant: self.inner[0].lexeme.clone(),
            });

            stack.push(variant_typ.id());
            let padding = tid.size(types)? - 1;

            Ok(TypedExpr::CastEnumStruct { padding, idx })
        } else if let Some(Type::GenericRecordBase {
            kind: RecordKind::EnumStruct,
            generics,
            ..
        }) = types.get(&tid)
        {
            Err(HayError::new(
                format!(
                    "Cannot create instance instance of generic type `{tid}` without annotations."
                ),
                self.token.loc,
            )
            .with_hint(format!("`{tid}` is generic over {generics:?}")))
        } else {
            Err(HayError::new(
                format!("Unknown identifier `{}`", self.ident.lexeme),
                self.token.loc,
            ))
        }
    }
}
