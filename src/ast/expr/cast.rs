use std::collections::HashMap;

use crate::{
    ast::member::TypedMember,
    error::HayError,
    lex::token::Token,
    types::{
        RecordKind, Signature, Stack, Type, TypeId, TypeMap, UncheckedFunction, Variance,
        VariantType,
    },
};

use super::TypedExpr;

#[derive(Debug, Clone)]
pub struct ExprCast {
    pub token: Token,
    pub typ: Token,
}

impl ExprCast {
    fn deny_private_cast(
        &self,
        cast_typ: &TypeId,
        members: &[TypedMember],
        func: &UncheckedFunction,
        types: &mut TypeMap,
    ) -> Result<(), HayError> {
        if members.iter().any(|m| !m.is_public()) {
            match &func.impl_on {
                Some(tid) => {
                    if Variance::new(cast_typ, tid, types) >= Variance::Contravariant {
                        return Err(HayError::new(
                            format!(
                                "Cannot cast to type `{cast_typ}` because it has private members."
                            ),
                            self.token.loc.clone(),
                        ));
                    }
                }
                None => {
                    return Err(HayError::new(
                        format!("Cannot cast to type `{cast_typ}` because it has private members."),
                        self.token.loc.clone(),
                    ))
                }
            }
        }
        Ok(())
    }

    pub fn type_check(
        self,
        stack: &mut Stack,
        types: &mut TypeMap,
        func: &UncheckedFunction,
        generic_map: &Option<HashMap<TypeId, TypeId>>,
    ) -> Result<TypedExpr, HayError> {
        let typ_id = TypeId::from_token(&self.typ, types, &vec![])?;
        let typ_id = if let Some(map) = generic_map {
            if let Ok(tid) = typ_id.assign(&self.token, map, types) {
                // try to assign for annotated casts
                tid
            } else {
                // If annotation fails, need to use resolution.
                typ_id
            }
        } else {
            typ_id
        };

        let cast_type = if let Some(typ) = types.get(&typ_id) {
            typ.clone()
        } else {
            return Err(HayError::new(
                format!("Cannot cast to unknown type: {typ_id}"),
                self.token.loc,
            ));
        };

        match &cast_type {
            Type::Record { members, kind, .. } => match kind {
                RecordKind::Struct => {
                    self.deny_private_cast(&typ_id, members, func, types)?;
                    Signature::new(
                        members.iter().map(|m| m.typ.clone()).collect(),
                        vec![typ_id.clone()],
                    )
                    .evaluate(
                        &self.token,
                        stack,
                        types,
                        Variance::Covariant,
                    )?;
                    Ok(TypedExpr::Cast { typ: typ_id })
                }
                RecordKind::Union => {
                    let mut sigs = vec![];

                    members.iter().for_each(|m| {
                        sigs.push(Signature::new(vec![m.typ.clone()], vec![typ_id.clone()]));
                    });

                    let padding = typ_id.size(types)? - stack.iter().last().unwrap().size(types)?;
                    Signature::evaluate_many(
                        &sigs,
                        &self.token,
                        stack,
                        types,
                        Variance::Covariant,
                    )?;

                    Ok(TypedExpr::Pad { padding })
                }
                RecordKind::EnumStruct => {
                    let mut e = HayError::new(
                        format!("Cannot cast to enum struct base: `{typ_id}`"),
                        self.token.loc.clone(),
                    )
                    .with_hint("Cast to one of the following variants instead:");

                    for member in members {
                        e = e.with_hint(format!(" - {typ_id}::{}", member.ident.lexeme))
                    }

                    Err(e)
                }
                RecordKind::Tuple => unreachable!(),
                RecordKind::Interface => unreachable!(),
            },
            Type::U64 => {
                Signature::evaluate_many(
                    &vec![
                        Signature::new(vec![Type::U64.id()], vec![Type::U64.id()]),
                        Signature::new(vec![Type::U8.id()], vec![Type::U64.id()]),
                        Signature::new(vec![Type::Bool.id()], vec![Type::U64.id()]),
                        Signature::new(vec![Type::Char.id()], vec![Type::U64.id()]),
                        Signature::new_generic(
                            vec![TypeId::new("*T")],
                            vec![Type::U64.id()],
                            vec![TypeId::new("T")],
                        ),
                        Signature::new_generic(
                            vec![TypeId::new("&T")],
                            vec![Type::U64.id()],
                            vec![TypeId::new("T")],
                        ),
                    ],
                    &self.token,
                    stack,
                    types,
                    Variance::Covariant,
                )?;
                Ok(TypedExpr::Cast { typ: typ_id })
            }
            Type::U8 => {
                Signature::evaluate_many(
                    &vec![
                        Signature::new(vec![Type::U64.id()], vec![Type::U8.id()]),
                        Signature::new(vec![Type::U8.id()], vec![Type::U8.id()]),
                        Signature::new(vec![Type::Bool.id()], vec![Type::U8.id()]),
                        Signature::new(vec![Type::Char.id()], vec![Type::U8.id()]),
                    ],
                    &self.token,
                    stack,
                    types,
                    Variance::Variant,
                )?;
                Ok(TypedExpr::Cast { typ: typ_id })
            }
            Type::Char => {
                Signature::evaluate_many(
                    &vec![
                        Signature::new(vec![Type::U8.id()], vec![Type::Char.id()]),
                        Signature::new(vec![Type::U64.id()], vec![Type::Char.id()]),
                        Signature::new(vec![Type::Char.id()], vec![Type::Char.id()]),
                    ],
                    &self.token,
                    stack,
                    types,
                    Variance::Variant,
                )?;
                Ok(TypedExpr::Cast { typ: typ_id })
            }
            Type::Tuple { inner, .. } => {
                Signature::new(inner.clone(), vec![typ_id.clone()]).evaluate(
                    &self.token,
                    stack,
                    types,
                    Variance::Variant,
                )?;

                Ok(TypedExpr::Cast { typ: typ_id })
            }
            Type::Pointer { .. } => {
                Signature::new(vec![Type::U64.id()], vec![typ_id.clone()]).evaluate(
                    &self.token,
                    stack,
                    types,
                    Variance::Covariant,
                )?;
                Ok(TypedExpr::Cast { typ: typ_id })
            }
            Type::GenericRecordBase {
                generics,
                members,
                kind,
                ..
            } => {
                let tid = match kind {
                    RecordKind::Struct => {
                        self.deny_private_cast(&typ_id, members, func, types)?;
                        Signature::new_generic(
                            members.iter().map(|m| m.typ.clone()).collect(),
                            vec![typ_id.clone()],
                            generics.clone(),
                        )
                        .evaluate(
                            &self.token,
                            stack,
                            types,
                            Variance::Covariant,
                        )?;
                        typ_id
                    }
                    RecordKind::Union => {
                        let sigs = members
                            .iter()
                            .map(|m| {
                                Signature::new_generic(
                                    vec![m.typ.clone()],
                                    vec![typ_id.clone()],
                                    generics.clone(),
                                )
                            })
                            .collect::<Vec<Signature>>();

                        Signature::evaluate_many(
                            &sigs,
                            &self.token,
                            stack,
                            types,
                            Variance::Variant,
                        )?;

                        typ_id
                    }
                    RecordKind::EnumStruct => {
                        let mut e = HayError::new(
                            format!("Cannot cast to enum struct base: `{typ_id}`"),
                            self.token.loc.clone(),
                        )
                        .with_hint("Cast to one of the following variants instead:");

                        for member in members {
                            e = e.with_hint(format!(" - {typ_id}::{}", member.ident.lexeme))
                        }

                        return Err(e);
                    }
                    RecordKind::Interface => unreachable!(),
                    RecordKind::Tuple => unreachable!(),
                };

                Ok(TypedExpr::Cast { typ: tid })
            }
            Type::Enum { .. } => Err(HayError::new_type_err(
                "Casting to enums is unsupported.",
                self.token.loc,
            )),
            Type::Variant(VariantType { base, variant }) => {
                match types
                    .get(base)
                    .unwrap_or_else(|| panic!("{base} should be a recognized type!"))
                {
                    Type::Record {
                        kind: RecordKind::EnumStruct,
                        members,
                        ..
                    } => {
                        let (idx, member) = members
                            .iter()
                            .enumerate()
                            .find(|(_, m)| &m.ident.lexeme == variant)
                            .unwrap_or_else(
                                || panic!("Enum struct `{base}` variant `{variant}` should be real at this point.")
                            );

                        let padding = base.size(types)? - 1 - member.typ.size(types)?;

                        let signature =
                            Signature::new(vec![member.typ.clone()], vec![typ_id.clone()]);

                        signature.evaluate(&self.token, stack, types, Variance::Covariant)?;

                        Ok(TypedExpr::CastEnumStruct { padding, idx })
                    }
                    Type::GenericRecordBase {
                        kind: RecordKind::EnumStruct,
                        members,
                        generics,
                        ..
                    } => {
                        let (idx, member) = members.iter().enumerate().find(|(_, m)| &m.ident.lexeme == variant).unwrap_or_else(
                            || panic!("Enum struct `{base}` variant `{variant}` should be real at this point.")
                        );

                        let member = member.clone();

                        let signature = Signature::new_generic(
                            vec![member.typ.clone()],
                            vec![typ_id.clone()],
                            generics.clone(),
                        );

                        let padding = if let Some(map) =
                            signature.evaluate(&self.token, stack, types, Variance::Covariant)?
                        {
                            stack.last().unwrap().size(types)?
                                - 1
                                - member.typ.assign(&self.token, &map, types)?.size(types)?
                        } else {
                            stack.last().unwrap().size(types)? - 1 - member.typ.size(types)?
                        };

                        Ok(TypedExpr::CastEnumStruct { padding, idx })
                    }
                    _ => Err(HayError::new_type_err(
                        "Casting to non-enum-struct variant is unsupported",
                        self.token.loc,
                    )),
                }
            }
            Type::Bool => unimplemented!(),
            Type::InterfaceBase(_) => unimplemented!(),
            Type::InterfaceInstance(_) => unimplemented!(),
            Type::AssociatedTypeBase(_) => unimplemented!(),
            Type::AssociatedTypeInstance(_) => unimplemented!(),
            Type::GenericFunction { .. }
            | Type::UncheckedFunction { .. }
            | Type::Function { .. }
            | Type::GenericRecordInstance { .. }
            | Type::Stub { .. } => unreachable!(),
            Type::Never => unreachable!("Casting to never types is not supported"),
            Type::RecordPreDeclaration { .. } => unreachable!(),
        }
    }
}
