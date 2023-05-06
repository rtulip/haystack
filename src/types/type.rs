use std::{collections::HashSet, fmt::Display};

use crate::{
    ast::{
        stmt::{TypeDescription, UserDefinedTypes},
        visibility::Visibility,
    },
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken, Literal},
    types::Substitutions,
};

use super::{
    BaseType, FunctionType, PointerType, RecordKind, RecordMember, RecordType, Stack, TypeId,
    TypeVar, VariantType,
};
pub type FreeVars = HashSet<TypeVar>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Base(BaseType),
    Pointer(PointerType),
    TypeVar(TypeVar),
    Record(RecordType),
    Function(FunctionType),
    PreDeclaration(TypeId),
    Variant(VariantType),
}

impl Type {
    pub fn u64() -> Self {
        Type::Base(BaseType::U64)
    }

    pub fn u8() -> Self {
        Type::Base(BaseType::U8)
    }

    pub fn bool() -> Self {
        Type::Base(BaseType::Bool)
    }

    pub fn char() -> Self {
        Type::Base(BaseType::Char)
    }

    pub fn never() -> Self {
        Type::Base(BaseType::Never)
    }

    pub fn size(&self, token: &Token) -> Result<usize, HayError> {

        match self {
            Type::Base(base) if base != &BaseType::Never => Ok(1),
            Type::Pointer(_) => Ok(1),
            _ => todo!("{self}"),
        }

    }

    pub fn merge_free_vars(a: Option<&FreeVars>, b: Option<&FreeVars>) -> Option<FreeVars> {
        match (a, b) {
            (Some(a), None) => Some(a.clone()),
            (None, Some(b)) => Some(b.clone()),
            (Some(a), Some(b)) => {
                let mut free_vars = FreeVars::from_iter(a.clone().into_iter());
                free_vars.extend(b.clone());
                Some(free_vars)
            }
            (None, None) => None,
        }
    }

    pub fn from_token(
        token: &Token,
        user_defined_types: &UserDefinedTypes,
        free_vars: Option<&FreeVars>,
    ) -> Result<Self, HayError> {
        let typ = match &token.kind {
            TokenKind::Type(typ) => typ,
            _ => {
                return Err(HayError::new(
                    format!(
                        "Cannot extract type from {}, as it is not a type token!",
                        token.lexeme
                    ),
                    token.loc.clone(),
                ))
            }
        };

        Self::from_type_token(token, typ, user_defined_types, free_vars)
    }

    pub fn from_type_token(
        token: &Token,
        typ: &TypeToken,
        user_defined_types: &UserDefinedTypes,
        free_vars: Option<&FreeVars>,
    ) -> Result<Self, HayError> {
        match typ {
            TypeToken::Array { base, size } => {
                let t = Type::from_type_token(token, base, user_defined_types, free_vars)?;

                let arr = match user_defined_types.get(&TypeId::new("Arr")) {
                    Some(TypeDescription::Record(record)) => &record.typ,
                    _ => todo!(),
                };

                let subs = Substitutions::new(token, vec![TypeVar::new("T")], vec![t])?;

                Type::Record(arr.clone()).substitute(token, &subs)
            }
            TypeToken::Associated { base, typ } => {
                let t = Type::from_type_token(token, &base, user_defined_types, free_vars)?;

                let variant = match &t {
                    Type::Record(RecordType {
                        kind: RecordKind::EnumStruct,
                        members,
                        ..
                    }) => {
                        if members.iter().find(|m| &m.ident == typ).is_none() {
                            todo!()
                        }
                        Type::Variant(VariantType {
                            variant: typ.clone(),
                            typ: Box::new(t),
                        })
                    }
                    _ => todo!(),
                };

                Ok(variant)
            }
            TypeToken::Base(base) => {
                if let Ok(base) = BaseType::try_from(base.as_ref()) {
                    return Ok(Type::Base(base));
                }

                if let Some(TypeDescription::Record(desc)) =
                    user_defined_types.get(&TypeId::new(base))
                {
                    return Ok(Type::Record(desc.typ.clone()));
                }

                if let Some(free_vars) = free_vars {
                    if let Some(free_var) = free_vars.get(&TypeVar::new(base)) {
                        return Ok(Type::TypeVar(free_var.clone()));
                    }
                }
                
                if let Some(TypeDescription::PreDeclaration(predecl)) =
                    user_defined_types.get(&TypeId::new(base))
                {
                    return Ok(Type::PreDeclaration(TypeId::new(&predecl.name)));
                }

                todo!("{token}: {typ} {free_vars:?}")
            }
            TypeToken::Parameterized { base, inner } => {
                let (base_typ, ordered_free_vars);
                if let Some(TypeDescription::Record(desc)) =
                    user_defined_types.get(&TypeId::new(base))
                {
                    base_typ = Type::Record(desc.typ.clone());
                    if let Some(desc_free_vars) = &desc.free_vars {
                        ordered_free_vars = desc_free_vars.clone();
                    } else {
                        todo!()
                    }
                } else {
                    todo!();
                };

                let mut mapped_types = vec![];
                for inner in inner {
                    mapped_types.push(Type::from_type_token(
                        &token,
                        inner,
                        user_defined_types,
                        free_vars,
                    )?);
                }

                let subs = Substitutions::new(&token, ordered_free_vars, mapped_types)?;
                let typ = base_typ.substitute(&token, &subs)?;

                Ok(typ)
            }
            TypeToken::Pointer { inner, mutable } => Ok(Type::Pointer(PointerType {
                mutable: *mutable,
                inner: Box::new(Type::from_type_token(
                    token,
                    inner,
                    user_defined_types,
                    free_vars,
                )?),
            })),
            TypeToken::Tuple { inner, idents } => {
                let mut inner_types = vec![];
                for inner in inner {
                    inner_types.push(Type::from_token(inner, user_defined_types, free_vars)?);
                }

                let idents: Vec<_> = match idents {
                    Some(tokens) => tokens.iter().map(|tok| token.lexeme.clone()).collect(),
                    None => (0..inner.len())
                        .into_iter()
                        .map(|i| format!("{i}"))
                        .collect(),
                };

                assert!(inner_types.len() == idents.len());
                let members = inner_types
                    .into_iter()
                    .zip(idents.into_iter())
                    .map(|(typ, ident)| RecordMember {
                        ident,
                        vis: Visibility::Public,
                        typ,
                    })
                    .collect();

                Ok(Type::Record(RecordType {
                    kind: RecordKind::Tuple,
                    ident: None,
                    members,
                }))
            }
        }
    }

    pub fn substitute(self, token: &Token, subs: &Substitutions) -> Result<Self, HayError> {
        match self {
            Type::Base(_) => Ok(self),
            Type::Function(_) => todo!(),
            Type::Record(record)
                if matches!(
                    record.kind,
                    RecordKind::Struct | RecordKind::EnumStruct | RecordKind::Tuple
                ) =>
            {
                let mut members = vec![];
                for member in record.members {
                    members.push(RecordMember {
                        ident: member.ident,
                        vis: member.vis,
                        typ: member.typ.substitute(token, subs)?,
                    });
                }

                let typ = RecordType {
                    kind: record.kind,
                    ident: record.ident,
                    members,
                };

                Ok(Type::Record(typ))
            }
            Type::Record(record) => {
                todo!("{:?}", record.kind)
            }
            Type::Pointer(ptr) => Ok(Type::Pointer(PointerType {
                mutable: ptr.mutable,
                inner: Box::new(ptr.inner.substitute(token, subs)?),
            })),
            Type::TypeVar(var) => {
                if let Some(typ) = subs.get(&var) {
                    Ok(typ.clone())
                } else {
                    Ok(Type::TypeVar(var))
                }
            }
            Type::PreDeclaration(_) => todo!(),
            Type::Variant(_) => todo!(),
        }
    }

    pub fn get_inner_accessors(&self, token: &Token, inner: &[Token]) -> Result<Self, HayError> {
        if inner.is_empty() {
            return Ok(self.clone());
        }

        match self {
            Type::Variant(VariantType {
                typ: box Type::Record(record),
                ..
            })
            | Type::Record(record)
                if record.kind == RecordKind::Struct =>
            {
                if let Some(m) = record
                    .members
                    .iter()
                    .find(|m| &m.ident == &inner.first().unwrap().lexeme)
                {
                    return Ok(m.typ.get_inner_accessors(token, &inner[1..])?);
                } else {
                    return Err(HayError::new_type_err(
                        format!(
                            "{} `{}` doesn't have a member `{}`",
                            record.kind,
                            token.lexeme,
                            &inner.first().unwrap().lexeme,
                        ),
                        token.loc.clone(),
                    )
                    .with_hint(format!(
                        "`{}` has the following members: {:?}",
                        token.lexeme,
                        record.members.iter().map(|m| &m.ident).collect::<Vec<_>>()
                    )));
                }
            }
            Type::Variant(VariantType {
                typ: box Type::Record(record),
                ..
            })
            | Type::Record(record)
                if record.kind == RecordKind::EnumStruct =>
            {
                match &inner[0].kind {
                    TokenKind::Literal(Literal::U64(n)) => {
                        if *n as usize >= record.members.len() {
                            return Err(HayError::new(
                                format!(
                                    "{n} is out of range for `{self}`. Expected a value between 0 and {} inclusive.", 
                                    record.members.len() -1
                                ), 
                                token.loc.clone()
                            ));
                        }

                        return Ok(record.members[*n as usize].typ.get_inner_accessors(token, &inner[1..])?);
                    },
                    kind => return Err(
                        HayError::new(
                            format!("Internal Error: Expected a number literal to access into `{self}`, but found {kind} instead."), 
                            token.loc.clone()
                    ))
                }
            }
            Type::Pointer(PointerType { mutable, inner: inner_typ }) => {
                Ok(Type::Pointer(PointerType { mutable: *mutable, inner: Box::new(inner_typ.get_inner_accessors(token, inner)?)}))
            }
            _ => todo!("{self:?}"),
        }
    }

    pub fn unify(
        &self,
        token: &Token,
        other: &Type,
        subs: &mut Substitutions,
    ) -> Result<(), HayError> {
        match (self, other) {
            (Type::TypeVar(this), Type::TypeVar(var)) if this == var => (),
            (t, Type::TypeVar(var)) | (Type::TypeVar(var), t) => match subs.get(&var) {
                Some(sub) if sub == t => (),
                Some(sub) if matches!(sub, Type::TypeVar(_)) => {self.unify(token, &sub.clone(), subs)?;},
                Some(_) => {
                    t.clone().substitute(token, subs)?;
                },
                None => {
                    subs.insert(var.clone(), t.clone());
                }
            },
            (
                Type::Pointer(PointerType {
                    mutable: true,
                    inner: left,
                    ..
                }),
                Type::Pointer(PointerType {
                    mutable: true,
                    inner: right,
                }),
            ) => left.unify(token, right, subs)?,
            (
                Type::Pointer(PointerType { inner: left, .. }),
                Type::Pointer(PointerType {
                    mutable: false,
                    inner: right,
                }),
            ) => left.unify(token, right, subs)?,
            (
                Type::Record(RecordType {
                    kind: RecordKind::Struct | RecordKind::EnumStruct,
                    ident: Some(ident),
                    members,
                    ..
                }),
                Type::Record(RecordType {
                    kind: RecordKind::Struct | RecordKind::EnumStruct,
                    ident: Some(other_ident),
                    members: other_members,
                    ..
                }),
            ) if ident == other_ident => {
                for (t, o) in members.iter().zip(other_members.iter()) {
                    t.typ.unify(token, &o.typ, subs)?;
                }
            }
            (a, b) if a == b => (),
            (a, b) => {
                return Err(HayError::new(
                    format!("Cannot unify {a} and {b}"),
                    token.loc.clone(),
                ))
            }
        }

        Ok(())
    }

    pub fn cast(&self, token: &Token, stack: &mut Stack) -> Result<(), HayError> {
        match self {
            Type::Base(base_type) => base_type.cast(token, stack),
            Type::Record(record) => record.cast(token, stack),
            Type::Pointer(pointer) => pointer.cast(token, stack),
            Type::Variant(variant) => variant.cast(token, stack),
            _ => todo!("{self}"),
        }
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Base(base) => write!(f, "{base}"),
            Type::Function(func) => write!(f, "{func}"),
            Type::Pointer(ptr) => write!(f, "{}{}", if ptr.mutable { "*" } else { "&" }, ptr.inner),
            Type::PreDeclaration(predecl) => write!(f, "{}", predecl.0),
            Type::Record(RecordType {
                ident: Some(TypeId(ident)),
                ..
            }) => write!(f, "{ident}"),
            Type::Record(RecordType {
                ident: None,
                // members,
                ..
            }) => {
                todo!()
            }
            Type::TypeVar(TypeVar(ident)) => write!(f, "{ident}"),
            Type::Variant(variant) => write!(f, "{}::{}", variant.typ, variant.variant),
        }
    }
}
