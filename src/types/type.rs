use std::collections::HashSet;

use crate::{
    ast::{
        stmt::{TypeDescription, UserDefinedTypes},
        visibility::Visibility,
    },
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken},
    types::Substitutions,
};

use super::{
    BaseType, FunctionType, PointerType, RecordKind, RecordMember, RecordType, TypeId, TypeVar,
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

    pub fn from_token(
        token: &Token,
        user_defined_types: &UserDefinedTypes,
        free_vars: &FreeVars,
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
        free_vars: &FreeVars,
    ) -> Result<Self, HayError> {
        match typ {
            TypeToken::Array { base, size } => todo!(),
            TypeToken::Associated { base, typ } => todo!(),
            TypeToken::Base(base) => {
                if let Ok(base) = BaseType::try_from(base.as_ref()) {
                    return Ok(Type::Base(base));
                }

                if let Some(TypeDescription::Record(desc)) =
                    user_defined_types.get(&TypeId::new(base))
                {
                    return Ok(Type::Record(desc.typ.clone()));
                }

                if let Some(free_var) = free_vars.get(&TypeVar::new(base)) {
                    return Ok(Type::TypeVar(free_var.clone()));
                }

                if let Some(TypeDescription::PreDeclaration(predecl)) =
                    user_defined_types.get(&TypeId::new(base))
                {
                    return Ok(Type::PreDeclaration(TypeId::new(&predecl.name)));
                }

                todo!("{token}")
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
        }
    }

    pub fn get_inner_accessors(&self, token: &Token, inner: &[Token]) -> Result<Self, HayError> {
        if inner.is_empty() {
            return Ok(self.clone());
        }

        match self {
            Type::Record(record) if record.kind == RecordKind::Struct => {
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
            _ => todo!("{self:?}"),
        }
    }
}
