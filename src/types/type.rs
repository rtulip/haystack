use std::{collections::HashSet, fmt::{Display, Debug}};

use crate::{
    ast::{
        stmt::{TypeDescription, UserDefinedTypes, Interfaces, RecordDescription},
        visibility::Visibility, expr::TypedExpr,
    },
    error::HayError,
    lex::token::{Token, TokenKind, TypeToken, Literal},
    types::Substitutions,
};

use super::{
    BaseType, FunctionType, PointerType, RecordKind, RecordMember, RecordType, Stack, TypeId,
    TypeVar, VariantType, InterfaceType,
};
pub type FreeVars = HashSet<TypeVar>;

#[derive(Clone, PartialEq, Eq)]
pub enum Type {
    Base(BaseType),
    Pointer(PointerType),
    TypeVar(TypeVar),
    Record(RecordType),
    Function(FunctionType),
    PreDeclaration(TypeId),
    Interface(InterfaceType),
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
            Type::Record(RecordType { kind: RecordKind::Union, members, .. }) => todo!(),
            Type::Record(RecordType { kind: RecordKind::EnumStruct, members, .. }) => {
                let mut sizes = vec![];
                for m in members {
                    sizes.push(m.typ.size(token)?);
                }

                Ok(sizes.iter().max().unwrap_or(&0) + 1)
            },
            Type::Record(RecordType { kind: RecordKind::Enum, members, .. }) => todo!(),
            Type::Record(RecordType { kind: RecordKind::Struct | RecordKind::Tuple, members, .. }) => {
                let mut sum = 0;
                for m in members {
                    sum += m.typ.size(token)?;
                }

                Ok(sum)
            },
            Type::TypeVar(_) => Err(HayError::new(format!("TypeVar {self} have unknown size"), token.loc.clone())),
            _ => todo!("{self:?}"),
        }

    }

    pub fn size_unchecked(&self) -> usize {

        let token = Token::default();
        // TODO: DON'T DO THIS unwrap...
        self.size(&token).unwrap_or(1)

    }

    pub fn width(&self) -> usize {
        if matches!(self, Type::Base(BaseType::Char | BaseType::U8)) {
            1
        } else {
            8
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
        interfaces: &Interfaces,
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

        Self::from_type_token(token, typ, user_defined_types, interfaces, free_vars)
    }

    pub fn from_type_token(
        token: &Token,
        typ: &TypeToken,
        user_defined_types: &UserDefinedTypes,
        interfaces: &Interfaces,
        free_vars: Option<&FreeVars>,
    ) -> Result<Self, HayError> {
        match typ {
            TypeToken::Array { base, .. } => {
                let t = Type::from_type_token(token, base, user_defined_types, interfaces, free_vars)?;

                let arr = match user_defined_types.get(&TypeId::new("Arr")) {
                    Some(TypeDescription::Record(record)) => &record.typ,
                    _ => todo!(),
                };

                let subs = Substitutions::new(token, vec![TypeVar::new("T")], vec![t])?;

                Type::Record(arr.clone()).substitute(token, &subs)
            }
            TypeToken::Associated { base, typ } => {
                let t = Type::from_type_token(token, &base, user_defined_types, interfaces, free_vars)?;

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

                return Err(HayError::new(format!("Unknown Base Type: {base}. Free Types: {free_vars:?}"), token.loc.clone()))
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
                } else if let Some(iface) = interfaces.get(base) {        
                    
                    base_typ = iface.typ.clone();
                    
                    ordered_free_vars = iface.ordered_free_vars.clone();
                } else {
                    todo!();
                };

                let mut mapped_types = vec![];
                for inner in inner {
                    mapped_types.push(Type::from_type_token(
                        &token,
                        inner,
                        user_defined_types,
                        interfaces, 
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
                    interfaces, 
                    free_vars,
                )?),
            })),
            TypeToken::Tuple { inner, idents } => {
                let mut inner_types = vec![];
                for inner in inner {
                    inner_types.push(Type::from_token(inner, user_defined_types, interfaces, free_vars)?);
                }

                let idents: Vec<_> = match idents {
                    Some(tokens) => tokens.iter().map(|tok| tok.lexeme.clone()).collect(),
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
                    ordered_free_vars: None,
                }))
            }
        }
    }

    pub fn substitute(self, token: &Token, subs: &Substitutions) -> Result<Self, HayError> {
        match self {
            Type::Base(_) => Ok(self),
            Type::Function(_) => todo!(),
            Type::Interface(InterfaceType { iface, types }) => {
                let mut typs = vec![];
                for t in types {
                    typs.push(t.substitute(token, subs)?);
                }

                Ok(Type::Interface(InterfaceType { iface, types: typs }))
            },
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

                let mut ordered_free_vars = if let Some(ordered_free_vars) = record.ordered_free_vars {

                    let mut new_free_vars = vec![];
                    for t in ordered_free_vars {
                        new_free_vars.push(t.substitute(token, subs)?);
                    }

                    Some(new_free_vars)

                } else {
                    None
                };
                let typ = RecordType {
                    kind: record.kind,
                    ident: record.ident,
                    members,
                    ordered_free_vars,
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
            Type::PreDeclaration(_) => Ok(self),
            Type::Variant(variant) => {
                Ok(Type::Variant(VariantType { variant: variant.variant, typ: Box::new(variant.typ.substitute(token, subs)?) }))
            },
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
                if matches!(record.kind, RecordKind::Struct | RecordKind::Tuple)  =>
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
            (Type::TypeVar(this), Type::TypeVar(var)) if this == var => {
                if subs.get(&var).is_none() {
                    subs.insert(var.clone(), Type::TypeVar(this.clone()));
                }   
            },
            (t, Type::TypeVar(var))  => match subs.get(&var) {
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
                })
                | Type::Variant(VariantType { typ: box Type::Record(RecordType {
                    kind: RecordKind::Struct | RecordKind::EnumStruct,
                    ident: Some(ident),
                    members,
                    ..
                    }), 
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
                })| Type::Variant(VariantType { typ: box Type::Record(RecordType {
                    kind: RecordKind::Struct | RecordKind::EnumStruct,
                    ident: Some(other_ident),
                    members: other_members,
                    ..
                    }), 
                    .. 
                }),
            ) if ident == other_ident => {
                for (t, o) in members.iter().zip(other_members.iter()) {
                    t.typ.unify(token, &o.typ, subs)?;
                }
            }
            (Type::Record(RecordType { ident: Some(ident),.. }), Type::PreDeclaration(predecl)) 
                | (Type::PreDeclaration(predecl), Type::Record(RecordType { ident: Some(ident),.. })) if ident == predecl => (),
            (Type::Interface(this), Type::Interface(that)) if &this.iface == &that.iface => {
                for (t, o) in this.types.iter().zip(that.types.iter()) {
                    t.unify(token, o, subs)?;
                }

            },
            (Type::Variant(this), Type::Variant(that)) if &this.variant == &that.variant => {
                
                this.typ.unify(token, &that.typ, subs)?;

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

    pub fn cast(&self, token: &Token, stack: &mut Stack) -> Result<TypedExpr, HayError> {
        match self {
            Type::Base(base_type) => base_type.cast(token, stack),
            Type::Record(record) => record.cast(token, stack),
            Type::Pointer(pointer) => pointer.cast(token, stack),
            Type::Variant(variant) => variant.cast(token, stack),
            _ => todo!("{self}"),
        }
    }

    pub fn is_generic(&self) -> bool {
        match self {
            Type::Base(_) => false,
            Type::Function(_) => todo!(),
            Type::Interface(_) => todo!(),
            Type::Pointer(pointer) => pointer.inner.is_generic(),
            Type::PreDeclaration(_) => todo!(),
            Type::Record(record) => record.members.iter().any(|member| member.typ.is_generic()),
            Type::TypeVar(_) => true,
            Type::Variant(variant) => variant.typ.is_generic(),
            
        }
    }

    pub fn unify_from_base(&self, token: &Token, interfaces: &Interfaces) -> Result<(String, Substitutions), HayError> {
        
        if let Type::Interface(InterfaceType { iface, .. }) = self {
            
            if let Some(iface_desc) = interfaces.get(iface) {
                                
                let mut subs = Substitutions::empty();
                self.unify(token, &iface_desc.typ, &mut subs)?;
                Ok((iface.clone(), subs))
            } else {
                todo!("Err")
            }

        } else {
            todo!("err");
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
                ordered_free_vars: None,
                ..
            }) => write!(f, "{ident}"),
            Type::Record(RecordType {
                ident: Some(TypeId(ident)),
                ordered_free_vars: Some(ordered_free_vars),
                ..
            }) => write!(f, "{ident}<{}>", ordered_free_vars.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(" ")),
            Type::Record(RecordType {
                ident: None,
                members,
                ..
            }) => {
                write!(f, "[{}]", members.iter().map(|member| format!("{}", member.typ)).collect::<Vec<_>>().join(" "))
            }
            Type::TypeVar(TypeVar(ident)) => write!(f, "{ident}"),
            Type::Variant(variant) => write!(f, "{}::{}", variant.typ, variant.variant),
            Type::Interface(iface) => write!(f, "{}<{}>", iface.iface, iface.types.iter().map(|t| format!("{t}")).collect::<Vec<_>>().join(" ")),
        }
    }
}

impl Debug for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self}")
    }
}
