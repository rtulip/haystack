use crate::ast::arg::Arg;
use crate::ast::expr::UntypedExpr;
use crate::ast::stmt::Member;
use crate::error::HayError;
use crate::lex::token::Token;
use crate::types::ConcreteType;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct TypeId(pub String);

impl TypeId {
    pub fn new<S: Into<String>>(id: S) -> Self {
        TypeId(id.into())
    }

    pub fn assign(
        &self,
        token: &Token,
        map: &HashMap<TypeId, TypeId>,
        types: &mut HashMap<TypeId, Type>,
    ) -> Result<TypeId, HayError> {
        if let Some(new_t) = map.get(&self) {
            return Ok(new_t.clone());
        }

        println!("Trying to assign {:?} to {self}", map);

        let maybe_typ = types.get(&self).cloned();
        match maybe_typ {
            Some(typ) => match typ {
                Type::GenericRecordBase {
                    token: base_token,
                    name: name_token,
                    generics,
                    members,
                    kind,
                    ..
                } => {
                    let mut resolved_members = vec![];
                    for m in members {
                        resolved_members.push(Member {
                            vis: m.vis,
                            token: m.token,
                            ident: m.ident,
                            typ: ConcreteType(m.typ.0.assign(token, map, types)?),
                        });
                    }
                    let mut resolved_generics = vec![];
                    for t in generics {
                        resolved_generics.push(t.assign(token, map, types)?);
                    }

                    println!(
                        "Resolved Members: {:?}",
                        resolved_members
                            .iter()
                            .map(|m| (&m.ident.lexeme, &m.typ.0 .0))
                            .collect::<HashMap<&String, &String>>()
                    );

                    println!(
                        "Resolved Generics: {:?}",
                        resolved_generics
                            .iter()
                            .map(|t| (&t.0))
                            .collect::<Vec<&String>>()
                    );

                    let mut name = format!("{self}<");
                    for t in &resolved_generics[0..resolved_generics.len() - 1] {
                        name = format!("{name}{t} ");
                    }

                    let name = TypeId::new(format!("{name}{}>", resolved_generics.last().unwrap()));

                    let t = Type::Record {
                        token: base_token,
                        name: Token {
                            kind: name_token.kind,
                            lexeme: name.0.clone(),
                            loc: name_token.loc,
                        },
                        members: resolved_members,
                        kind: kind,
                    };

                    types.insert(name.clone(), t);
                    Ok(name)
                }
                Type::Pointer { inner } => {
                    let inner = inner.assign(token, map, types)?;
                    let t = Type::Pointer { inner };
                    let id: TypeId = t.id();

                    types.insert(id.clone(), t);
                    Ok(id)
                }
                Type::Char
                | Type::U64
                | Type::U8
                | Type::Bool
                | Type::Enum { .. }
                | Type::Record { .. } => Ok(self.clone()),
                _ => unimplemented!("Havent finished working on {:?}", typ),
            },
            None => todo!(
                "Haven't handled none case yet. This would imply that {} a placeholder type.",
                self.0
            ),
        }
    }
}

impl std::fmt::Display for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum RecordKind {
    Struct,
    Union,
}

#[derive(Debug, Clone)]
pub enum Type {
    U8,
    U64,
    Char,
    Bool,
    Pointer {
        inner: TypeId,
    },
    Record {
        token: Token,
        name: Token,
        members: Vec<Member<ConcreteType>>,
        kind: RecordKind,
    },
    GenericRecordBase {
        token: Token,
        name: Token,
        generics: Vec<TypeId>,
        members: Vec<Member<ConcreteType>>,
        kind: RecordKind,
    },
    GenericRecordInstance {
        base: TypeId, //
        base_generics: Vec<TypeId>,
        alias_list: Vec<TypeId>,
        members: Vec<Member<ConcreteType>>,
        kind: RecordKind,
    },
    Enum {
        token: Token,
        name: Token,
        variants: Vec<Token>,
    },
    UncheckedFunction {
        token: Token,
        name: Token,
        inputs: Vec<Arg<ConcreteType>>,
        outputs: Vec<Arg<ConcreteType>>,
        generics: Vec<TypeId>,
        body: Vec<Box<UntypedExpr>>,
    },
}

impl Type {
    pub fn id(&self) -> TypeId {
        match self {
            Type::U64 => TypeId::new("u64"),
            Type::U8 => TypeId::new("u8"),
            Type::Char => TypeId::new("char"),
            Type::Bool => TypeId::new("bool"),
            Type::Enum { name, .. }
            | Type::GenericRecordBase { name, .. }
            | Type::Record { name, .. } => TypeId::new(&name.lexeme),
            Type::Pointer { inner } => TypeId::new(format!("*{}", inner.0)),
            Type::GenericRecordInstance {
                base, alias_list, ..
            } => {
                let mut name = format!("{base}<");
                for t in &alias_list[0..alias_list.len() - 1] {
                    name = format!("{name}{t} ");
                }
                name = format!("{name}{}>", alias_list.last().unwrap());

                TypeId::new(name)
            }
            Type::UncheckedFunction { .. } => {
                unimplemented!("Haven't implemented name from Unchecked Functions.")
            }
        }
    }
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub inputs: Vec<TypeId>,
    pub outputs: Vec<TypeId>,
}
