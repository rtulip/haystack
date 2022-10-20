use super::arg::Arg;
use super::expr::UntypedExpr;
use crate::error::HayError;
use crate::lex::token::{Token, TokenKind, TypeToken};

use crate::types::{ConcreteType, RecordKind, Signature, Type, TypeId, Untyped};
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Visitiliby {
    Public,
    Private,
}

#[derive(Debug, Clone)]
pub struct Member<TypeState> {
    pub vis: Visitiliby,
    pub token: Token,
    pub ident: Token,
    pub typ: TypeState,
}

pub fn type_id_from_type_token(
    token: &Token,
    typ: &TypeToken,
    types: &mut HashMap<TypeId, Type>,
    local_types: &Vec<TypeId>,
) -> Result<TypeId, HayError> {
    match typ {
        TypeToken::Array { .. } => unimplemented!("Haven't implemented getting array types"),
        TypeToken::Base(base) => {
            if types.contains_key(&TypeId::new(base))
                || local_types.iter().find(|t| &t.0 == base).is_some()
            {
                Ok(TypeId(base.clone()))
            } else {
                HayError::new(format!("Unrecognized type: {base}"), token.loc.clone())
            }
        }
        TypeToken::Parameterized { base, inner } => {
            let base_tid = TypeId::new(base);
            match types.get(&base_tid).cloned() {
                Some(Type::GenericRecordBase {
                    generics,
                    members,
                    kind,
                    ..
                }) => {
                    if generics.len() != inner.len() {
                        return HayError::new(format!("Incorrect number of type annotations provided. Expected annotations for {:?}", generics), token.loc.clone());
                    }

                    let mut annotations = vec![];
                    for t in inner {
                        annotations.push(type_id_from_type_token(&token, t, types, local_types)?);
                    }

                    if annotations.iter().any(|t| types.get(t).is_none()) {
                        let t = Type::GenericRecordInstance {
                            base: TypeId::new(base),
                            base_generics: generics.clone(),
                            alias_list: annotations,
                            members: members.clone(),
                            kind: kind.clone(),
                        };
                        let tid = t.id();

                        types.insert(tid.clone(), t);
                        Ok(tid)
                    } else {
                        let map: HashMap<TypeId, TypeId> =
                            generics.into_iter().zip(annotations.into_iter()).collect();
                        println!("{token}: Assigning {base_tid} with {:?}", map);
                        base_tid.assign(token, &map, types)
                    }
                }
                Some(_) => {
                    return HayError::new(
                        format!("Type {base} cannot be annotated, because it is not generic."),
                        token.loc.clone(),
                    );
                }
                None => {
                    return HayError::new(
                        format!("Unrecognized base type: {base}"),
                        token.loc.clone(),
                    )
                }
            }
        }
        TypeToken::Pointer(inner) => {
            let inner_typ_id = type_id_from_type_token(token, inner, types, local_types)?;
            let ident = format!("*{}", inner_typ_id.0);
            let typ = Type::Pointer {
                inner: inner_typ_id,
            };

            types.insert(TypeId::new(ident.clone()), typ);

            Ok(TypeId(ident))
        }
    }
}

pub fn type_id_from_token(
    token: &Token,
    types: &mut HashMap<TypeId, Type>,
    local_types: &Vec<TypeId>,
) -> Result<TypeId, HayError> {
    if types.contains_key(&TypeId::new(&token.lexeme))
        || local_types.iter().find(|t| t.0 == token.lexeme).is_some()
    {
        return Ok(TypeId(token.lexeme.clone()));
    }

    let typ = match &token.kind {
        TokenKind::Type(typ) => typ,
        _ => panic!("Didn't expect this...: {:?}", token.kind),
    };

    type_id_from_type_token(token, typ, types, local_types)
}

fn resolve_members(
    members: Vec<Member<Untyped>>,
    types: &mut HashMap<TypeId, Type>,
    local_types: &Vec<TypeId>,
) -> Result<Vec<Member<ConcreteType>>, HayError> {
    let mut out = vec![];
    for m in members {
        let typ = ConcreteType(type_id_from_token(&m.token, types, local_types)?);
        out.push(Member {
            vis: m.vis,
            token: m.token,
            ident: m.ident,
            typ,
        })
    }

    Ok(out)
}

fn resolve_arguments(
    args: Vec<Arg<Untyped>>,
    types: &mut HashMap<TypeId, Type>,
    local_types: &Vec<TypeId>,
) -> Result<Vec<Arg<ConcreteType>>, HayError> {
    let mut out = vec![];

    for arg in args {
        let typ = ConcreteType(type_id_from_token(&arg.token, types, local_types)?);
        out.push(Arg {
            token: arg.token,
            ident: arg.ident,
            typ,
        });
    }

    Ok(out)
}

fn bulid_local_generics(
    annotations: Option<Vec<Arg<Untyped>>>,
    types: &HashMap<TypeId, Type>,
) -> Result<Vec<TypeId>, HayError> {
    match annotations {
        None => Ok(vec![]),
        Some(annotations) => {
            let mut out = vec![];
            for a in annotations {
                if types.contains_key(&TypeId::new(&a.token.lexeme)) {
                    return HayError::new(format!("Generic type {} cannot be used as it has already been defined elsewhere.", a.token.lexeme), a.token.loc.clone());
                }
                out.push(TypeId(a.token.lexeme));
            }
            Ok(out)
        }
    }
}

#[derive(Clone)]
pub enum Stmt {
    Function {
        token: Token,
        name: Token,
        inputs: Vec<Arg<Untyped>>,
        outputs: Vec<Arg<Untyped>>,
        annotations: Option<Vec<Arg<Untyped>>>,
        body: Vec<Box<UntypedExpr>>,
    },
    Record {
        token: Token,
        name: Token,
        annotations: Option<Vec<Arg<Untyped>>>,
        members: Vec<Member<Untyped>>,
        kind: RecordKind,
    },
    Enum {
        token: Token,
        name: Token,
        variants: Vec<Token>,
    },
    Var {
        token: Token,
        expr: Box<UntypedExpr>,
    },
}

impl Stmt {
    pub fn add_to_global_scope(
        self,
        types: &mut HashMap<TypeId, Type>,
        global_env: &mut HashMap<String, Signature>,
    ) -> Result<(), HayError> {
        match self {
            Stmt::Record {
                token,
                name,
                annotations,
                members,
                kind,
            } => {
                let generics = bulid_local_generics(annotations, types)?;
                let members = resolve_members(members, types, &generics)?;

                let prev = match generics.len() {
                    0 => types.insert(
                        TypeId::new(&name.lexeme),
                        Type::Record {
                            token,
                            name: name.clone(),
                            members,
                            kind,
                        },
                    ),
                    _ => types.insert(
                        TypeId::new(&name.lexeme),
                        Type::GenericRecordBase {
                            token,
                            name: name.clone(),
                            generics,
                            members,
                            kind,
                        },
                    ),
                };

                match prev {
                    None => (),
                    Some(_) => {
                        return HayError::new(
                            format!("Name conflict: `{}` defined elsewhere.", name.lexeme),
                            name.loc,
                        )
                    }
                }
            }
            Stmt::Enum {
                token,
                name,
                variants,
            } => {
                let tid = TypeId::new(&name.lexeme);
                let t = Type::Enum {
                    token,
                    name: name.clone(),
                    variants,
                };
                match types.insert(tid, t) {
                    None => (),
                    Some(_) => {
                        return HayError::new(
                            format!("Name conflict. `{}` defined elsewhere", name.lexeme),
                            name.loc,
                        )
                    }
                }
            }
            Stmt::Function {
                token,
                name,
                inputs,
                outputs,
                annotations,
                body,
            } => {
                let generics = bulid_local_generics(annotations, types)?;
                let inputs = resolve_arguments(inputs, types, &generics)?;
                let outputs = resolve_arguments(outputs, types, &generics)?;

                let sig = Signature {
                    inputs: inputs.iter().map(|arg| arg.typ.0.clone()).collect(),
                    outputs: outputs.iter().map(|arg| arg.typ.0.clone()).collect(),
                };

                match types.insert(
                    TypeId::new(&name.lexeme),
                    Type::UncheckedFunction {
                        token,
                        name: name.clone(),
                        generics,
                        inputs,
                        outputs,
                        body,
                    },
                ) {
                    None => {
                        global_env.insert(name.lexeme, sig);
                    }
                    Some(_) => {
                        return HayError::new(
                            format!("Function name conflict. `{}`", name.lexeme),
                            name.loc,
                        );
                    }
                }
            }
            Stmt::Var { token, expr } => {
                return HayError::new("Var Statements aren't supported yet", token.loc);
            }
        }

        Ok(())
    }
}

impl std::fmt::Debug for Stmt {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Stmt::Function {
                token, name, body, ..
            } => {
                writeln!(f, "[{}] fn {}:", token.loc, name.lexeme)?;
                for e in body {
                    writeln!(f, "  {e}")?;
                }

                Ok(())
            }
            Stmt::Record {
                token,
                name,
                members,
                kind,
                ..
            } => {
                writeln!(
                    f,
                    "[{}] {} {}:",
                    token.loc,
                    match kind {
                        RecordKind::Union => "union",
                        RecordKind::Struct => "struct",
                    },
                    name.lexeme
                )?;
                for mem in members {
                    writeln!(f, "  {}: {}", mem.token.lexeme, mem.ident.lexeme)?;
                }

                Ok(())
            }
            Stmt::Enum {
                token,
                name,
                variants,
            } => {
                writeln!(f, "[{}] enum {}:", token.loc, name.lexeme)?;
                for v in variants {
                    writeln!(f, "  {}", v.lexeme)?;
                }

                Ok(())
            }
            Stmt::Var { token, expr } => {
                write!(f, "[{}] {expr}", token.loc)
            }
        }
    }
}
