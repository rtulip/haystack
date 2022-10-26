use super::arg::Arg;
use super::expr::Expr;
use crate::error::HayError;
use crate::lex::token::Token;

use crate::types::{RecordKind, Signature, Type, TypeId, Typed, Untyped};
use std::collections::{BTreeMap, HashMap};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

fn resolve_members(
    members: Vec<Member<Untyped>>,
    types: &mut BTreeMap<TypeId, Type>,
    local_types: &Vec<TypeId>,
) -> Result<Vec<Member<Typed>>, HayError> {
    let mut out = vec![];
    for m in members {
        let typ = Typed(TypeId::from_token(&m.token, types, local_types)?);
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
    types: &mut BTreeMap<TypeId, Type>,
    local_types: &Vec<TypeId>,
) -> Result<Vec<Arg<Typed>>, HayError> {
    let mut out = vec![];

    for arg in args {
        let typ = Typed(TypeId::from_token(&arg.token, types, local_types)?);
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
    types: &BTreeMap<TypeId, Type>,
) -> Result<Vec<TypeId>, HayError> {
    match annotations {
        None => Ok(vec![]),
        Some(annotations) => {
            let mut out = vec![];
            for a in annotations {
                if types.contains_key(&TypeId::new(&a.token.lexeme)) {
                    return Err(HayError::new(format!("Generic type {} cannot be used as it has already been defined elsewhere.", a.token.lexeme), a.token.loc));
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
        body: Vec<Expr>,
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
        expr: Box<Expr>,
    },
}

impl Stmt {
    pub fn add_to_global_scope(
        self,
        types: &mut BTreeMap<TypeId, Type>,
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
                        return Err(HayError::new(
                            format!("Name conflict: `{}` defined elsewhere.", name.lexeme),
                            name.loc,
                        ))
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
                        return Err(HayError::new(
                            format!("Name conflict. `{}` defined elsewhere", name.lexeme),
                            name.loc,
                        ))
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

                let sig = Signature::new_maybe_generic(
                    inputs.iter().map(|arg| arg.typ.0.clone()).collect(),
                    outputs.iter().map(|arg| arg.typ.0.clone()).collect(),
                    if generics.is_empty() {
                        None
                    } else {
                        Some(generics.clone())
                    },
                );

                let typ = if generics.is_empty() {
                    Type::UncheckedFunction {
                        token,
                        name: name.clone(),
                        inputs,
                        outputs,
                        body,
                        generic_map: None,
                    }
                } else {
                    Type::GenericFunction {
                        token,
                        name: name.clone(),
                        inputs,
                        outputs,
                        generics,
                        body,
                    }
                };

                match types.insert(TypeId::new(&name.lexeme), typ) {
                    None => {
                        global_env.insert(name.lexeme, sig);
                    }
                    Some(_) => {
                        return Err(HayError::new(
                            format!(
                                "Function name conflict. `{}` defined elsewhere",
                                name.lexeme
                            ),
                            name.loc,
                        ));
                    }
                }
            }
            Stmt::Var { token, expr } => {
                if let Expr::Var { token, typ, ident } = *expr {
                    let inner = TypeId::from_token(&typ, types, &vec![])?;
                    let ptr = Type::Pointer { inner };
                    let id = ptr.id();
                    types.insert(ptr.id(), ptr);

                    let sig = Signature::new(vec![], vec![id]);

                    match global_env.insert(ident.lexeme.clone(), sig) {
                        None => (),
                        Some(_) => {
                            return Err(HayError::new(
                                format!("Var conflict. `{}` defined elsewhere", ident.lexeme),
                                token.loc,
                            ))
                        }
                    }
                } else {
                    return Err(HayError::new(
                        format!("{}: Logic Error -- Assertion failed. Expected Expr::Var from Stmt::Var", line!()),
                        token.loc,
                    ));
                }
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
