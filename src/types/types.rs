use crate::ast::arg::Arg;
use crate::ast::expr::UntypedExpr;
use crate::ast::stmt::Member;
use crate::error::HayError;
use crate::lex::token::{Loc, Token};
use crate::types::Typed;
use std::collections::HashMap;
use std::hash::Hash;

#[derive(Clone, Hash, PartialEq, Eq)]
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
                            typ: Typed(m.typ.0.assign(token, map, types)?),
                        });
                    }
                    let mut resolved_generics = vec![];
                    for t in generics {
                        resolved_generics.push(t.assign(token, map, types)?);
                    }

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

    pub fn size(&self, types: &HashMap<TypeId, Type>) -> Result<usize, HayError> {
        match types.get(&self).unwrap() {
            Type::Bool
            | Type::Char
            | Type::U64
            | Type::U8
            | Type::Enum { .. }
            | Type::Pointer { .. } => Ok(1),
            Type::Record { members, kind, .. } => match kind {
                RecordKind::Struct => {
                    let mut sum = 0;
                    for member in members {
                        sum += member.typ.0.size(types)?;
                    }
                    Ok(sum)
                }
                RecordKind::Union => {
                    let mut max = 0;
                    for member in members {
                        let sz = member.typ.0.size(types)?;
                        if sz > max {
                            max = sz;
                        }
                    }

                    Ok(max)
                }
            },
            Type::GenericRecordBase { .. } | Type::GenericRecordInstance { .. } => {
                Err(HayError::new(
                    "Generic Records do not have a size known at compile time",
                    Loc::new("", 0, 0, 0),
                ))
            }
            Type::CheckedFunction { .. } | Type::UncheckedFunction { .. } => Err(HayError::new(
                "Functions do not have a size",
                Loc::new("", 0, 0, 0),
            )),
        }
    }
}

impl std::fmt::Debug for TypeId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
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
        members: Vec<Member<Typed>>,
        kind: RecordKind,
    },
    GenericRecordBase {
        token: Token,
        name: Token,
        generics: Vec<TypeId>,
        members: Vec<Member<Typed>>,
        kind: RecordKind,
    },
    GenericRecordInstance {
        base: TypeId, //
        base_generics: Vec<TypeId>,
        alias_list: Vec<TypeId>,
        members: Vec<Member<Typed>>,
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
        inputs: Vec<Arg<Typed>>,
        outputs: Vec<Arg<Typed>>,
        generics: Vec<TypeId>,
        body: Vec<Box<UntypedExpr>>,
    },
    CheckedFunction {
        token: Token,
        name: Token,
        inputs: Vec<Arg<Typed>>,
        outputs: Vec<Arg<Typed>>,
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
            Type::CheckedFunction { .. } => {
                unimplemented!("Haven't implemented name from Checked Functions")
            }
        }
    }
}

#[derive(Clone)]
pub struct Signature {
    pub inputs: Vec<TypeId>,
    pub outputs: Vec<TypeId>,
    generics: Option<Vec<TypeId>>,
}

impl Signature {
    pub fn new(inputs: Vec<TypeId>, outputs: Vec<TypeId>) -> Self {
        Self {
            inputs,
            outputs,
            generics: None,
        }
    }

    pub fn new_maybe_generic(
        inputs: Vec<TypeId>,
        outputs: Vec<TypeId>,
        generics: Option<Vec<TypeId>>,
    ) -> Self {
        Self {
            inputs,
            outputs,
            generics: generics,
        }
    }

    pub fn evaluate(&self, token: &Token, stack: &mut Vec<TypeId>) -> Result<(), HayError> {
        if stack.len() < self.inputs.len() {
            return Err(HayError::new_type_err(
                format!("Invalid number of inputs for {:?}", token.lexeme),
                token.loc.clone(),
            )
            .with_hint(format!("Expected: {:?}", self.inputs))
            .with_hint(format!("Found:    {:?}", stack)));
        }

        if self.generics.is_some() {
            todo!("{token} Generic Signatures...");
        }

        for (input, stk) in self.inputs.iter().rev().zip(stack.iter().rev()) {
            if input != stk {
                return Err(HayError::new_type_err(
                    format!("Type Error - Invalid inputs for `{:?}`", token.lexeme).as_str(),
                    token.loc.clone(),
                )
                .with_hint(format!("Expected: {:?}", self.inputs))
                .with_hint(format!(
                    "Found:    {:?}",
                    stack
                        .iter()
                        .rev()
                        .take(self.inputs.len())
                        .rev()
                        .collect::<Vec<&TypeId>>()
                )));
            }
        }

        for _ in &self.inputs {
            stack.pop();
        }

        for out in &self.outputs {
            stack.push(out.clone());
        }

        Ok(())
    }

    pub fn evaluate_many(
        sigs: &[Signature],
        token: &Token,
        stack: &mut Vec<TypeId>,
    ) -> Result<(), HayError> {
        println!("{token} Evaluate Many: {:?}", stack);
        let in_len = sigs[0].inputs.len();
        let out_len = sigs[0].outputs.len();
        if !sigs
            .iter()
            .all(|sig| sig.inputs.len() == in_len && sig.outputs.len() == out_len)
        {
            let mut e = HayError::new(
                "Logic Error - All signatures should have the same input and output lengths for evaluate many.",
                token.loc.clone(),
            ).with_hint("Found these signatures:");

            for sig in sigs {
                e = e.with_hint(format!("{:?}", sig));
            }
            return Err(e);
        }

        for sig in sigs {
            println!("Checking Signature: {:?}", sig);
            println!("Stack: {:?}", stack);
            if let Ok(_) = sig.evaluate(token, stack) {
                return Ok(());
            } else {
                println!("Continuing on i guess: {:?}", stack);
            }
        }

        let mut e = HayError::new_type_err(
            format!("Invalid inputs for `{}`", token.kind),
            token.loc.clone(),
        )
        .with_hint(format!("Expected one of {} signatures:", sigs.len()));

        for sig in sigs {
            e = e.with_hint(format!("  {:?}", sig.inputs));
        }

        e = e.with_hint("Found:");
        e = e.with_hint(format!(
            "  {:?}",
            stack
                .iter()
                .rev()
                .take(in_len)
                .rev()
                .collect::<Vec<&TypeId>>(),
        ));

        Err(e)
    }
}

impl std::fmt::Debug for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "inputs: {:?} outputs: {:?}",
            self.inputs.iter().map(|t| &t.0).collect::<Vec<&String>>(),
            self.outputs.iter().map(|t| &t.0).collect::<Vec<&String>>(),
        )
    }
}
