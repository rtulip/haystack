use crate::compiler::{compiler_error, evaluate_signature};
use crate::ir::{
    function::{Function, LocalVar},
    keyword::Keyword,
    literal::Literal,
    operator::Operator,
    token::{Token, TokenKind},
    types::{Signature, Type, TypeName, Visibility},
    FnTable, Frame, Stack,
};
use serde::{Deserialize, Serialize};
use std::collections::{BTreeMap, HashMap};

#[derive(Serialize, Deserialize, Clone, PartialEq)]
pub enum OpKind {
    PushInt(u64),
    PushBool(bool),
    PushString(String),
    PushEnum { typ: TypeName, idx: usize },
    Add,
    Sub,
    Mul,
    Div,
    LessThan,
    LessEqual,
    GreaterThan,
    GreaterEqual,
    Equals,
    NotEquals,
    Mod,
    Read(Option<(usize, usize)>),
    Write(Option<(usize, usize)>),
    Cast(TypeName),
    Pad(usize),
    SizeOf(TypeName),
    Global(String),
    Word(String),
    Ident(String, Vec<String>),
    MakeIdent { ident: String, size: Option<usize> },
    PushFramed { offset: isize, size: usize },
    PushIdent { index: usize, inner: Vec<String> },
    PushLocal(String),
    PushLocalPtr(usize),
    Syscall(u64),
    Call(String, Vec<TypeName>),
    PrepareFunc,
    JumpCond(Option<usize>),
    Jump(Option<usize>),
    JumpDest(usize),
    StartBlock,
    EndBlock,
    DestroyFramed { type_name: Option<TypeName> },
    ReleaseFramed(Option<usize>),
    Return,
    Default,
    Nop(Keyword),
}

impl std::fmt::Debug for OpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpKind::PushInt(i) => write!(f, "Push({i})"),
            OpKind::PushBool(b) => write!(f, "Push({b})"),
            OpKind::PushString(s) => write!(f, "Push({s})"),
            OpKind::PushEnum { typ, idx } => {
                write!(f, "Push({typ}::{idx})")
            }
            OpKind::Add => write!(f, "+"),
            OpKind::Sub => write!(f, "-"),
            OpKind::Mul => write!(f, "*"),
            OpKind::Div => write!(f, "/"),
            OpKind::LessThan => write!(f, "<"),
            OpKind::LessEqual => write!(f, "<="),
            OpKind::GreaterThan => write!(f, ">"),
            OpKind::GreaterEqual => write!(f, ">="),
            OpKind::Equals => write!(f, "=="),
            OpKind::NotEquals => write!(f, "!="),
            OpKind::Mod => write!(f, "%"),
            OpKind::Read(_) => write!(f, "@"),
            OpKind::Write(_) => write!(f, "!"),
            OpKind::Cast(typ) => write!(f, "Cast({typ})"),
            OpKind::Pad(n) => write!(f, "Pad({n})"),
            OpKind::SizeOf(typ) => write!(f, "SizeOf({typ})"),
            OpKind::Global(s) => write!(f, "Global({s})"),
            OpKind::Word(s) => write!(f, "Word({s})"),
            OpKind::Ident(s, fs) => write!(f, "Ident({s}::{:?}", fs),
            OpKind::MakeIdent { ident: s, size } => write!(f, "MakeIdent({s}, {:?})", size),
            OpKind::PushIdent { index: i, inner } => {
                write!(f, "PushIdent({i}")?;
                inner.iter().for_each(|x| write!(f, "::{x}").unwrap());
                write!(f, ")")
            }
            OpKind::PushFramed { offset, size } => write!(f, "PushFrame({offset}:{size})"),
            OpKind::PushLocal(ident) => write!(f, "PushLocal({ident})"),
            OpKind::PushLocalPtr(offset) => write!(f, "PushLocalPtr({offset})"),
            OpKind::Syscall(n) => write!(f, "Syscall({n})"),
            OpKind::Call(func, _s) => write!(f, "Call({func})"),
            OpKind::PrepareFunc => write!(f, "PrepareFunc"),
            OpKind::JumpCond(Some(dest)) => write!(f, "JumpCond({dest})"),
            OpKind::JumpCond(None) => unreachable!(),
            OpKind::Jump(Some(dest)) => write!(f, "Jump({dest})"),
            OpKind::Jump(None) => unreachable!(),
            OpKind::JumpDest(dest) => write!(f, "JumpDest({dest})"),
            OpKind::StartBlock => write!(f, "StartBlock"),
            OpKind::EndBlock => write!(f, "EndBlock()"),
            OpKind::DestroyFramed { type_name, .. } => write!(f, "DestroyFramed({:?})", type_name),
            OpKind::ReleaseFramed(width) => write!(f, "ReleaseFramed({:?})", width),
            OpKind::Return => write!(f, "Return"),
            OpKind::Default => write!(f, "Default"),
            OpKind::Nop(kw) => write!(f, "Marker({:?})", kw),
        }
    }
}

impl Default for OpKind {
    fn default() -> Self {
        OpKind::Default
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Default, PartialEq)]
pub struct Op {
    pub kind: OpKind,
    pub token: Token,
}

impl Op {
    fn get_type_from_frame(
        &self,
        frame: &Frame,
        index: usize,
        inner: &[String],
        type_map: &HashMap<TypeName, Type>,
        type_visibility: &Option<TypeName>,
    ) -> (TypeName, usize) {
        let mut t = frame[index].clone();
        let mut t_offset: usize = frame[0..index]
            .iter()
            .map(|t| type_map.get(t).unwrap().size(&self.token, type_map) * 8)
            .sum();
        for field in inner {
            let (new_t, new_offset) = match type_map.get(&t).unwrap() {
                Type::Struct {
                    name,
                    ref members,
                    ref visibility,
                    ref idents,
                }
                | Type::ResolvedStruct {
                    name,
                    ref members,
                    ref visibility,
                    ref idents,
                    ..
                } => {
                    if idents.contains(&field.clone()) {
                        let idx = idents.iter().position(|s| s == &field.clone()).unwrap();
                        if matches!(visibility[idx], Visibility::Private) {
                            let cmp_type_name = match type_map.get(&t).unwrap() {
                                Type::Struct { name, .. } => name,
                                Type::ResolvedStruct { base, .. } => base,
                                _ => unreachable!(),
                            };

                            if let Some(ref vis_name) = type_visibility {
                                if cmp_type_name != vis_name {
                                    compiler_error(
                                        &self.token,
                                        format!(
                                            "Struct member `{field}` is private and cannot accessed"
                                        )
                                        .as_str(),
                                        vec![
                                            "Private members can only be accessed inside an impl block",
                                            format!("{cmp_type_name} {vis_name}").as_str()
                                        ],
                                    )
                                }
                            } else {
                                compiler_error(
                                    &self.token,
                                    format!(
                                        "Struct member `{field}` is private and cannot accessed"
                                    )
                                    .as_str(),
                                    vec![
                                        "Private members can only be accessed inside an impl block",
                                    ],
                                )
                            }
                        }
                        (
                            members[idx].clone(),
                            members[idx + 1..]
                                .iter()
                                .map(|t| {
                                    type_map.get(t).unwrap().size(&self.token, type_map)
                                        * type_map.get(t).unwrap().width()
                                })
                                .sum::<usize>(),
                        )
                    } else {
                        compiler_error(
                            &self.token,
                            format!("Struct `{name}` doesn't have a field: `{field}`").as_str(),
                            vec![format!(
                                "Struct `{name}` has these fields: {:?}",
                                members
                                    .iter()
                                    .zip(idents.iter())
                                    .collect::<Vec<(&TypeName, &String)>>()
                            )
                            .as_str()],
                        );
                    }
                }
                Type::Union {
                    name,
                    ref members,
                    ref visibility,
                    ref idents,
                }
                | Type::ResolvedUnion {
                    name,
                    ref members,
                    ref visibility,
                    ref idents,
                    ..
                } => {
                    if idents.contains(&field.clone()) {
                        let idx = idents.iter().position(|s| s == &field.clone()).unwrap();
                        if matches!(visibility[idx], Visibility::Private) {
                            compiler_error(
                                &self.token,
                                format!("Union member `{field}` is private and cannot accessed")
                                    .as_str(),
                                vec!["Private members can only be accessed inside an impl block"],
                            )
                        }
                        let delta = type_map.get(&t).unwrap().size(&self.token, type_map)
                            * type_map.get(&t).unwrap().width()
                            - type_map
                                .get(&members[idx])
                                .unwrap()
                                .size(&self.token, type_map)
                                * type_map.get(&members[idx]).unwrap().width();
                        (members[idx].clone(), delta)
                    } else {
                        compiler_error(
                            &self.token,
                            format!("Struct `{name}` doesn't have a field: `{field}`").as_str(),
                            vec![format!(
                                "Struct `{name}` has these fields: {:?}",
                                members
                                    .iter()
                                    .zip(idents.iter())
                                    .collect::<Vec<(&TypeName, &String)>>()
                            )
                            .as_str()],
                        );
                    }
                }
                Type::U64
                | Type::U8
                | Type::Bool
                | Type::Enum { .. }
                | Type::Pointer { .. }
                | Type::Placeholder { .. } => compiler_error(
                    &self.token,
                    format!("Non-struct type {:?} doesn't have a member {field}", t).as_str(),
                    vec![],
                ),
                Type::GenericStructInstance { .. }
                | Type::GenericStructBase { .. }
                | Type::GenericUnionInstance { .. }
                | Type::GenericUnionBase { .. } => unreachable!(),
            };
            t = new_t;
            t_offset += new_offset;
        }
        (t, t_offset)
    }

    pub fn type_check(
        &mut self,
        stack: &mut Stack,
        frame: &mut Frame,
        fn_table: &FnTable,
        type_map: &mut HashMap<TypeName, Type>,
        gen_map: &HashMap<TypeName, TypeName>,
        locals: &BTreeMap<String, LocalVar>,
        globals: &BTreeMap<String, (TypeName, String)>,
        type_visibility: &Option<TypeName>,
    ) -> Option<Function> {
        let op: Option<(OpKind, Function)> = match &self.kind {
            OpKind::Add => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::U64.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::Sub => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::U64.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::Mul => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::U64.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::Div => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::U64.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::LessThan => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::Bool.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::LessEqual => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::Bool.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::GreaterThan => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::Bool.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::GreaterEqual => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::Bool.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::Equals => {
                if let (Some(a), Some(b)) = (stack.pop(), stack.pop()) {
                    match (type_map.get(&a).unwrap(), type_map.get(&b).unwrap()) {
                        (Type::U64, Type::U64) => {
                            evaluate_signature(
                                self,
                                &Signature {
                                    inputs: vec![],
                                    outputs: vec![Type::Bool.name()],
                                },
                                stack,
                            );
                        }
                        (Type::Enum { name: enum1, .. }, Type::Enum { name: enum2, .. }) => {
                            if enum1 == enum2 {
                                evaluate_signature(
                                    self,
                                    &Signature {
                                        inputs: vec![],
                                        outputs: vec![Type::Bool.name()],
                                    },
                                    stack,
                                );
                            } else {
                                compiler_error(
                                    &self.token,
                                    format!("Cannot compare enums `{enum1}` and `{enum2}").as_str(),
                                    vec![],
                                );
                            }
                        }
                        (_, _) => {
                            stack.push(b);
                            stack.push(a);
                            evaluate_signature(
                                self,
                                &Signature {
                                    inputs: vec![],
                                    outputs: vec![Type::Bool.name()],
                                },
                                stack,
                            );
                        }
                    }
                } else {
                    compiler_error(
                        &self.token,
                        "Insuffient arguments for equals comparison.",
                        vec![
                            format!("Expected: {:#?}", [Type::U64.name(), Type::U64.name()])
                                .as_str(),
                        ],
                    )
                }
                None
            }
            OpKind::NotEquals => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::Bool.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::Mod => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name(), Type::U64.name()],
                        outputs: vec![Type::U64.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::Read(None) => {
                let typ = if let Some(t) = stack.last() {
                    if let Type::Pointer { typ } = type_map.get(t).unwrap() {
                        typ.clone()
                    } else {
                        compiler_error(
                            &self.token,
                            "Read expects a pointer on top of the stack",
                            vec![format!("Found {:?} instead.", stack.last()).as_str()],
                        );
                    }
                } else {
                    compiler_error(
                        &self.token,
                        "Read expects a pointer on top of the stack",
                        vec![format!("Found {:?} instead.", stack.last()).as_str()],
                    );
                };

                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::Pointer { typ: typ.clone() }.name()],
                        outputs: vec![typ.clone()],
                    },
                    stack,
                );

                let n = type_map.get(&typ).unwrap().size(&self.token, type_map);
                let width = type_map.get(&typ).unwrap().width();

                self.kind = OpKind::Read(Some((n, width)));

                None
            }
            OpKind::Read(Some(_)) => {
                panic!("Read width shouldn't have been resolved at this point...")
            }
            OpKind::Write(None) => {
                let typ = if let Some(t) = stack.last() {
                    if let Type::Pointer { typ } = type_map.get(t).unwrap() {
                        typ.clone()
                    } else {
                        compiler_error(
                            &self.token,
                            "Write expects a pointer on top of the stack",
                            vec![format!("Found {:?} instead.", stack.last()).as_str()],
                        );
                    }
                } else {
                    compiler_error(
                        &self.token,
                        "Write expects a pointer on top of the stack",
                        vec![format!("Found {:?} instead.", stack.last()).as_str()],
                    );
                };

                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![typ.clone(), Type::Pointer { typ: typ.clone() }.name()],
                        outputs: vec![],
                    },
                    stack,
                );

                let n = type_map.get(&typ).unwrap().size(&self.token, type_map);
                let width = type_map.get(&typ).unwrap().width();

                self.kind = OpKind::Write(Some((n, width)));
                None
            }
            OpKind::Write(Some(_width)) => {
                panic!("Read width should have been resolved at this point...")
            }
            OpKind::SizeOf(typ) => {
                let t = type_map.get(typ).unwrap();
                let typ_after = if t.is_generic(type_map) {
                    Type::assign_generics(&self.token, typ, gen_map, type_map)
                } else {
                    t.name()
                };

                let size = type_map
                    .get(&typ_after)
                    .unwrap()
                    .size(&self.token, type_map)
                    * type_map.get(&typ_after).unwrap().width();

                self.kind = OpKind::PushInt(size as u64);
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![Type::U64.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::Cast(cast_type) => {
                match type_map.get(cast_type).unwrap().clone() {
                    Type::Struct {
                        name: _, members, ..
                    } => {
                        evaluate_signature(
                            self,
                            &Signature {
                                inputs: members.clone(),
                                outputs: vec![cast_type.clone()],
                            },
                            stack,
                        );
                    }
                    Type::GenericUnionBase { .. } => unimplemented!(
                        "{}: Casting to generic union base isn't implemented yet",
                        self.token.loc
                    ),
                    Type::GenericUnionInstance { .. } => {
                        let new_typ =
                            Type::assign_generics(&self.token, cast_type, gen_map, type_map);
                        if let Some(typ) = stack.pop() {
                            if let Some(Type::ResolvedUnion { members, .. }) =
                                type_map.get(&new_typ)
                            {
                                if members.contains(&typ) {
                                    evaluate_signature(
                                        self,
                                        &Signature {
                                            inputs: vec![], // We've already popped the type
                                            outputs: vec![new_typ.clone()],
                                        },
                                        stack,
                                    );
                                    let size_delta = type_map
                                        .get(&new_typ)
                                        .unwrap()
                                        .size(&self.token, type_map)
                                        - type_map.get(&typ).unwrap().size(&self.token, type_map);
                                    self.kind = OpKind::Pad(size_delta);
                                } else {
                                    compiler_error(
                                        &self.token,
                                        format!("Type {:?} cannot be cast to {:?}", typ, new_typ)
                                            .as_str(),
                                        vec![format!(
                                            "Union {:?} expects one of these: {:?}",
                                            cast_type, members
                                        )
                                        .as_str()],
                                    )
                                }
                            } else {
                                unreachable!();
                            }
                        } else {
                            compiler_error(
                                &self.token,
                                "Casting requires at least one element on the stack.",
                                vec![],
                            )
                        }
                    }
                    Type::GenericStructBase { name, members, .. } => {
                        if members.len() > stack.len() {
                            compiler_error(
                                &self.token,
                                format!(
                                    "Insufficient number of elements on the stack to cast to {name}"
                                )
                                .as_str(),
                                vec![
                                    format!("Expected: {:?}", members).as_str(),
                                    format!("Found:    {:?}", stack).as_str(),
                                ],
                            );
                        }
                        let resolved_struct =
                            Type::resolve_struct(&self.token, &cast_type, stack, type_map);
                        match type_map.get(&resolved_struct).unwrap() {
                            Type::ResolvedStruct {
                                name: _, members, ..
                            } => {
                                evaluate_signature(
                                    self,
                                    &Signature {
                                        inputs: members.clone(),
                                        outputs: vec![resolved_struct.clone()],
                                    },
                                    stack,
                                );
                            }
                            _ => unreachable!(),
                        }
                    }
                    Type::U64 => {
                        let typ = if let Some(t) = stack.last() {
                            match type_map.get(t) {
                                Some(Type::U64) => Type::U64,
                                Some(Type::U8) => Type::U8,
                                Some(Type::Bool) => Type::Bool,
                                Some(Type::Pointer { typ }) => Type::Pointer { typ: typ.clone() },
                                None
                                | Some(Type::Enum { .. })
                                | Some(Type::Union { .. })
                                | Some(Type::GenericUnionBase { .. })
                                | Some(Type::GenericUnionInstance { .. })
                                | Some(Type::ResolvedUnion { .. })
                                | Some(Type::Struct { .. })
                                | Some(Type::GenericStructBase { .. })
                                | Some(Type::GenericStructInstance { .. })
                                | Some(Type::ResolvedStruct { .. })
                                | Some(Type::Placeholder { .. }) => Type::U64,
                            }
                        } else {
                            Type::U64
                        };

                        evaluate_signature(
                            self,
                            &Signature {
                                inputs: vec![typ.name()],
                                outputs: vec![Type::U64.name()],
                            },
                            stack,
                        );
                    }
                    Type::U8 => {
                        let typ = if let Some(t) = stack.last() {
                            match type_map.get(t) {
                                Some(Type::U64) => Type::U64,
                                Some(Type::U8) => Type::U8,
                                Some(Type::Bool) => Type::Bool,
                                None
                                | Some(Type::Pointer { .. })
                                | Some(Type::Enum { .. })
                                | Some(Type::Union { .. })
                                | Some(Type::GenericUnionBase { .. })
                                | Some(Type::GenericUnionInstance { .. })
                                | Some(Type::ResolvedUnion { .. })
                                | Some(Type::Struct { .. })
                                | Some(Type::GenericStructBase { .. })
                                | Some(Type::GenericStructInstance { .. })
                                | Some(Type::ResolvedStruct { .. })
                                | Some(Type::Placeholder { .. }) => Type::U8,
                            }
                        } else {
                            Type::U8
                        };

                        evaluate_signature(
                            self,
                            &Signature {
                                inputs: vec![typ.name()],
                                outputs: vec![Type::U8.name()],
                            },
                            stack,
                        );
                    }
                    Type::Pointer { typ } => {
                        let typ = if type_map.get(&typ).unwrap().clone().is_generic(type_map) {
                            Type::assign_generics(&self.token, &typ, gen_map, type_map)
                        } else {
                            typ
                        };
                        evaluate_signature(
                            self,
                            &Signature {
                                inputs: vec![Type::U64.name()],
                                outputs: vec![Type::Pointer { typ }.name()],
                            },
                            stack,
                        );
                    }
                    Type::Union { members, .. } | Type::ResolvedUnion { members, .. } => {
                        if let Some(typ) = stack.pop() {
                            if members.contains(&typ) {
                                evaluate_signature(
                                    self,
                                    &Signature {
                                        inputs: vec![], // We've already popped the type
                                        outputs: vec![cast_type.clone()],
                                    },
                                    stack,
                                );

                                let size_delta =
                                    type_map.get(cast_type).unwrap().size(&self.token, type_map)
                                        - type_map.get(&typ).unwrap().size(&self.token, type_map);
                                self.kind = OpKind::Pad(size_delta);
                            } else {
                                compiler_error(
                                    &self.token,
                                    format!("Type {:?} cannot be cast to {:?}", typ, cast_type)
                                        .as_str(),
                                    vec![format!(
                                        "Union {:?} expects one of these: {:?}",
                                        cast_type, members
                                    )
                                    .as_str()],
                                )
                            }
                        } else {
                            compiler_error(
                                &self.token,
                                "Casting requires at least one element on the stack.",
                                vec![],
                            )
                        }
                    }
                    Type::Bool => {
                        unimplemented!("{}: Casting to Bool isn't implemented yet.", self.token.loc)
                    }
                    Type::Placeholder { name } => {
                        let concrete_t = gen_map.get(&name).unwrap();
                        self.kind = OpKind::Cast(concrete_t.clone());
                        self.type_check(
                            stack,
                            frame,
                            fn_table,
                            type_map,
                            gen_map,
                            locals,
                            globals,
                            type_visibility,
                        );
                    }
                    Type::GenericStructInstance { .. } => unreachable!(
                        "{}: Casting to instance of generic struct should be unreachable",
                        self.token.loc
                    ),
                    Type::ResolvedStruct { .. } => unreachable!(
                        "{}: Casting to resolved struct type should be unreachable",
                        self.token.loc
                    ),
                    Type::Enum { .. } => {
                        compiler_error(&self.token, "casting to Enums isn't supported.", vec![])
                    }
                }

                None
            }
            OpKind::Pad(_) => unreachable!(
                "{}: {:?} shouldn't be type checked.",
                self.token.loc, self.kind
            ),
            OpKind::PushString { .. } => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![Type::str().name()],
                    },
                    stack,
                );
                None
            }
            OpKind::PushInt(_) => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![Type::U64.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::PushBool(_) => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![Type::Bool.name()],
                    },
                    stack,
                );
                None
            }
            OpKind::PushEnum { typ, idx } => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![typ.clone()],
                    },
                    stack,
                );

                self.kind = OpKind::PushInt(*idx as u64);
                None
            }
            OpKind::MakeIdent { ident, .. } => {
                if let Some(typ) = stack.pop() {
                    self.kind = OpKind::MakeIdent {
                        ident: ident.clone(),
                        size: Some(type_map.get(&typ).unwrap().size(&self.token, type_map)),
                    };
                    frame.push(typ);
                } else {
                    compiler_error(
                        &self.token,
                        "Type Error - Creating a `var` requires at least one element on the stack.",
                        vec![format!("Stack: {:?}", stack).as_str()],
                    )
                }
                None
            }
            OpKind::PushIdent { index, inner } => {
                let (t, offset) =
                    self.get_type_from_frame(frame, *index, inner, type_map, type_visibility);
                let size = type_map.get(&t).unwrap().size(&self.token, type_map);
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![t],
                    },
                    stack,
                );

                self.kind = OpKind::PushFramed {
                    offset: offset as isize,
                    size,
                };

                None
            }
            OpKind::PushFramed { .. } => {
                panic!("OpKind::PushFramed shouldn't be generated before type checking...");
            }
            OpKind::PushLocal(ident) => {
                let (typ, offset) = Function::locals_get_offset(ident, locals);
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![typ],
                    },
                    stack,
                );

                self.kind = OpKind::PushLocalPtr(offset);

                None
            }
            OpKind::PushLocalPtr(_) => {
                panic!("{:?} shouldn't be generated before type checking...", self)
            }
            OpKind::JumpCond(_) => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::Bool.name()],
                        outputs: vec![],
                    },
                    stack,
                );
                None
            }
            OpKind::Jump(_) => None,
            OpKind::JumpDest(_) => None,
            OpKind::StartBlock => None,
            OpKind::EndBlock => None,
            OpKind::DestroyFramed { .. } => {
                let t = frame.last().unwrap();
                match type_map.get(t).unwrap() {
                    Type::ResolvedStruct { base, .. } => {
                        let dtor_name = format!("-{base}");
                        if let Some(f) = fn_table.get(&dtor_name) {
                            let new_f =
                                f.resolve_generic_function(&self.token, &vec![t.clone()], type_map);
                            Some((OpKind::Call(new_f.name.clone(), vec![]), new_f))
                        } else {
                            let new_dtor = type_map
                                .get(t)
                                .unwrap()
                                .generate_empty_destructor(&self.token, type_map);
                            Some((OpKind::Call(new_dtor.name.clone(), vec![]), new_dtor))
                        }
                    }
                    Type::GenericStructBase { .. }
                    | Type::GenericStructInstance { .. }
                    | Type::GenericUnionBase { .. }
                    | Type::GenericUnionInstance { .. } => unreachable!(),
                    Type::Union { .. }
                    | Type::ResolvedUnion { .. }
                    | Type::U64
                    | Type::U8
                    | Type::Pointer { .. }
                    | Type::Bool => {
                        self.kind = OpKind::Nop(Keyword::Function);
                        None
                    }
                    _ => {
                        let dtor_name = format!("-{}", type_map.get(t).unwrap().name());
                        if let Some(_) = fn_table.get(&dtor_name) {
                            self.kind = OpKind::Call(dtor_name, vec![]);
                            None
                        } else {
                            Some((
                                OpKind::Call(dtor_name, vec![]),
                                type_map
                                    .get(t)
                                    .unwrap()
                                    .generate_empty_destructor(&self.token, type_map),
                            ))
                        }
                    }
                }
            }
            OpKind::ReleaseFramed(None) => {
                let t = frame.pop().unwrap();
                self.kind = OpKind::ReleaseFramed(Some(
                    type_map.get(&t).unwrap().size(&self.token, type_map)
                        * type_map.get(&t).unwrap().width(),
                ));
                None
            }
            OpKind::ReleaseFramed(Some(_)) => {
                panic!("Release Framed Intrinsic should be unreachable")
            }
            OpKind::Return => {
                *frame = Vec::<TypeName>::new();
                None
            }
            OpKind::Syscall(n) => {
                if stack.len() < *n as usize + 1 {
                    compiler_error(
                        &self.token,
                        format!(
                            "{:?} Requires {} elements on the stack.",
                            self.kind,
                            *n as usize + 1
                        )
                        .as_str(),
                        vec![format!("Stack: {:?}", stack).as_str()],
                    );
                }

                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64.name()],
                        outputs: vec![],
                    },
                    stack,
                );
                for _ in 0..*n {
                    let t = stack.pop().unwrap();
                    if type_map.get(&t).unwrap().size(&self.token, type_map) != 1 {
                        compiler_error(
                            &self.token,
                            "Only u64's can be used in a syscall",
                            vec![format!("Type: {:?}", t).as_str()],
                        );
                    }
                }

                stack.push(Type::U64.name());

                None
            }
            OpKind::Call(func_name, annotations) => {
                let f = fn_table.get(func_name).unwrap_or_else(|| {
                    panic!("Function names should be recognizable at this point... {func_name}")
                });
                if f.is_generic() {
                    let new_fn = if annotations.is_empty() {
                        f.resolve_generic_function(&self.token, stack, type_map)
                    } else {
                        let mut resolved_annotations: Vec<TypeName> = annotations
                            .iter()
                            .map(|t| Type::assign_generics(&self.token, t, gen_map, type_map))
                            .collect();
                        f.assign_generics(&self.token, &mut resolved_annotations, type_map)
                    };

                    evaluate_signature(self, &new_fn.sig, stack);
                    Some((OpKind::Call(new_fn.name.clone(), vec![]), new_fn))
                } else {
                    evaluate_signature(self, &f.sig, stack);
                    None
                }
            }
            OpKind::Nop(_) => None,
            OpKind::Global(s) => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![globals.get(s).unwrap().0.clone()],
                    },
                    stack,
                );
                None
            }
            OpKind::Word(_) => {
                unreachable!("Shouldn't have any words left to type check: {:?}", self)
            }
            OpKind::Ident(_, _) => unreachable!("Shouldn't have any idents left to type check"),
            OpKind::PrepareFunc => None,
            OpKind::Default => unreachable!("Default op shouldn't be compiled"),
        };

        if let Some((new_op, new_fn)) = op {
            self.kind = new_op;
            return Some(new_fn);
        }

        None
    }
}

impl From<Token> for Op {
    fn from(token: Token) -> Op {
        match &token.kind {
            TokenKind::Literal(Literal::Int(x)) => Op {
                kind: OpKind::PushInt(*x),
                token,
            },
            TokenKind::Literal(Literal::Bool(b)) => Op {
                kind: OpKind::PushBool(*b),
                token,
            },
            TokenKind::Literal(Literal::String(_)) => {
                panic!("Strings have to be made with knowledge of the string list")
            }
            TokenKind::Operator(Operator::Add) => Op {
                kind: OpKind::Add,
                token,
            },
            TokenKind::Operator(Operator::Sub) => Op {
                kind: OpKind::Sub,
                token,
            },
            TokenKind::Operator(Operator::Mul) => Op {
                kind: OpKind::Mul,
                token,
            },
            TokenKind::Operator(Operator::Div) => Op {
                kind: OpKind::Div,
                token,
            },
            TokenKind::Operator(Operator::LessThan) => Op {
                kind: OpKind::LessThan,
                token,
            },
            TokenKind::Operator(Operator::LessEqual) => Op {
                kind: OpKind::LessEqual,
                token,
            },
            TokenKind::Operator(Operator::GreaterThan) => Op {
                kind: OpKind::GreaterThan,
                token,
            },
            TokenKind::Operator(Operator::GreaterEqual) => Op {
                kind: OpKind::GreaterEqual,
                token,
            },
            TokenKind::Operator(Operator::Equals) => Op {
                kind: OpKind::Equals,
                token,
            },
            TokenKind::Operator(Operator::NotEquals) => Op {
                kind: OpKind::NotEquals,
                token,
            },
            TokenKind::Operator(Operator::Mod) => Op {
                kind: OpKind::Mod,
                token,
            },
            TokenKind::Operator(Operator::Read) => Op {
                kind: OpKind::Read(None),
                token,
            },
            TokenKind::Operator(Operator::Write) => Op {
                kind: OpKind::Write(None),
                token,
            },
            TokenKind::Comment(c) => panic!("Cannot convert comment to op: {:?}", c),
            TokenKind::Keyword(kw) => panic!("Keywords cannot be converted into ops: {:?}", kw),
            TokenKind::Marker(m) => compiler_error(
                &token,
                format!("Unexpected Marker: {:?}", m).as_str(),
                vec![],
            ),
            TokenKind::Word(word) => Op {
                kind: OpKind::Word(word.clone()),
                token,
            },
            TokenKind::EndOfFile => panic!("Cannot convert end of file into an op!"),
        }
    }
}
