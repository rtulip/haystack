use crate::compiler::{compiler_error, evaluate_signature};
use crate::ir::{
    function::Function,
    keyword::Keyword,
    literal::Literal,
    operator::Operator,
    token::{Token, TokenKind},
    types::{Signature, Type},
    FnTable, Frame, Stack,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Serialize, Deserialize, Clone, PartialEq)]
pub enum OpKind {
    PushInt(u64),
    PushBool(bool),
    PushString(usize),
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
    Print,
    Read(Option<(usize, usize)>),
    Write(Option<(usize, usize)>),
    Cast(String),
    Split,
    Word(String),
    Ident(String, Vec<String>),
    MakeIdent { ident: String, size: Option<usize> },
    PushFramed { offset: usize, size: usize },
    PushIdent { index: usize, inner: Vec<String> },
    Syscall(u64),
    Call(String),
    PrepareFunc,
    JumpCond(Option<usize>),
    Jump(Option<usize>),
    JumpDest(usize),
    StartBlock,
    EndBlock(usize),
    Return,
    Default,
    Nop(Keyword),
}

impl std::fmt::Debug for OpKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            OpKind::PushInt(i) => write!(f, "Push({i})"),
            OpKind::PushBool(b) => write!(f, "Push({b})"),
            OpKind::PushString(i) => write!(f, "Push(str_{i})"),
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
            OpKind::Read(_) => write!(f, "@"),
            OpKind::Write(_) => write!(f, "!"),
            OpKind::Print => write!(f, "print"),
            OpKind::Cast(name) => write!(f, "Cast({name})"),
            OpKind::Split => write!(f, "Split"),
            OpKind::Word(s) => write!(f, "Word({s})"),
            OpKind::Ident(s, fs) => write!(f, "Ident({s}::{:?}", fs),
            OpKind::MakeIdent { ident: s, .. } => write!(f, "MakeIdent({s})"),
            OpKind::PushIdent { index: i, .. } => write!(f, "PushIdent({i})"),
            OpKind::PushFramed { offset, size } => write!(f, "PushFrame({offset}:{size})"),
            OpKind::Syscall(n) => write!(f, "Syscall({n})"),
            OpKind::Call(func) => write!(f, "Call({func})"),
            OpKind::PrepareFunc => write!(f, "PrepareFunc"),
            OpKind::JumpCond(Some(dest)) => write!(f, "JumpCond({dest})"),
            OpKind::JumpCond(None) => unreachable!(),
            OpKind::Jump(Some(dest)) => write!(f, "Jump({dest})"),
            OpKind::Jump(None) => unreachable!(),
            OpKind::JumpDest(dest) => write!(f, "JumpDest({dest})"),
            OpKind::StartBlock => write!(f, "StartBlock"),
            OpKind::EndBlock(n) => write!(f, "EndBlock({n})"),
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
    fn get_type_from_frame(&self, frame: &Frame, index: usize, inner: &[String]) -> (Type, usize) {
        let mut t = frame[index].clone();
        let mut t_offset: usize = frame[0..index].iter().map(|t| t.size()).sum();

        for field in inner {
            let (new_t, new_offset) = match t {
                Type::Struct {
                    name,
                    ref members,
                    ref idents,
                }
                | Type::ResolvedStruct {
                    name,
                    ref members,
                    ref idents,
                    ..
                } => {
                    if idents.contains(&field.clone()) {
                        let idx = idents.iter().position(|s| s == &field.clone()).unwrap();
                        (
                            members[idx].clone(),
                            members[idx + 1..].iter().map(|t| t.size()).sum::<usize>(),
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
                                    .collect::<Vec<(&Type, &String)>>()
                            )
                            .as_str()],
                        );
                    }
                }
                other_t => compiler_error(
                    &self.token,
                    format!(
                        "Non-struct type {:?} doesn't have a member {field}",
                        other_t
                    )
                    .as_str(),
                    vec![],
                ),
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
        type_map: &HashMap<String, Type>,
    ) -> Option<Function> {
        let op: Option<(OpKind, Function)> = match &self.kind {
            OpKind::Add => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::U64],
                    },
                    stack,
                );
                None
            }
            OpKind::Sub => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::U64],
                    },
                    stack,
                );
                None
            }
            OpKind::Mul => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::U64],
                    },
                    stack,
                );
                None
            }
            OpKind::Div => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::U64],
                    },
                    stack,
                );
                None
            }
            OpKind::LessThan => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::Bool],
                    },
                    stack,
                );
                None
            }
            OpKind::LessEqual => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::Bool],
                    },
                    stack,
                );
                None
            }
            OpKind::GreaterThan => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::Bool],
                    },
                    stack,
                );
                None
            }
            OpKind::GreaterEqual => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::Bool],
                    },
                    stack,
                );
                None
            }
            OpKind::Equals => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::Bool],
                    },
                    stack,
                );
                None
            }
            OpKind::NotEquals => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64, Type::U64],
                        outputs: vec![Type::Bool],
                    },
                    stack,
                );
                None
            }
            OpKind::Read(None) => {
                let (typ, width) = if let Some(Type::Pointer { typ, width }) = stack.last() {
                    (typ.clone(), width.clone())
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
                        inputs: vec![Type::Pointer {
                            typ: typ.clone(),
                            width: width,
                        }],
                        outputs: vec![*typ.clone()],
                    },
                    stack,
                );

                let n = typ.size();
                println!("N: {n}, width: {width}");
                self.kind = OpKind::Read(Some((n, width)));

                None
            }
            OpKind::Read(Some(_)) => {
                panic!("Read width shouldn't have been resolved at this point...")
            }
            OpKind::Write(None) => {
                todo!()
            }
            OpKind::Write(Some(_width)) => {
                panic!("Read width should have been resolved at this point...")
            }
            OpKind::Cast(name) => {
                assert!(type_map.contains_key(name));
                let cast_type = type_map.get(name).unwrap();
                match cast_type {
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
                        let resolved_struct = Type::resolve_struct(&self.token, cast_type, stack);
                        match &resolved_struct {
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
                    _ => unreachable!(),
                }

                None
            }
            OpKind::Split => {
                let (struct_t, members) = match stack.last() {
                    Some(Type::Struct {
                        name: _, members, ..
                    }) => (stack.last().unwrap().clone(), members.clone()),
                    Some(Type::ResolvedStruct {
                        name: _, members, ..
                    }) => (stack.last().unwrap().clone(), members.clone()),
                    _ => compiler_error(
                        &self.token,
                        "Split requires a struct on top of the stack.",
                        vec![format!("Stack: {:?}", stack).as_str()],
                    ),
                };

                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![struct_t],
                        outputs: members,
                    },
                    stack,
                );

                None
            }
            OpKind::PushString { .. } => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![Type::str()],
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
                        outputs: vec![Type::U64],
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
                        outputs: vec![Type::Bool],
                    },
                    stack,
                );
                None
            }
            OpKind::MakeIdent { ident, .. } => {
                if let Some(typ) = stack.pop() {
                    self.kind = OpKind::MakeIdent {
                        ident: ident.clone(),
                        size: Some(typ.size()),
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
                let (t, offset) = self.get_type_from_frame(frame, *index, inner);
                let size = t.size();
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![t],
                    },
                    stack,
                );

                self.kind = OpKind::PushFramed { offset, size };

                None
            }
            OpKind::PushFramed { .. } => {
                panic!("OpKind::PushFramed shouldn't be generated before type checking...");
            }
            OpKind::Print => {
                let typ = if let Some(&Type::U8) = stack.last() {
                    Type::U8
                } else if let Some(&Type::U64) = stack.last() {
                    Type::U64
                } else {
                    Type::U64
                };
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![typ],
                        outputs: vec![],
                    },
                    stack,
                );
                None
            }
            OpKind::JumpCond(_) => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::Bool],
                        outputs: vec![],
                    },
                    stack,
                );
                None
            }
            OpKind::Jump(_) => None,
            OpKind::JumpDest(_) => None,
            OpKind::StartBlock => None,
            OpKind::EndBlock(n) => {
                if *n > frame.len() {
                    panic!(
                        "Logic error - Frame doesn't have enough items! N: {} len: {}",
                        n,
                        frame.len()
                    );
                }
                for _ in 0..*n {
                    frame.pop();
                }

                None
            }
            OpKind::Return => {
                *frame = Vec::<Type>::new();
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
                        inputs: vec![Type::U64],
                        outputs: vec![],
                    },
                    stack,
                );
                for _ in 0..*n {
                    let t = stack.pop().unwrap();
                    if t.size() != 1 {
                        compiler_error(
                            &self.token,
                            "Only u64's can be used in a syscall",
                            vec![format!("Type: {:?}", t).as_str()],
                        );
                    }
                }

                stack.push(Type::U64);

                None
            }
            OpKind::Call(func_name) => {
                let f = fn_table.get(func_name).unwrap_or_else(|| {
                    panic!("Function names should be recognizable at this point... {func_name}")
                });

                if f.is_generic() {
                    let new_fn = f.resolve_generic_function(&self.token, stack);
                    evaluate_signature(self, &new_fn.sig, stack);
                    Some((OpKind::Call(new_fn.name.clone()), new_fn))
                } else {
                    evaluate_signature(self, &f.sig, stack);
                    None
                }
            }
            OpKind::Nop(_) => None,
            OpKind::Word(_) => unreachable!("Shouldn't have any words left to type check"),
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
            TokenKind::Word(word) => match word.as_str() {
                "print" => Op {
                    kind: OpKind::Print,
                    token,
                },
                _ => Op {
                    kind: OpKind::Word(word.clone()),
                    token,
                },
            },
            TokenKind::EndOfFile => panic!("Cannot convert end of file into an op!"),
        }
    }
}
