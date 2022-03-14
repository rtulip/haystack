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
    PushString(String),
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
    Cast(String),
    Split,
    Word(String),
    MakeIdent {
        ident: String,
        size: Option<usize>,
    },
    PushIdent {
        index: usize,
        offset: Option<usize>,
        size: Option<usize>,
    },
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
            OpKind::PushString(s) => write!(f, "Push(\"{s}\")"),
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
            OpKind::Print => write!(f, "print"),
            OpKind::Cast(name) => write!(f, "Cast({name})"),
            OpKind::Split => write!(f, "Split"),
            OpKind::Word(s) => write!(f, "Word({s})"),
            OpKind::MakeIdent { ident: s, .. } => write!(f, "MakeIdent({s})"),
            OpKind::PushIdent { index: i, .. } => write!(f, "PushIdent({i})"),
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
            OpKind::Cast(name) => {
                assert!(type_map.contains_key(name));
                let cast_type = type_map.get(name).unwrap();
                assert!(matches!(cast_type, Type::StructType { .. }));
                match cast_type {
                    Type::StructType {
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
                    _ => unreachable!(),
                }

                None
            }
            OpKind::Split => {
                let struct_t = match stack.last() {
                    Some(Type::StructType {
                        name,
                        members,
                        idents,
                    }) => Type::StructType {
                        name: name.clone(),
                        members: members.clone(),
                        idents: idents.clone(),
                    },
                    Some(_) => todo!(),
                    None => todo!(),
                };

                match &struct_t {
                    Type::StructType {
                        name: _, members, ..
                    } => evaluate_signature(
                        self,
                        &Signature {
                            inputs: vec![struct_t.clone()],
                            outputs: members.clone(),
                        },
                        stack,
                    ),
                    _ => unreachable!(),
                }

                None
            }
            OpKind::PushString(_) => todo!(),
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
            OpKind::PushIdent { index: n, .. } => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![frame[*n].clone()],
                    },
                    stack,
                );

                println!("{n}: Frame: {:?}", frame);

                let offset = frame[0..*n].iter().map(|t| t.size()).sum();
                println!("Offset: {offset}");
                self.kind = OpKind::PushIdent {
                    index: *n,
                    offset: Some(offset),
                    size: Some(frame[*n].size()),
                };

                None
            }
            OpKind::Print => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::U64],
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
            OpKind::Call(func_name) => {
                let f = fn_table.get(func_name).unwrap_or_else(|| {
                    panic!("Function names should be recognizable at this point... {func_name}")
                });

                if f.is_generic() {
                    let new_fn = f.make_concrete(stack);
                    evaluate_signature(self, &new_fn.sig, stack);
                    Some((OpKind::Call(new_fn.name.clone()), new_fn))
                } else {
                    evaluate_signature(self, &f.sig, stack);
                    None
                }
            }
            OpKind::Nop(_) => None,
            OpKind::Word(_) => unreachable!("Shouldn't have any words left to type check"),
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
            TokenKind::Literal(Literal::String(s)) => Op {
                kind: OpKind::PushString(s.clone()),
                token,
            },
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
