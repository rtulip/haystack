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

#[derive(Debug, Serialize, Deserialize, Clone)]
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
    Word(String),
    MakeIdent(String),
    PushIdent(usize),
    Call(String),
    PrepareFunc(Function),
    Return,
    Default,
}

impl Default for OpKind {
    fn default() -> Self {
        OpKind::Default
    }
}

#[derive(Debug, Serialize, Deserialize, Clone, Default)]
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
    ) -> Option<Function> {
        let op: Option<(OpKind, Function)> = match &self.kind {
            OpKind::Add => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::u64_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::Sub => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::u64_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::Mul => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::u64_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::Div => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::u64_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::LessThan => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::bool_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::LessEqual => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::bool_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::GreaterThan => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::bool_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::GreaterEqual => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::bool_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::Equals => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::bool_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::NotEquals => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t(), Type::u64_t()],
                        outputs: vec![Type::bool_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::PushIdent(n) => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![frame[*n].clone()],
                    },
                    stack,
                );
                None
            }
            OpKind::PushString(_) => todo!(),
            OpKind::PushInt(_) => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![],
                        outputs: vec![Type::u64_t()],
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
                        outputs: vec![Type::bool_t()],
                    },
                    stack,
                );
                None
            }
            OpKind::MakeIdent(_) => {
                if let Some(typ) = stack.pop() {
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
            OpKind::Print => {
                evaluate_signature(
                    self,
                    &Signature {
                        inputs: vec![Type::u64_t()],
                        outputs: vec![],
                    },
                    stack,
                );
                None
            }
            OpKind::Return => {
                *frame = Vec::<Type>::new();
                None
            }
            OpKind::Call(func_name) => {
                let f = fn_table
                    .get(func_name)
                    .expect("Function names should be recognizable at this point...");

                if f.is_generic() {
                    let new_fn = f.into_concrete(stack);
                    evaluate_signature(self, &new_fn.sig, stack);
                    Some((OpKind::Call(new_fn.name.clone()), new_fn))
                } else {
                    evaluate_signature(self, &f.sig, stack);
                    None
                }
            }
            OpKind::Word(_) => unreachable!("Shouldn't have any words left to type check"),
            OpKind::PrepareFunc(_) => unreachable!("PrepareFunction shouldn't be type checked."),
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
            TokenKind::Keyword(kw) => match kw {
                Keyword::Function => panic!("Cannot convert function keyword into an op"),
                Keyword::Var => todo!("Var keyword isn't implemented yet"),
            },
            TokenKind::Marker(m) => panic!("Cannot convert marker to op: {:?}", m),
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
