use std::collections::HashMap;

use crate::lex::token::{Literal, Operator};

#[derive(Debug, Clone)]
pub enum InitData {
    String(String),
    Arr { size: usize, pointer: String },
}

pub enum UninitData {
    Region(usize),
}

pub type InitDataMap = HashMap<String, InitData>;
pub type UninitDataMap = HashMap<String, UninitData>;

#[derive(Debug, Clone)]
pub enum Instruction {
    Call(String),
    Return,
    PushU64(u64),
    PushGlobal {
        id: String,
    },
    PushFromFrame {
        offset_from_end: usize,
        bytes: usize,
    },
    PushPtrToFrame {
        offset_from_end: usize,
    },
    FramePtrToFrameReserve {
        offset: usize,
        size: usize,
        width: usize,
    },
    PushToFrame {
        quad_words: usize,
    },
    Operator {
        op: Operator,
        // TODO: REMOVE SIZE
        size: Option<(usize, usize)>,
    },
    JumpDest {
        id: usize,
    },
    JumpFalse {
        dest_id: usize,
    },
    Jump {
        dest_id: usize,
    },
    Syscall(usize),
    StartBlock,
    EndBlock {
        bytes_to_free: usize,
    },
    FrameReserve {
        bytes: usize,
    },
    InitLocalVarArr {
        offset_to_var: usize,
        offset_to_data: usize,
        data_size: usize,
        data_width: usize,
    },
}

impl Instruction {
    pub fn escape_string(s: &String) -> String {
        let mut new_s = String::new();
        let mut i = 0;

        while i < s.len() {
            match s.chars().nth(i).unwrap() {
                '\\' => {
                    i += 1;
                    match s.chars().nth(i).unwrap() {
                        'n' => new_s.push('\n'),
                        't' => new_s.push('\t'),
                        'r' => new_s.push('\r'),
                        '0' => new_s.push('\0'),
                        '\\' => new_s.push('\\'),
                        _ => unreachable!(),
                    }
                }
                c => new_s.push(c),
            }

            i += 1;
        }
        new_s
    }

    pub fn from_literal(literal: &Literal, init_data: &mut InitDataMap) -> Vec<Instruction> {
        match literal {
            Literal::Bool(b) => vec![Instruction::PushU64(*b as u64)],
            Literal::Char(c) => vec![Instruction::PushU64(*c as u64)],
            Literal::U64(n) => vec![Instruction::PushU64(*n)],
            Literal::U8(n) => vec![Instruction::PushU64(*n as u64)],
            Literal::String(s) => {
                let n = init_data.len();
                let id = format!("str_{n}");

                assert!(init_data
                    .insert(id.clone(), InitData::String(s.clone()))
                    .is_none());

                vec![
                    Instruction::PushU64(s.len() as u64),
                    Instruction::PushGlobal { id },
                ]
            }
        }
    }

    pub fn count_framed_bytes(instrs: &[Instruction]) -> usize {
        let mut framed_bytes = 0;
        for i in instrs {
            match i {
                Instruction::PushToFrame { quad_words } => framed_bytes += quad_words * 8,
                Instruction::FramePtrToFrameReserve { .. } => framed_bytes += 8,
                Instruction::EndBlock { bytes_to_free } => framed_bytes -= bytes_to_free,
                _ => (),
            }
        }

        framed_bytes
    }
}
