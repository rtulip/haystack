use std::collections::HashMap;

use crate::lex::token::Operator;

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
}
