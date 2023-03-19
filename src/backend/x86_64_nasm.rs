pub struct X86_64;
use std::io::Write;

use crate::lex::token::Operator;

use super::{InitData, Instruction, UninitData};

impl super::CodeGen for X86_64 {
    fn encode_name(label: &str) -> String {
        let mut s = String::from("fn_");
        label.chars().for_each(|c| match c {
            '.' => s.push_str("_dot_"),
            '<' => s.push('_'),
            '>' => s.push('_'),
            '+' => s.push_str("_plus_"),
            ' ' => s.push('_'),
            '*' => s.push_str("_star_"),
            '&' => s.push_str("_amp_"),
            '_' => s.push_str("__"),
            ':' => s.push('_'),
            '[' => s.push_str("_lbrace_"),
            ']' => s.push_str("_rbrace_"),
            c => s.push(c),
        });
        s
    }

    fn instruction(
        file: &mut std::fs::File,
        hash: &str,
        instruction: &Instruction,
    ) -> Result<(), std::io::Error> {
        writeln!(file, "  ; -- {instruction:?}")?;
        match instruction {
            Instruction::FrameReserve { bytes } => {
                writeln!(file, "  sub qword [frame_end_ptr], {bytes}")?;
            }
            Instruction::PushFromFrame {
                offset_from_end,
                bytes,
            } => {
                writeln!(file, "  mov rax, [frame_end_ptr]")?;
                writeln!(file, "  add rax, {}", offset_from_end * 8)?;
                for _ in 0..*bytes {
                    writeln!(file, "  mov rbx, [rax]")?;
                    writeln!(file, "  push rbx")?;
                    writeln!(file, "  add rax, 8")?;
                }
            }
            Instruction::PushPtrToFrame {
                offset_from_end, ..
            } => {
                writeln!(file, "  mov rax, [frame_end_ptr]")?;
                writeln!(file, "  add rax, {}", offset_from_end * 8)?;
                writeln!(file, "  push rax")?;
            }
            Instruction::PushToFrame { quad_words } => {
                for _ in 0..*quad_words {
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  sub  qword [frame_end_ptr], 8")?;
                    writeln!(file, "  mov  rbx, [frame_end_ptr]")?;
                    writeln!(file, "  mov  [rbx], rax")?;
                }
            }
            Instruction::FramePtrToFrameReserve {
                offset,
                size,
                width,
            } => {
                writeln!(file, "  mov  rax, [frame_start_ptr]")?;
                if *size > 0 {
                    writeln!(file, "  sub  rax, {}", offset + 16 + ((size - 1) * width))?;
                }

                writeln!(file, "  sub  qword [frame_end_ptr], 8")?;
                writeln!(file, "  mov  rbx, [frame_end_ptr]")?;
                writeln!(file, "  mov  [rbx], rax")?;
            }
            Instruction::StartBlock => (),
            Instruction::EndBlock { bytes_to_free } => {
                writeln!(file, "  add qword [frame_end_ptr], {bytes_to_free}")?;
            }
            Instruction::Operator { op, size: None } => match op {
                Operator::Plus => {
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  add  rax, rbx")?;
                    writeln!(file, "  push rax")?;
                }
                Operator::Minus => {
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  sub  rax, rbx")?;
                    writeln!(file, "  push rax")?;
                }
                Operator::Star => {
                    writeln!(file, "  pop  rcx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  mul  rcx")?;
                    writeln!(file, "  push rax")?;
                }
                Operator::Slash => {
                    writeln!(file, "  mov  rdx, 0")?;
                    writeln!(file, "  pop  rcx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  div  rcx")?;
                    writeln!(file, "  push rax")?;
                }
                Operator::Modulo => {
                    writeln!(file, "  mov  rdx, 0")?;
                    writeln!(file, "  pop  rcx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  div  rcx")?;
                    writeln!(file, "  push rdx")?;
                }
                Operator::GreaterEqual => {
                    writeln!(file, "  mov  rcx, 0")?;
                    writeln!(file, "  mov  rdx, 1")?;
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  cmp  rax, rbx")?;
                    writeln!(file, "  cmovge rcx, rdx")?;
                    writeln!(file, "  push rcx")?;
                }
                Operator::GreaterThan => {
                    writeln!(file, "  mov  rcx, 0")?;
                    writeln!(file, "  mov  rdx, 1")?;
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  cmp  rax, rbx")?;
                    writeln!(file, "  cmovg rcx, rdx")?;
                    writeln!(file, "  push rcx")?;
                }
                Operator::LessThan => {
                    writeln!(file, "  mov  rcx, 0")?;
                    writeln!(file, "  mov  rdx, 1")?;
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  cmp  rax, rbx")?;
                    writeln!(file, "  cmovl rcx, rdx")?;
                    writeln!(file, "  push rcx")?;
                }
                Operator::LessEqual => {
                    writeln!(file, "  mov  rcx, 0")?;
                    writeln!(file, "  mov  rdx, 1")?;
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  cmp  rax, rbx")?;
                    writeln!(file, "  cmovle rcx, rdx")?;
                    writeln!(file, "  push rcx")?;
                }
                Operator::Equal => {
                    writeln!(file, "  mov  rcx, 0")?;
                    writeln!(file, "  mov  rdx, 1")?;
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  cmp  rax, rbx")?;
                    writeln!(file, "  cmove rcx, rdx")?;
                    writeln!(file, "  push rcx")?;
                }
                Operator::BangEqual => {
                    writeln!(file, "  mov  rcx, 0")?;
                    writeln!(file, "  mov  rdx, 1")?;
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  cmp  rax, rbx")?;
                    writeln!(file, "  cmovne rcx, rdx")?;
                    writeln!(file, "  push rcx")?;
                }
                Operator::Ampersand => {
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  and  rax, rbx")?;
                    writeln!(file, "  push rax")?;
                }
                Operator::Pipe => {
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  or   rax, rbx")?;
                    writeln!(file, "  push rax")?;
                }
                Operator::Caret => {
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  xor  rax, rbx")?;
                    writeln!(file, "  push rax")?;
                }
                Operator::ShiftLeft => {
                    writeln!(file, "  pop  rcx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  shl  rax, cl")?;
                    writeln!(file, "  push rax")?;
                }
                Operator::ShiftRight => {
                    writeln!(file, "  pop  rcx")?;
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  shr  rax, cl")?;
                    writeln!(file, "  push rax")?;
                }
                Operator::Unary { .. } => unreachable!(
                    "Unary expressions should have been converted into other instructions",
                ),
                Operator::Read | Operator::Write => unreachable!(),
            },
            Instruction::Operator {
                op,
                size: Some((size, width)),
            } => match op {
                Operator::Read => {
                    if *size == 0 {
                        return Ok(());
                    }

                    let register = match width {
                        1 => "bl",
                        2 => "bx",
                        3 => "ebx",
                        8 => "rbx",
                        w => unreachable!("n: {w}"),
                    };
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  mov  rbx, 0")?;
                    writeln!(file, "  mov  {register}, [rax]")?;
                    writeln!(file, "  push rbx")?;
                    for _ in 1..*size {
                        writeln!(file, "  add  rax, {width}")?;
                        writeln!(file, "  mov  rbx, 0")?;
                        writeln!(file, "  mov  {register}, [rax]")?;
                        writeln!(file, "  push rbx")?;
                    }
                }
                Operator::Write => {
                    if *size == 0 {
                        return Ok(());
                    }

                    let register = match width {
                        1 => "bl",
                        2 => "bx",
                        4 => "ebx",
                        8 => "rbx",
                        _ => unreachable!(),
                    };
                    writeln!(file, "  pop  rax")?;
                    writeln!(file, "  add  rax, {}", (*size - 1) * width)?;
                    writeln!(file, "  pop  rbx")?;
                    writeln!(file, "  mov  [rax], {register}")?;
                    for _ in 1..*size {
                        writeln!(file, "  sub  rax, {width}")?;
                        writeln!(file, "  pop  rbx")?;
                        writeln!(file, "  mov  [rax], {register}")?;
                    }
                }
                _ => unreachable!(),
            },
            Instruction::Jump { dest_id } => writeln!(file, "  jmp {hash}_jmp_dest_{dest_id}")?,
            Instruction::JumpFalse { dest_id } => {
                writeln!(file, "  pop  rax")?;
                writeln!(file, "  test rax, rax")?;
                writeln!(file, "  jz {hash}_jmp_dest_{dest_id}")?;
            }
            Instruction::JumpDest { id } => writeln!(file, "{hash}_jmp_dest_{id}:")?,
            Instruction::PushU64(n) => writeln!(file, "  push {n}")?,
            Instruction::PushGlobal { id } => writeln!(file, "  push {id}")?,
            Instruction::Call(func) => {
                let hash = X86_64::encode_name(func);
                writeln!(file, "  mov rax, [frame_start_ptr]")?;
                writeln!(file, "  sub  qword [frame_end_ptr], 8")?;
                writeln!(file, "  mov  rbx, [frame_end_ptr]")?;
                writeln!(file, "  mov  [rbx], rax")?;
                writeln!(file, "  mov  rax, [frame_end_ptr]")?;
                writeln!(file, "  mov  qword [frame_start_ptr], rax")?;
                writeln!(file, "  call {hash} ; -- {func}")?;
            }
            Instruction::InitLocalVarArr {
                offset_to_var,
                offset_to_data,
                data_size,
                data_width,
            } => {
                writeln!(file, "  mov rax, [frame_start_ptr]")?;
                writeln!(file, "  sub rax, {}", offset_to_var + 16)?;
                writeln!(file, "  mov rbx, [frame_start_ptr]")?;
                writeln!(
                    file,
                    "  sub rbx, {}",
                    offset_to_data + 8 + data_size * data_width
                )?;
                writeln!(file, "  mov [rax], rbx")?;
                writeln!(file, "  sub rax, 8")?;
                writeln!(file, "  mov rbx, {data_size}")?;
                writeln!(file, "  mov [rax], rbx")?;
            }
            Instruction::Syscall(n) => {
                let order = ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"];
                for register in order.iter().take(*n + 1) {
                    writeln!(file, "  pop  {register}").unwrap();
                }
                writeln!(file, "  syscall").unwrap();
                writeln!(file, "  push rax").unwrap();
            }
            Instruction::Return => {
                writeln!(file, "  mov  rax, [frame_start_ptr]")?;
                writeln!(file, "  push qword [rax-8]")?;
                writeln!(file, "  mov  [frame_end_ptr], rax")?;
                writeln!(file, "  add  qword [frame_end_ptr], 8")?;
                writeln!(file, "  mov  rax, [rax]")?;
                writeln!(file, "  mov  qword [frame_start_ptr], rax")?;
                writeln!(file, "  ret")?;
            }
        }
        Ok(())
    }

    fn generate_function(
        file: &mut std::fs::File,
        label: &str,
        instructions: &[super::Instruction],
    ) -> Result<(), std::io::Error> {
        let hash = X86_64::encode_name(label);
        writeln!(file, "; fn {label}")?;
        writeln!(file, "{hash}:")?;
        // Save the return address
        writeln!(file, "  pop rax")?;
        writeln!(file, "  sub  qword [frame_end_ptr], 8")?;
        writeln!(file, "  mov  rbx, [frame_end_ptr]")?;
        writeln!(file, "  mov  [rbx], rax")?;

        for i in instructions {
            X86_64::instruction(file, hash.as_str(), i)?;
        }

        Ok(())
    }

    fn open(file: &mut std::fs::File) -> Result<(), std::io::Error> {
        writeln!(file, "segment .text")
    }

    fn entry(file: &mut std::fs::File) -> Result<(), std::io::Error> {
        writeln!(file, "global _start")?;
        writeln!(file, "_start: ")?;
        writeln!(file, "  mov  qword [frame_start_ptr], frame_stack_end")?;
        writeln!(file, "  mov  qword [frame_end_ptr], frame_stack_end")?;

        let call = Instruction::Call(String::from("main"));
        X86_64::instruction(file, "", &call)?;

        writeln!(file, "exit:")?;
        writeln!(file, "  mov  rax, 60")?;
        writeln!(file, "  mov  rdi, 0")?;
        writeln!(file, "  syscall")?;

        Ok(())
    }

    fn init_data(
        file: &mut std::fs::File,
        data: &std::collections::HashMap<String, InitData>,
    ) -> Result<(), std::io::Error> {
        writeln!(file)?;
        writeln!(file, "segment .data")?;
        for (id, data) in data {
            match data {
                InitData::String(s) => {
                    write!(file, "  {id}: db ")?;
                    if !s.is_empty() {
                        s.as_bytes()
                            .iter()
                            .for_each(|b| write!(file, "{b:#x}, ").unwrap());
                    } else {
                        write!(file, "0x00, ")?;
                    }
                }
                InitData::Arr { size, pointer } => {
                    write!(file, "  {id}: dq {size}, {pointer}").unwrap()
                }
            }
            writeln!(file)?;
        }

        Ok(())
    }

    fn uninit_data(
        file: &mut std::fs::File,
        data: &std::collections::HashMap<String, UninitData>,
    ) -> Result<(), std::io::Error> {
        writeln!(file, "segment .bss")?;
        writeln!(file, "  frame_start_ptr: resq 1")?;
        writeln!(file, "  frame_end_ptr: resq 1")?;
        writeln!(file, "  frame_stack: resq 1048576")?;
        writeln!(file, "  frame_stack_end:")?;

        for (id, data) in data {
            match data {
                UninitData::Region(size) => writeln!(file, "  {id}: resq {size}")?,
            }
        }

        Ok(())
    }
}
