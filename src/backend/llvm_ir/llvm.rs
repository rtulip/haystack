use std::io::Write;

use crate::{
    backend::{CodeGen, InitData, Instruction, UninitData},
    lex::token::Operator,
};

pub struct LLVM {
    var_num: usize,
}

impl LLVM {
    fn bump_var(&mut self) -> usize {
        let x = self.var_num;
        self.var_num += 1;
        x
    }
}

impl Default for LLVM {
    fn default() -> Self {
        LLVM { var_num: 1 }
    }
}

impl CodeGen for LLVM {
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
            c => s.push(c),
        });
        s
    }

    fn entry(&mut self, file: &mut std::fs::File) -> Result<(), std::io::Error> {
        writeln!(file, "; -- todo: entry...")?;
        Ok(())
    }

    fn generate_function(
        &mut self,
        file: &mut std::fs::File,
        label: &str,
        instructions: &[Instruction],
    ) -> Result<(), std::io::Error> {
        let hash = Self::encode_name(label);
        writeln!(file, "; fn {label}")?;
        writeln!(file, "define void @{hash}() {{")?;
        self.var_num = 1;
        for i in instructions {
            self.instruction(file, hash.as_str(), i)?;
        }

        writeln!(file, "}}")?;
        Ok(())
    }

    fn init_data(
        &mut self,
        file: &mut std::fs::File,
        data: &std::collections::HashMap<String, InitData>,
    ) -> Result<(), std::io::Error> {
        writeln!(file, "; -- todo init data")?;
        for (id, data) in data {
            match data {
                InitData::String(s) => {
                    // @.hello   = private unnamed_addr constant [13 x i8] c"Hello World!\00"
                    let bytes = s.as_bytes();
                    write!(
                        file,
                        "  @{id} = private unnamed_addr constand [{} x i8] c\"",
                        bytes.len() + 1,
                    )?;
                    for b in bytes {
                        write!(file, "{b:#x}, ")?;
                    }

                    write!(file, "\\00\"")?;
                }
                InitData::Arr {
                    size: _,
                    pointer: _,
                } => (),
            }
            writeln!(file)?;
        }
        Ok(())
    }

    fn instruction(
        &mut self,
        file: &mut std::fs::File,
        label_hash: &str,
        instruction: &Instruction,
    ) -> Result<(), std::io::Error> {
        writeln!(file, "  ; -- {instruction:?}")?;

        match instruction {
            Instruction::FrameReserve { bytes } => {
                for _ in 0..*bytes {
                    writeln!(
                        file,
                        "  call void @__internal_stack_push_byte(%struct.Stack_t* @frame, i8 0)"
                    )?;
                }
            }
            Instruction::StartBlock => (),
            Instruction::PushToFrame { quad_words } => {
                for _ in 0..*quad_words {
                    let x = self.bump_var();
                    let y = self.bump_var();
                    let z = self.bump_var();
                    writeln!(
                        file,
                        "  %{x} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)",
                    )?;
                    writeln!(file, "  %{y} = bitcast i8* %{x} to i64*")?;
                    writeln!(file, "  %{z} = load i64, i64* %{y}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @frame, i64 %{z})"
                    )?;
                }
            }
            Instruction::PushFromFrame {
                offset_from_end,
                bytes,
            } => {
                let x = self.bump_var();
                writeln!(
                    file,
                    "  %{x} = call i8* @Stack_offset(%struct.Stack_t* @frame, i64 {offset_from_end})"
                )?;
                writeln!(
                    file,
                    "  call void @Stack_push_bytes(%struct.Stack_t* @stack, i8* %{x}, i64 {bytes})"
                )?;
            }
            Instruction::Operator { op, size: None } => match op {
                Operator::GreaterEqual => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result_i1 = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result_i1} = icmp uge i64 %{lhs}, %{rhs}")?;
                    writeln!(file, "  %{result} = zext i1 %{result_i1} to i64")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::GreaterThan => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result_i1 = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result_i1} = icmp ugt i64 %{lhs}, %{rhs}")?;
                    writeln!(file, "  %{result} = zext i1 %{result_i1} to i64")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::LessEqual => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result_i1 = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result_i1} = icmp ule i64 %{lhs}, %{rhs}")?;
                    writeln!(file, "  %{result} = zext i1 %{result_i1} to i64")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::LessThan => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result_i1 = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result_i1} = icmp ult i64 %{lhs}, %{rhs}")?;
                    writeln!(file, "  %{result} = zext i1 %{result_i1} to i64")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Equal => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result_i1 = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result_i1} = icmp eq i64 %{lhs}, %{rhs}")?;
                    writeln!(file, "  %{result} = zext i1 %{result_i1} to i64")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::BangEqual => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result_i1 = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result_i1} = icmp ne i64 %{lhs}, %{rhs}")?;
                    writeln!(file, "  %{result} = zext i1 %{result_i1} to i64")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Plus => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = add i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Minus => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = sub i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Star => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = mul i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Slash => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = udiv i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Modulo => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = urem i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }

                Operator::ShiftLeft => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = shl i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::ShiftRight => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = shr i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Ampersand => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = and i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Pipe => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = or i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Caret => {
                    let rhs_i8_ptr = self.bump_var();
                    let lhs_i8_ptr = self.bump_var();
                    let rhs_i64_ptr = self.bump_var();
                    let lhs_i64_ptr = self.bump_var();
                    let rhs = self.bump_var();
                    let lhs = self.bump_var();
                    let result = self.bump_var();
                    writeln!(
                        file,
                        "  %{rhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(
                        file,
                        "  %{lhs_i8_ptr} = call i8* @Stack_pop(%struct.Stack_t* @stack, i64 8)"
                    )?;
                    writeln!(file, "  %{rhs_i64_ptr} = bitcast i8* %{rhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{lhs_i64_ptr} = bitcast i8* %{lhs_i8_ptr} to i64*")?;
                    writeln!(file, "  %{rhs} = load i64, i64* %{rhs_i64_ptr}")?;
                    writeln!(file, "  %{lhs} = load i64, i64* %{lhs_i64_ptr}")?;
                    writeln!(file, "  %{result} = xor i64 %{lhs}, %{rhs}")?;
                    writeln!(
                        file,
                        "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{result})"
                    )?;
                }
                Operator::Read | Operator::Write | Operator::Unary(_) => unreachable!(),
            },
            Instruction::Operator {
                op,
                size: Some((size, width)),
            } => match op {
                Operator::Read => {
                    let ptr_i8 = self.bump_var();
                    let ptr = self.bump_var();
                    let int_type = 8 * width;
                    writeln!(
                        file,
                        "  %{ptr_i8} = call i8* @Stack_pop_u64(%struct.Stack_t* @stack)"
                    )?;
                    writeln!(
                        file,
                        "  %{ptr} = bitcast i8* %{ptr_i8} to [{size} x i{int_type}]*"
                    )?;
                    for i in 0..*size {
                        let value_ptr = self.bump_var();
                        let value = self.bump_var();
                        writeln!(
                            file,
                            "  %{value_ptr} = getelementptr [{size} x i{int_type}], [{size} x i{int_type}]* %{ptr}, i32 0, i32 {i}"
                        )?;

                        writeln!(
                            file,
                            "  %{value} = load i{int_type}, i{int_type}* %{value_ptr}"
                        )?;
                    }
                }
                Operator::Write => {
                    let ptr_i8 = self.bump_var();
                    let ptr = self.bump_var();
                    let int_type = width * 8;
                    writeln!(
                        file,
                        "  %{ptr_i8} = call i8* @Stack_pop_u64(%struct.Stack_t* @stack)"
                    )?;
                    writeln!(
                        file,
                        "  %{ptr} = bitcast i8* %{ptr_i8} to [{size} x i{int_type}]*",
                    )?;
                    for i in (0..*size).into_iter().rev() {
                        let value_i8 = self.bump_var();
                        let value_i64 = self.bump_var();
                        let value = self.bump_var();
                        let dest_ptr = self.bump_var();
                        writeln!(
                            file,
                            "  %{value_i8} = call i8* @Stack_pop_u64(%struct.Stack_t* @stack)"
                        )?;
                        writeln!(
                            file,
                            "  %{value_i64} = bitcast i8* %{value_i8} to i{int_type}*"
                        )?;
                        writeln!(
                            file,
                            "  %{value} = load i{int_type}, i{int_type}* %{value_i64}"
                        )?;
                        writeln!(
                            file,
                            "  %{dest_ptr} = getelementptr [{size} x i{int_type}], [{size} x i{int_type}]* %{ptr}, i32 0, i32 {i}"
                        )?;
                        writeln!(
                            file,
                            "  store i{int_type} %{value}, i{int_type}* %{dest_ptr}"
                        )?;
                    }
                }
                _ => unreachable!(),
            },
            Instruction::JumpFalse { dest_id } => {
                let cond_p_i8 = self.bump_var();
                let cond_p_i64 = self.bump_var();
                let cond_u64 = self.bump_var();
                let cond = self.bump_var();
                writeln!(
                    file,
                    "  %{cond_p_i8} = call i8* @Stack_pop_u64(%struct.Stack_t* @stack)"
                )?;
                writeln!(file, "  %{cond_p_i64} = bitcast i8* %{cond_p_i8} to i64*")?;
                writeln!(file, "  %{cond_u64} = load i64, i64* %{cond_p_i64}")?;
                writeln!(file, "  %{cond} = trunc i64 %{cond_u64} to i1")?;
                writeln!(file, "  br i1 %{cond}, label %{label_hash}_fallthrough_{dest_id}, label %{label_hash}_jmp_dest_{dest_id}")?;
                writeln!(file, "  {label_hash}_fallthrough_{dest_id}:")?;
            }
            Instruction::PushU64(n) => {
                writeln!(
                    file,
                    "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 {n})"
                )?;
            }
            Instruction::PushGlobal { id } => {
                let x = self.bump_var();
                writeln!(file, "  %{x} = ptrtoint i8* @{id} to i64")?;
                writeln!(
                    file,
                    "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{x})"
                )?;
            }
            Instruction::Call(func) => {
                let hash = Self::encode_name(func);
                writeln!(file, "  call void @{hash}()")?;
            }
            Instruction::EndBlock { bytes_to_free } => {
                let x = self.bump_var();
                writeln!(
                    file,
                    "  %{x} = call i8* @Stack_pop(%struct.Stack_t* @frame, i64 {bytes_to_free})"
                )?;
            }
            Instruction::Jump { dest_id } => {
                self.bump_var();
                writeln!(file, "  br label %{label_hash}_jmp_dest_{dest_id}")?;
            }
            Instruction::JumpDest { id } => {
                // self.bump_var();
                writeln!(file, "  br label %{label_hash}_jmp_dest_{id}")?;
                writeln!(file, "  {label_hash}_jmp_dest_{id}:")?;
            }
            Instruction::Return => {
                writeln!(file, "  ret void")?;
            }
            Instruction::PushPtrToFrame { offset_from_end } => {
                let ptr = self.bump_var();
                let value = self.bump_var();
                writeln!(
                    file,
                    "  %{ptr} = call i8* @Stack_offset(%struct.Stack_t* @frame, i64 {offset_from_end})"
                )?;
                writeln!(file, "  %{value} = ptrtoint i8* %{ptr} to i64")?;
                writeln!(
                    file,
                    "  call void @Stack_push_u64(%struct.Stack_t* @stack, i64 %{value})"
                )?;
            }
            Instruction::InitLocalVarArr {
                offset_to_var: _,
                offset_to_data: _,
                data_size: _,
                data_width: _,
            } => {
                writeln!(file, "unimplemented")?;
            }
            Instruction::FramePtrToFrameReserve {
                offset: _,
                size: _,
                width: _,
            } => {
                writeln!(file, "unimplemented")?;
            }
            Instruction::Syscall(_) => {
                writeln!(file, "this is deprecated...")?;
            }
        }

        Ok(())
    }

    fn open(&mut self, file: &mut std::fs::File) -> Result<(), std::io::Error> {
        let builtin_open = std::fs::read_to_string("src/backend/llvm_ir/llvm_stack.ll")?;
        write!(file, "{builtin_open}")?;
        Ok(())
    }

    fn uninit_data(
        &mut self,
        file: &mut std::fs::File,
        data: &std::collections::HashMap<String, UninitData>,
    ) -> Result<(), std::io::Error> {
        writeln!(file, "; -- todo uninit data")?;
        for _ in data {}
        Ok(())
    }
}
