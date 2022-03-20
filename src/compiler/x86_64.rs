use crate::ir::{
    data::{InitData, UninitData},
    function::Function,
    op::{Op, OpKind},
    program::Program,
};
use std::collections::HashMap;
use std::io::prelude::*;

fn init_data_to_x86_64(file: &mut std::fs::File, ident: &str, data: &InitData) {
    write!(file, "  {ident}: ").unwrap();
    match data {
        InitData::String(s) => {
            write!(file, "db ").unwrap();
            s.as_bytes()
                .iter()
                .for_each(|b| write!(file, "{:#x}, ", b).unwrap());
        }
        InitData::Arr { size, pointer } => {
            write!(file, "dq {size}, {pointer}").unwrap();
        }
    }

    writeln!(file).unwrap();
}

fn uninit_data_to_x86_64(file: &mut std::fs::File, ident: &str, data: &UninitData) {
    write!(file, "  {ident}: ").unwrap();
    match data {
        UninitData::Marker => (),
        UninitData::Region(size) => {
            write!(file, "resq {size}").unwrap();
        }
    }
    writeln!(file).unwrap();
}

fn frame_push_rax(file: &mut std::fs::File) {
    writeln!(file, "  sub  qword [frame_end_ptr], 8").unwrap();
    writeln!(file, "  mov  rbx, [frame_end_ptr]").unwrap();
    writeln!(file, "  mov  [rbx], rax").unwrap();
}

fn frame_pop_n(file: &mut std::fs::File, n: &usize) {
    writeln!(file, "  add qword [frame_end_ptr], {}", 8 * n).unwrap();
}

fn compile_op(
    op: &Op,
    func: Option<&Function>,
    init_data: &HashMap<String, InitData>,
    file: &mut std::fs::File,
) {
    writeln!(file, "  ; -- {:?}", op).unwrap();
    match &op.kind {
        OpKind::PushInt(x) => writeln!(file, "  push {x}").unwrap(),
        OpKind::PushBool(b) => {
            if *b {
                writeln!(file, "  push 1").unwrap()
            } else {
                writeln!(file, "  push 0").unwrap()
            }
        }
        OpKind::PushString(str_ident) => {
            let str_len = if let InitData::String(s) = init_data.get(str_ident).unwrap() {
                s.len()
            } else {
                panic!("{str_ident} doesn't map to an InitData::String.");
            };
            writeln!(file, "  push {str_len}").unwrap();
            writeln!(file, "  push {str_ident}").unwrap();
        }
        OpKind::Add => {
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  add  rax, rbx").unwrap();
            writeln!(file, "  push rax").unwrap();
        }
        OpKind::Sub => {
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  sub  rax, rbx").unwrap();
            writeln!(file, "  push rax").unwrap();
        }
        OpKind::Mul => {
            writeln!(file, "  pop  rcx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  mul  rcx").unwrap();
            writeln!(file, "  push rax").unwrap();
        }
        OpKind::Div => {
            writeln!(file, "  mov  rdx, 0").unwrap();
            writeln!(file, "  pop  rcx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  div  rcx").unwrap();
            writeln!(file, "  push rax").unwrap();
        }
        OpKind::LessThan => {
            writeln!(file, "  mov  rcx, 0").unwrap();
            writeln!(file, "  mov  rdx, 1").unwrap();
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  cmp  rax, rbx").unwrap();
            writeln!(file, "  cmovl rcx, rdx").unwrap();
            writeln!(file, "  push rcx").unwrap();
        }
        OpKind::LessEqual => {
            writeln!(file, "  mov  rcx, 0").unwrap();
            writeln!(file, "  mov  rdx, 1").unwrap();
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  cmp  rax, rbx").unwrap();
            writeln!(file, "  cmovle rcx, rdx").unwrap();
            writeln!(file, "  push rcx").unwrap();
        }
        OpKind::GreaterThan => {
            writeln!(file, "  mov  rcx, 0").unwrap();
            writeln!(file, "  mov  rdx, 1").unwrap();
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  cmp  rax, rbx").unwrap();
            writeln!(file, "  cmovg rcx, rdx").unwrap();
            writeln!(file, "  push rcx").unwrap();
        }
        OpKind::GreaterEqual => {
            writeln!(file, "  mov  rcx, 0").unwrap();
            writeln!(file, "  mov  rdx, 1").unwrap();
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  cmp  rax, rbx").unwrap();
            writeln!(file, "  cmovge rcx, rdx").unwrap();
            writeln!(file, "  push rcx").unwrap();
        }
        OpKind::Equals => {
            writeln!(file, "  mov  rcx, 0").unwrap();
            writeln!(file, "  mov  rdx, 1").unwrap();
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  cmp  rax, rbx").unwrap();
            writeln!(file, "  cmove rcx, rdx").unwrap();
            writeln!(file, "  push rcx").unwrap();
        }
        OpKind::NotEquals => {
            writeln!(file, "  mov  rcx, 0").unwrap();
            writeln!(file, "  mov  rdx, 1").unwrap();
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  cmp  rax, rbx").unwrap();
            writeln!(file, "  cmovne rcx, rdx").unwrap();
            writeln!(file, "  push rcx").unwrap();
        }
        OpKind::Read(Some((n, width))) => {
            let register = match width {
                1 => "bl",
                2 => "bx",
                3 => "ebx",
                8 => "rbx",
                w => unreachable!("n: {w}"),
            };
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  mov  rbx, 0").unwrap();
            writeln!(file, "  mov  {register}, [rax]").unwrap();
            writeln!(file, "  push rbx").unwrap();
            for _ in 1..*n {
                writeln!(file, "  add  rax, {width}").unwrap();
                writeln!(file, "  mov  rbx, 0").unwrap();
                writeln!(file, "  mov  {register}, [rax]").unwrap();
                writeln!(file, "  push rbx").unwrap();
            }
        }
        OpKind::Read(None) => panic!("Read size should have been resolved at this point..."),
        OpKind::Write(Some((n, width))) => {
            let register = match width {
                1 => "bl",
                2 => "bx",
                4 => "ebx",
                8 => "rbx",
                _ => unreachable!(),
            };
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  add  rax, {}", (n - 1) * width).unwrap();
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  mov  [rax], {register}").unwrap();
            for _ in 1..*n {
                writeln!(file, "  sub  rax, {width}").unwrap();
                writeln!(file, "  pop  rbx").unwrap();
                writeln!(file, "  mov  [rax], {register}").unwrap();
            }
        }
        OpKind::Write(None) => panic!("Write size should have been resolved at this point..."),
        OpKind::Print => {
            writeln!(file, "  pop  rdi").unwrap();
            writeln!(file, "  call print").unwrap();
        }
        OpKind::SizeOf(_) => {
            unreachable!("SizeOf should have been converted into PushInt by code generation.")
        }
        OpKind::Cast(_) => (),
        OpKind::Split => (),
        OpKind::Global(s) => write!(file, "  push {s}").unwrap(),
        OpKind::Word(_) => unreachable!("Words shouldn't be compiled."),
        OpKind::Ident(_, _) => unreachable!("Idents shouldn't be compiled."),
        OpKind::MakeIdent {
            ident: _,
            size: Some(n),
        } => {
            for _ in 0..*n {
                writeln!(file, "  pop  rax").unwrap();
                frame_push_rax(file);
            }
        }
        OpKind::MakeIdent {
            ident: _,
            size: None,
        } => panic!("Size hasn't been provided for MakeIdent"),
        OpKind::PushIdent { .. } => {
            panic!("Push ident should have been transformed into PushFramed")
        }
        OpKind::PushFramed { offset, size } => {
            for delta in 0..*size {
                let x = offset + size - delta;
                writeln!(file, "  mov  rax, [frame_start_ptr]").unwrap();
                writeln!(file, "  mov  rax, [rax - {}]", (1 + x) * 8).unwrap();
                writeln!(file, "  push rax").unwrap();
            }
        }
        OpKind::Jump(Some(n)) => {
            writeln!(file, "  jmp {}_jmp_dest_{}", func.unwrap().name, n).unwrap();
        }
        OpKind::Jump(None) => panic!("Unhandled unconditional jump at {}", op.token),
        OpKind::JumpCond(Some(n)) => {
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  test rax, rax").unwrap();
            writeln!(file, "  jz {}_jmp_dest_{}", func.unwrap().name, n).unwrap();
        }
        OpKind::JumpCond(None) => panic!("Unhandled conditional jump at {}", op.token),
        OpKind::JumpDest(n) => {
            writeln!(file, "{}_jmp_dest_{}:", func.unwrap().name, n).unwrap();
        }
        OpKind::StartBlock => (),
        OpKind::EndBlock(n) => frame_pop_n(file, n),
        OpKind::Syscall(n) => {
            let order = ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"];
            for i in 0..=*n {
                writeln!(file, "  pop  {}", order[i as usize]).unwrap();
            }
            writeln!(file, "  syscall").unwrap();
            writeln!(file, "  push rax").unwrap();
        }
        OpKind::Call(name) => {
            writeln!(file, "  mov  rax, [frame_start_ptr]").unwrap();
            frame_push_rax(file);
            writeln!(file, "  mov  rax, [frame_end_ptr]").unwrap();
            writeln!(file, "  mov  qword [frame_start_ptr], rax").unwrap();
            writeln!(file, "  call {name}").unwrap();
        }
        OpKind::PrepareFunc => {
            writeln!(file, "{}:", func.unwrap().name).unwrap();
            writeln!(file, "  pop  rax").unwrap();
            frame_push_rax(file);
        }
        OpKind::Return => {
            writeln!(file, "  mov  rax, [frame_start_ptr]").unwrap();
            writeln!(file, "  push qword [rax-8]").unwrap();
            writeln!(file, "  mov  [frame_end_ptr], rax").unwrap();
            writeln!(file, "  add  qword [frame_end_ptr], 8").unwrap();
            writeln!(file, "  mov  rax, [rax]").unwrap();
            writeln!(file, "  mov  qword [frame_start_ptr], rax").unwrap();
            writeln!(file, "  ret").unwrap();
        }
        OpKind::Nop(_) => (),
        OpKind::Default => panic!("Unexpected Default Operation"),
    }
}

fn nasm_prelude(file: &mut std::fs::File) {
    writeln!(
        file,
        "segment .text
print:
  mov     r9, -3689348814741910323
  sub     rsp, 40
  mov     BYTE [rsp+31], 10
  lea     rcx, [rsp+30]
.L2:
  mov     rax, rdi
  lea     r8, [rsp+32]
  mul     r9
  mov     rax, rdi
  sub     r8, rcx
  shr     rdx, 3
  lea     rsi, [rdx+rdx*4]
  add     rsi, rsi
  sub     rax, rsi
  add     eax, 48
  mov     BYTE [rcx], al
  mov     rax, rdi
  mov     rdi, rdx
  mov     rdx, rcx
  sub     rcx, 1
  cmp     rax, 9
  ja      .L2
  lea     rax, [rsp+32]
  mov     edi, 1
  sub     rdx, rax
  xor     eax, eax
  lea     rsi, [rsp+32+rdx]
  mov     rdx, r8
  mov     rax, 1
  syscall
  add     rsp, 40
  ret"
    )
    .unwrap();
}

fn nasm_close(
    file: &mut std::fs::File,
    init_data: &HashMap<String, InitData>,
    uninit_data: &HashMap<String, UninitData>,
) {
    writeln!(file, "global _start").unwrap();
    writeln!(file, "_start: ").unwrap();
    writeln!(file, "  mov  qword [frame_start_ptr], frame_stack_end").unwrap();
    writeln!(file, "  mov  qword [frame_end_ptr], frame_stack_end").unwrap();
    compile_op(
        &Op {
            kind: OpKind::Call("main".to_string()),
            ..Default::default()
        },
        None,
        init_data,
        file,
    );
    writeln!(file, "exit:").unwrap();
    writeln!(file, "  mov  rax, 60").unwrap();
    writeln!(file, "  mov  rdi, 0").unwrap();
    writeln!(file, "  syscall").unwrap();
    writeln!(file, "segment .data").unwrap();
    init_data
        .iter()
        .for_each(|(k, v)| init_data_to_x86_64(file, k, v));
    writeln!(file, "segment .bss").unwrap();
    writeln!(file, "  frame_start_ptr: resq 1").unwrap();
    writeln!(file, "  frame_end_ptr: resq 1").unwrap();
    writeln!(file, "  frame_stack: resq 2048").unwrap();
    writeln!(file, "  frame_stack_end:").unwrap();
    uninit_data
        .iter()
        .for_each(|(k, v)| uninit_data_to_x86_64(file, k, v));
}

pub fn compile_program<P: AsRef<std::path::Path>>(program: &Program, out_path: P) {
    let mut file = std::fs::File::create(out_path).unwrap();
    nasm_prelude(&mut file);
    program.functions.iter().for_each(|f| {
        if f.is_generic() {
            return;
        }
        f.ops
            .iter()
            .for_each(|op| compile_op(op, Some(f), &program.init_data, &mut file));
    });
    nasm_close(&mut file, &program.init_data, &program.uninit_data);
}
