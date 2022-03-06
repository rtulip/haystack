use crate::ir::{Op, Program};
use std::io::prelude::*;

fn frame_push_rax(file: &mut std::fs::File) {
    writeln!(file, "  sub  qword [frame_end_ptr], 8").unwrap();
    writeln!(file, "  mov  rbx, [frame_end_ptr]").unwrap();
    writeln!(file, "  mov  [rbx], rax").unwrap();
}

fn compile_op(op: &Op, mut file: &mut std::fs::File) {
    writeln!(file, "; -- {:?}", op).unwrap();
    match op {
        Op::PushInt(x) => writeln!(file, "  push {x}").unwrap(),
        Op::PushBool(_b) => todo!(),
        Op::PushString(_s) => todo!(),
        Op::Add => {
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  add  rax, rbx").unwrap();
            writeln!(file, "  push rax").unwrap();
        }
        Op::Sub => {
            writeln!(file, "  pop  rbx").unwrap();
            writeln!(file, "  pop  rax").unwrap();
            writeln!(file, "  sub  rax, rbx").unwrap();
            writeln!(file, "  push rax").unwrap();
        }
        Op::Mul => todo!(),
        Op::Div => todo!(),
        Op::LessThan => todo!(),
        Op::LessEqual => todo!(),
        Op::GreaterThan => todo!(),
        Op::GreaterEqual => todo!(),
        Op::Equals => todo!(),
        Op::NotEquals => todo!(),
        Op::Print => {
            writeln!(file, "  pop  rdi").unwrap();
            writeln!(file, "  call print").unwrap();
        }
        Op::Word(_) => todo!(),
        Op::PushIdent(_idx) => todo!(),
        Op::Call(name) => {
            writeln!(file, "  mov  rax, [frame_start_ptr]").unwrap();
            frame_push_rax(&mut file);
            writeln!(file, "  mov  rax, [frame_end_ptr]").unwrap();
            writeln!(file, "  mov  qword [frame_start_ptr], rax").unwrap();
            writeln!(file, "  call {name}").unwrap();
        }
        Op::PrepareFunc(f) => {
            writeln!(file, "{}:", f.name).unwrap();
            writeln!(file, "  pop  rax").unwrap();
            frame_push_rax(&mut file);
            f.sig.inputs.iter().for_each(|typ| {
                if typ.ident.is_some() {
                    writeln!(file, "  pop  rax").unwrap();
                    frame_push_rax(&mut file);
                }
            })
        }
        Op::Return => {
            writeln!(file, "  mov  rax, [frame_start_ptr]").unwrap();
            writeln!(file, "  push qword [rax-8]").unwrap();
            writeln!(file, "  mov  [frame_end_ptr], rax").unwrap();
            writeln!(file, "  add  qword [frame_end_ptr], 8").unwrap();
            writeln!(file, "  mov  rax, [rax]").unwrap();
            writeln!(file, "  mov  qword [frame_start_ptr], rax").unwrap();
            writeln!(file, "  ret").unwrap();
        }
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

fn nasm_close(mut file: &mut std::fs::File) {
    writeln!(file, "global _start").unwrap();
    writeln!(file, "_start: ").unwrap();
    writeln!(file, "  mov  qword [frame_start_ptr], frame_stack_end").unwrap();
    writeln!(file, "  mov  qword [frame_end_ptr], frame_stack_end").unwrap();
    compile_op(&Op::Call("main".to_string()), &mut file);
    writeln!(file, "exit:").unwrap();
    writeln!(file, "  mov  rax, 60").unwrap();
    writeln!(file, "  mov  rdi, 0").unwrap();
    writeln!(file, "  syscall").unwrap();
    writeln!(file, "segment .bss").unwrap();
    writeln!(file, "  frame_start_ptr: resq 1").unwrap();
    writeln!(file, "  frame_end_ptr: resq 1").unwrap();
    writeln!(file, "  frame_stack: resq 2048").unwrap();
    writeln!(file, "  frame_stack_end:").unwrap();
}

pub fn compile_program<P: AsRef<std::path::Path>>(program: Program, out_path: P) {
    let mut file = std::fs::File::create(out_path).unwrap();
    nasm_prelude(&mut file);
    for func in &program.functions {
        compile_op(&Op::PrepareFunc((*func).clone()), &mut file);
        for op in &func.ops {
            compile_op(op, &mut file);
        }
        compile_op(&Op::Return, &mut file);
    }
    nasm_close(&mut file);
}
