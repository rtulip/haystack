mod compiler;
mod ir;
mod lex;
use std::io::{self, Write};
use std::process::Command;

fn run_command(cmd: &str, args: Vec<&str>) {
    print!("[CMD]: {cmd}");
    args.iter().for_each(|arg| print!(" {arg}"));
    println!("");
    let nasm_output = Command::new(cmd)
        .args(args)
        .output()
        .expect("Failed to run nasm");
    io::stdout().write_all(&nasm_output.stdout).unwrap();
    io::stderr().write_all(&nasm_output.stderr).unwrap();
    assert!(nasm_output.status.success());
}

fn main() {
    let input_path = "src/examples/loop.hay";
    let ir_path = "src/ir.json";
    println!("Converting {input_path} into IR");
    let mut program = lex::hay_into_ir(input_path);
    println!("Generating concrete functions");
    compiler::assign_words(&mut program);
    compiler::program_to_json(ir_path, &program);
    println!("Type checking...");
    program.type_check();
    println!("Normalizing Function Names");
    program.normalize_function_names();
    compiler::x86_64::compile_program(&program, "src/output.asm");

    run_command("nasm", vec!["-felf64", "src/output.asm"]);
    run_command("ld", vec!["-o", "a.out", "./src/output.o"]);
}
