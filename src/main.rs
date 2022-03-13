mod compiler;
mod ir;
mod lex;
use std::io::{self, Write};
use std::process::Command;

use clap::Parser;

#[derive(Parser)]
struct Cli {
    file: String,
    #[clap(short, long)]
    run: bool,
}

fn run_command(cmd: &str, args: Vec<&str>) {
    print!("[CMD]: {cmd}");
    args.iter().for_each(|arg| print!(" {arg}"));
    println!();
    let nasm_output = Command::new(cmd)
        .args(args)
        .output()
        .expect("Failed to run nasm");
    io::stdout().write_all(&nasm_output.stdout).unwrap();
    io::stderr().write_all(&nasm_output.stderr).unwrap();
    assert!(nasm_output.status.success());
}

fn main() {
    let cli = Cli::parse();
    let input_path = cli.file;
    let path = std::path::Path::new(&input_path);
    let ir_path = path.with_extension("json");
    let mut program = lex::hay_into_ir(&input_path);
    program.check_for_entry_point();
    program.check_for_name_conflicts();
    program.assign_words();
    compiler::program_to_json(&ir_path, &program);
    compiler::simplify_ir(&ir_path, &path.with_extension("simple"));
    program.type_check();
    program.normalize_function_names();
    compiler::x86_64::compile_program(&program, &path.with_extension("asm").to_str().unwrap());

    run_command(
        "nasm",
        vec!["-felf64", &path.with_extension("asm").to_str().unwrap()],
    );
    run_command(
        "ld",
        vec![
            "-o",
            &path.file_stem().unwrap().to_str().unwrap(),
            &path.with_extension("o").to_str().unwrap(),
        ],
    );
    if cli.run {
        run_command(
            format!("./{}", &path.file_stem().unwrap().to_str().unwrap()).as_str(),
            vec![],
        );
    }
}
