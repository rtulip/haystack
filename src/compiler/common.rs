use crate::ir::{program::Program, token::Token};
use std::fs;
use std::io::Write;

pub fn simplify_ir<P: AsRef<std::path::Path> + std::clone::Clone>(program: &Program, out_path: P) {
    let mut file = std::fs::File::create(out_path).unwrap();
    program.functions.iter().for_each(|f| {
        write!(&mut file, "{}(", f.name).unwrap();
        f.sig
            .inputs
            .iter()
            .for_each(|t| write!(&mut file, "{:?} ", t).unwrap());
        write!(&mut file, ") -> [").unwrap();
        f.sig
            .outputs
            .iter()
            .for_each(|t| write!(&mut file, "{:?} ", t).unwrap());
        writeln!(&mut file, "]:").unwrap();
        f.ops.iter().enumerate().for_each(|(i, op)| {
            writeln!(&mut file, "    {i}: {:?}", op.kind).unwrap();
        })
    })
}

pub fn program_to_json<P: AsRef<std::path::Path>>(ir_path: P, program: &Program) {
    let json = serde_json::to_string_pretty(program).unwrap();
    fs::write(ir_path, json).unwrap();
}

pub fn compiler_error(token: &Token, msg: &str, notes: Vec<&str>) -> ! {
    eprintln!("{}: ERROR: {msg}", token.loc);
    notes.iter().for_each(|note| eprintln!("    Note: {note}"));
    std::process::exit(1);
}
