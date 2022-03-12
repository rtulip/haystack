use crate::ir::{function::Function, token::Token, Program};
use std::collections::HashMap;
use std::fs;
use std::io::Write;

pub fn program_from_json<P: AsRef<std::path::Path>>(ir_path: P) -> Program {
    let json = fs::read_to_string(ir_path).unwrap();
    serde_json::from_str(&json.to_string()).unwrap()
}

pub fn simplify_ir<P: AsRef<std::path::Path> + std::clone::Clone>(ir_path: P, out_path: P) {
    let program = program_from_json(ir_path);
    let mut file = std::fs::File::create(out_path.clone()).unwrap();
    program.functions.iter().for_each(|f| {
        write!(&mut file, "{}(", f.name).unwrap();
        f.sig
            .inputs
            .iter()
            .for_each(|t| write!(&mut file, "{} ", t.name).unwrap());
        write!(&mut file, ") -> [").unwrap();
        f.sig
            .outputs
            .iter()
            .for_each(|t| write!(&mut file, "{} ", t.name).unwrap());
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

pub fn program_meta(program: &Program) -> HashMap<String, Function> {
    let mut fn_names: HashMap<String, Function> = HashMap::new();

    program.functions.iter().for_each(|func| {
        fn_names.insert(func.name.clone(), func.clone());
    });
    fn_names
}
