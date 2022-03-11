use crate::ir::{function::Function, token::Token, Program};
use std::collections::HashMap;
use std::fs;

// pub fn program_from_json<P: AsRef<std::path::Path>>(ir_path: P) -> Program {
//     let json = fs::read_to_string(ir_path).unwrap();
//     serde_json::from_str(&json.to_string()).unwrap()
// }

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
