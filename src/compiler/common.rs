use crate::ir::{Program, Token};
use std::collections::HashSet;
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

pub fn program_meta(program: &Program) -> HashSet<String> {
    let mut fn_names: HashSet<String> = HashSet::new();

    program.functions.iter().for_each(|func| {
        if !fn_names.insert(func.name.clone()) {
            compiler_error(
                &func.token.clone(),
                format!("Redefinition of function {}", func.name).as_str(),
                vec![],
            )
        }
    });
    fn_names
}
