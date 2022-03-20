use crate::ir::{program::Program, token::Token, types::Type};
use std::fs;
use std::io::Write;

pub fn simplify_ir<P: AsRef<std::path::Path> + std::clone::Clone>(program: &Program, out_path: P) {
    let mut file = std::fs::File::create(out_path).unwrap();
    writeln!(
        &mut file,
        "--------------------- Types ---------------------"
    )
    .unwrap();
    program.types.iter().for_each(|(_, t)| {
        writeln!(&mut file, "{:?}", t).unwrap();
        match t {
            Type::GenericStructBase {
                members, idents, ..
            }
            | Type::Struct {
                members, idents, ..
            }
            | Type::GenericStructInstance {
                members, idents, ..
            }
            | Type::ResolvedStruct {
                members, idents, ..
            } => members
                .iter()
                .zip(idents)
                .for_each(|(t, ident)| writeln!(&mut file, " -- {:?}: {ident}", t).unwrap()),
            _ => (),
        }
    });

    writeln!(
        &mut file,
        "--------------------- Functions ---------------------"
    )
    .unwrap();
    program.functions.iter().for_each(|f| {
        write!(&mut file, "{}(", f.name).unwrap();
        f.sig.inputs.iter().enumerate().for_each(|(i, t)| {
            if i != 0 {
                write!(&mut file, " ").unwrap();
            }
            write!(&mut file, "{:?}", t).unwrap();
        });
        write!(&mut file, ") -> [").unwrap();
        f.sig.outputs.iter().enumerate().for_each(|(i, t)| {
            if i != 0 {
                write!(&mut file, " ").unwrap();
            }
            write!(&mut file, "{:?}", t).unwrap();
        });
        writeln!(&mut file, "]:").unwrap();
        f.ops.iter().enumerate().for_each(|(i, op)| {
            writeln!(&mut file, "    {i}: {:?}", op.kind).unwrap();
        })
    });
    writeln!(
        &mut file,
        "--------------------- Initialized Data ---------------------"
    )
    .unwrap();

    program.init_data.iter().for_each(|(ident, data)| {
        writeln!(&mut file, "{ident}: {:?}", data).unwrap();
    });

    writeln!(
        &mut file,
        "--------------------- Uninitialized Data ---------------------"
    )
    .unwrap();

    program.uninit_data.iter().for_each(|(ident, data)| {
        writeln!(&mut file, "{ident}: {:?}", data).unwrap();
    });
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
