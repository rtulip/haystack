mod instruction;
mod x86_64_nasm;
pub use instruction::*;
use std::collections::HashMap;
pub use x86_64_nasm::*;

pub trait CodeGen {
    fn encode_name(label: &str) -> String;

    fn instruction(
        file: &mut std::fs::File,
        label_hash: &str,
        instruction: &Instruction,
    ) -> Result<(), std::io::Error>;

    fn open(file: &mut std::fs::File) -> Result<(), std::io::Error>;
    fn generate_function(
        file: &mut std::fs::File,
        label: &str,
        instructions: &[Instruction],
    ) -> Result<(), std::io::Error>;
    fn entry(file: &mut std::fs::File) -> Result<(), std::io::Error>;
    fn init_data(
        file: &mut std::fs::File,
        data: &HashMap<String, InitData>,
    ) -> Result<(), std::io::Error>;
    fn uninit_data(
        file: &mut std::fs::File,
        data: &HashMap<String, UninitData>,
    ) -> Result<(), std::io::Error>;
}

pub fn compile<B: CodeGen>(
    file: &str,
    functions: &Vec<(&str, Vec<Instruction>)>,
    init_data: &HashMap<String, InitData>,
    uninit_data: &HashMap<String, UninitData>,
) -> Result<(), std::io::Error> {
    let mut file = std::fs::File::create(file).unwrap();

    B::open(&mut file)?;
    for (label, instructions) in functions {
        B::generate_function(&mut file, label, instructions)?;
    }
    B::entry(&mut file)?;
    B::init_data(&mut file, init_data)?;
    B::uninit_data(&mut file, uninit_data)?;

    Ok(())
}
