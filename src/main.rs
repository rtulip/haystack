mod compiler;
mod ir;
mod lex;

fn main() {
    let input_path = "src/examples/foo.hay";
    let ir_path = "src/ir.json";
    let mut program = lex::hay_into_ir(input_path);
    compiler::assign_words(&mut program);
    compiler::program_to_json(ir_path, &program);
    program.type_check();
    program.normalize_function_names();
    compiler::x86_64::compile_program(&program, "src/output.asm");
}
