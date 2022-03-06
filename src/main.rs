mod compiler;
mod ir;
mod lexer;

fn main() {
    let input_path = "src/examples/math.hay";
    let ir_path = "src/ir.json";
    let mut program = lexer::hay_into_ir(input_path);
    compiler::assign_words(&mut program);
    compiler::program_to_json(ir_path, &program);
    compiler::x86_64::compile_program(program, "src/output.asm");
}
