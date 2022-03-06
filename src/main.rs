mod ir;
mod lexer;
fn main() {
    lexer::hay_into_ir("src/examples/math.hay", "src/ir.json");
}
