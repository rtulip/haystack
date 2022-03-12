mod common;
mod type_check;
pub mod x86_64;
pub use common::{compiler_error, program_to_json, simplify_ir};
pub use type_check::{evaluate_signature, type_check_ops_list};
