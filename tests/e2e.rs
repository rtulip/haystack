#[cfg(test)]
mod compilation_test {
    use std::{path::Path, process::Command};

    struct Cleanup(std::path::PathBuf);

    impl Drop for Cleanup {
        fn drop(&mut self) {
            if let Err(err) = std::fs::remove_file(&self.0) {
                if err.kind() != std::io::ErrorKind::NotFound {
                    eprintln!("Failed to clean up {}: {err}", self.0.display());
                }
            }
        }
    }

    fn assert_snapshot(path: &Path, actual: &str) {
        let normalized = actual.replace("\r\n", "\n");

        if !path.exists() {
            std::fs::write(path, actual).unwrap();
            println!("Created snapshot: {}", path.display());
            return;
        }

        let expected = std::fs::read_to_string(path).unwrap();
        let expected = expected.replace("\r\n", "\n");

        assert_eq!(
            normalized,
            expected,
            "Snapshot mismatch: {}",
            path.display()
        );
    }

    fn run_test(path: impl AsRef<Path>) {
        let path = path.as_ref();

        let executable = path.with_extension("");
        let _cleanup = Cleanup(executable);

        let output = Command::new(env!("CARGO_BIN_EXE_haystack"))
            .arg(path)
            .arg("--run")
            .output()
            .expect("Failed to execute compiler");

        let stdout = String::from_utf8(output.stdout).unwrap();
        let stderr = String::from_utf8(output.stderr).unwrap();
        let exitcode = output.status.code().unwrap();

        assert_snapshot(&path.with_extension("stdout"), &stdout);
        assert_snapshot(&path.with_extension("stderr"), &stderr);
        assert_snapshot(&path.with_extension("exitcode"), &exitcode.to_string());
    }

    mod functional {
        use super::*;
        #[test]
        fn add() {
            run_test("tests/functional/add.hay");
        }

        #[test]
        fn hello_world() {
            run_test("tests/functional/hello_world.hay");
        }

        #[test]
        fn fib() {
            run_test("tests/functional/fib.hay");
        }

        #[test]
        fn sizeof() {
            run_test("tests/functional/sizeof.hay");
        }

        #[test]
        fn pointer() {
            run_test("tests/functional/pointer.hay");
        }

        #[test]
        fn pointer_subtyping() {
            run_test("tests/functional/pointer_subtyping.hay");
        }
    }

    mod typecheck {
        use super::*;

        #[test]
        fn bad_return() {
            run_test("tests/typecheck/bad_return.hay");
        }

        #[test]
        fn conflicting_impl() {
            run_test("tests/typecheck/conflicting_impl.hay");
        }

        #[test]
        fn redefined_function() {
            run_test("tests/typecheck/redefined_function.hay");
        }

        #[test]
        fn redefined_free_var() {
            run_test("tests/typecheck/redefined_free_var.hay");
        }

        #[test]
        fn bad_metatype() {
            run_test("tests/typecheck/bad_metatype.hay");
        }

        #[test]
        fn bad_metatype2() {
            run_test("tests/typecheck/bad_metatype2.hay");
        }

        #[test]
        fn bad_enum_variant() {
            run_test("tests/typecheck/bad_enum_variant.hay");
        }

        #[test]
        fn bad_struct_member() {
            run_test("tests/typecheck/bad_struct_member.hay");
        }

        #[test]
        fn non_enum_inner_type() {
            run_test("tests/typecheck/non_enum_inner_type.hay");
        }

        #[test]
        fn mutable_pointer() {
            run_test("tests/typecheck/mutable_pointer.hay");
        }
    }

    mod scanning {
        use super::*;

        #[test]
        fn bad_number() {
            run_test("tests/scanning/bad_number.hay");
        }

        #[test]
        fn bad_u8() {
            run_test("tests/scanning/bad_u8.hay");
        }

        #[test]
        fn u8() {
            run_test("tests/scanning/u8.hay");
        }

        #[test]
        fn hex() {
            run_test("tests/scanning/hex.hay");
        }

        #[test]
        fn octal() {
            run_test("tests/scanning/octal.hay");
        }

        #[test]
        fn binary() {
            run_test("tests/scanning/binary.hay");
        }

        #[test]
        fn char() {
            run_test("tests/scanning/char.hay");
        }

        #[test]
        fn unknown_escaped_char() {
            run_test("tests/scanning/unknown_escaped_char.hay");
        }

        #[test]
        fn unterminated_char() {
            run_test("tests/scanning/unterminated_char.hay");
        }

        #[test]
        fn unterminated_string() {
            run_test("tests/scanning/unterminated_string.hay");
        }

        #[test]
        fn unexpected_character() {
            run_test("tests/scanning/unexpected_character.hay");
        }

        #[test]
        fn unknown_equals() {
            run_test("tests/scanning/unknown_equals.hay");
        }
    }

    mod parsing {
        use super::*;
        #[test]
        fn bad_const_pointer() {
            run_test("tests/parsing/bad_const_pointer.hay");
        }

        #[test]
        fn pointer_missing_type() {
            run_test("tests/parsing/pointer_missing_type.hay");
        }

        #[test]
        fn tuple_type() {
            run_test("tests/parsing/tuple_type.hay");
        }

        #[test]
        fn empty_tuple() {
            run_test("tests/parsing/empty_tuple.hay");
        }

        #[test]
        fn bad_function_output() {
            run_test("tests/parsing/bad_function_output.hay");
        }

        #[test]
        fn unmatched_block() {
            run_test("tests/parsing/unmatched_block.hay");
        }

        #[test]
        fn missing_argument_name() {
            run_test("tests/parsing/missing_argument_name.hay");
        }

        #[test]
        fn function_pointer() {
            run_test("tests/parsing/function_pointer.hay");
        }

        #[test]
        fn type_expr_whitespace() {
            run_test("tests/parsing/type_expr_whitespace.hay");
        }

        #[test]
        fn type_expr_not_a_type() {
            run_test("tests/parsing/type_expr_not_a_type.hay");
        }

        #[test]
        fn unclosed_tuple_expr() {
            run_test("tests/parsing/unclosed_tuple_expr.hay");
        }

        #[test]
        fn bad_include() {
            run_test("tests/parsing/bad_include.hay");
        }

        #[test]
        fn generic_extern() {
            run_test("tests/parsing/generic_extern.hay");
        }

        #[test]
        fn pointer_syntax() {
            run_test("tests/parsing/pointer_syntax.hay");
        }
    }
}
