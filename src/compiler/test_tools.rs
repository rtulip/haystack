use serde::{Deserialize, Serialize};
#[derive(Serialize, Deserialize, Debug, PartialEq)]
struct OutputSummary {
    exit_code: i32,
    stdout: String,
    stderr: String,
}

#[allow(dead_code)]
pub fn run_test(directory: &str, file_base: &str) -> Result<(), std::io::Error> {
    use crate::compiler::run_command;
    use std::process::Output;

    fn summarize_output(output: &Output) -> OutputSummary {
        let exit_code = output.status.code().unwrap();
        let stdout = String::from_utf8(output.stdout.clone()).unwrap();
        let stderr = String::from_utf8(output.stderr.clone()).unwrap();
        OutputSummary {
            exit_code,
            stdout,
            stderr,
        }
    }
    let file = format!("src/tests/{directory}/{file_base}");

    let output = run_command(
        "cargo",
        vec!["r", "-q", "--", format!("{file}.hay").as_str()],
    );
    let compilation_summary = summarize_output(&output);
    let com_path = format!("{file}.try_com");

    if std::path::Path::new(&com_path).exists() {
        let prev_output: OutputSummary =
            serde_json::from_str(std::fs::read_to_string(&com_path)?.as_str())?;
        assert_eq!(prev_output, compilation_summary);
    } else {
        std::fs::write(
            com_path,
            serde_json::to_string_pretty(&compilation_summary)?,
        )?;
    }

    if output.status.success() {
        let output = summarize_output(&run_command(format!("./{file_base}",).as_str(), vec![]));

        let run_path = format!("{file}.try_run");

        if std::path::Path::new(&run_path).exists() {
            let prev_output: OutputSummary =
                serde_json::from_str(std::fs::read_to_string(&run_path)?.as_str())?;
            assert_eq!(prev_output, output);
        } else {
            std::fs::write(run_path, serde_json::to_string_pretty(&output)?)?;
        }
    }

    Ok(())
}
