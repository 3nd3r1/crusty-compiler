use std::{
    fs,
    io::Write,
    os::unix::fs::PermissionsExt,
    path::Path,
    process::{Command, Stdio},
};

use libtest_mimic::{Arguments, Failed, Trial};

fn main() {
    let args = Arguments::from_args();
    let tests = collect_tests();
    libtest_mimic::run(&args, tests).exit();
}

fn collect_tests() -> Vec<Trial> {
    let programs_dir = Path::new("tests/programs");
    let mut tests = Vec::new();

    let mut entries: Vec<fs::DirEntry> = fs::read_dir(programs_dir)
        .expect("tests/programs directory not found")
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().map_or(false, |ext| ext == "tl"))
        .collect();
    entries.sort_by_key(|e| e.path());

    for entry in entries {
        let path = entry.path();
        let file_stem = path.file_stem().unwrap().to_string_lossy().to_string();
        let content = fs::read_to_string(&path)
            .unwrap_or_else(|_| panic!("Failed to read {}", path.display()));

        for (i, block) in content.split("\n---\n").enumerate() {
            let case = parse_test_case(block);
            let name = format!("{}_{}", file_stem, i + 1);
            tests.push(Trial::test(name, move || run_test_case(case)));
        }
    }

    tests
}

struct TestCase {
    source: String,
    inputs: Vec<String>,
    expected_outputs: Vec<String>,
}

fn parse_test_case(block: &str) -> TestCase {
    let mut source_lines = Vec::new();
    let mut inputs = Vec::new();
    let mut expected_outputs = Vec::new();

    for line in block.lines() {
        if let Some(rest) = line.strip_prefix("input ") {
            inputs.push(rest.to_string());
        } else if let Some(rest) = line.strip_prefix("prints ") {
            expected_outputs.push(rest.to_string());
        } else {
            source_lines.push(line);
        }
    }

    TestCase {
        source: source_lines.join("\n"),
        inputs,
        expected_outputs,
    }
}

fn run_test_case(case: TestCase) -> Result<(), Failed> {
    let tmp_dir = tempfile::TempDir::new().map_err(|e| e.to_string())?;
    let path = tmp_dir.path().join("a.out");

    let executable = crusty_compiler::compile(&case.source, Some(tmp_dir.path()))
        .map_err(|e| format!("Compilation failed: {}", e))?;

    fs::write(&path, &executable).map_err(|e| e.to_string())?;
    fs::set_permissions(&path, fs::Permissions::from_mode(0o755)).map_err(|e| e.to_string())?;

    let stdin_input = case.inputs.join("\n");
    let mut child = Command::new(&path)
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .map_err(|e| format!("Failed to run executable: {}", e))?;

    if let Some(mut stdin) = child.stdin.take() {
        stdin.write_all(stdin_input.as_bytes()).ok();
    }

    let output = child.wait_with_output().map_err(|e| e.to_string())?;
    let stdout = String::from_utf8_lossy(&output.stdout);
    let actual: Vec<&str> = stdout.lines().collect();
    let expected: Vec<&str> = case.expected_outputs.iter().map(|s| s.as_str()).collect();

    if actual != expected {
        return Err(format!(
            "output mismatch\nexpected: {:?}\ngot: {:?}",
            expected, actual
        )
        .into());
    }

    Ok(())
}
