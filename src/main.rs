use clap::{Parser, Subcommand};
use clap_stdin::FileOrStdin;
use compilers_project::compile;
use std::fs;

#[derive(Parser)]
struct Args {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Compile {
        #[arg(default_value = "-")]
        input_file: FileOrStdin,
        #[arg(long)]
        output: String,
    },
    Serve {
        #[arg(long, default_value = "127.0.0.1")]
        host: String,
        #[arg(long, default_value_t = 3000)]
        port: u16,
    },
}

fn main() {
    let args = Args::parse();

    match args.command {
        Commands::Compile { input_file, output } => {
            let source_code = input_file.contents().expect("Failed to read input");
            match compile(&source_code, None) {
                Ok(executable) => {
                    use std::os::unix::fs::PermissionsExt;
                    fs::write(&output, &executable).expect("Failed to write output file");
                    fs::set_permissions(&output, fs::Permissions::from_mode(0o755))
                        .expect("Failed to set permissions");
                }
                Err(e) => {
                    eprintln!("Compilation error: {}", e);
                    std::process::exit(1);
                }
            }
        }
        Commands::Serve { host, port } => {
            run_server(&host, port);
        }
    }
}

fn run_server(host: &str, port: u16) {
    let addr = format!("{}:{}", host, port);
    let listener = std::net::TcpListener::bind(&addr).expect("Failed to bind");
    println!("Starting server at {}", addr);

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                std::thread::spawn(|| {
                    if let Err(e) = handle(stream) {
                        eprintln!("Error handling request: {}", e);
                    }
                });
            }
            Err(e) => {
                eprintln!("Connection failed: {}", e);
            }
        }
    }
}

fn handle(mut stream: std::net::TcpStream) -> Result<(), String> {
    use std::io::{Read, Write};

    let mut buffer = Vec::new();
    stream.read_to_end(&mut buffer).map_err(|e| e.to_string())?;

    let input = String::from_utf8(buffer).map_err(|e| e.to_string())?;

    let result_str = process_request(&input)?;

    stream
        .write_all(result_str.as_bytes())
        .map_err(|e| e.to_string())
}

use serde::{Deserialize, Serialize};

#[derive(Deserialize)]
#[serde(tag = "command", rename_all = "lowercase")]
enum Request {
    Ping,
    Compile { code: String },
}

#[derive(Serialize)]
#[serde(untagged)]
enum RequestResult {
    Empty {},
    Program { program: String },
    Error { error: String },
}

fn process_request(input: &str) -> Result<String, String> {
    let request: Request = serde_json::from_str(&input).map_err(|e| e.to_string())?;

    let tmp_dir = tempfile::TempDir::new().map_err(|e| e.to_string())?;

    let result = match request {
        Request::Ping => RequestResult::Empty {},
        Request::Compile { code } => match compile(&code, Some(tmp_dir.path())) {
            Ok(executable) => {
                use base64::{Engine as _, engine::general_purpose::STANDARD};
                let encoded = STANDARD.encode(&executable);
                RequestResult::Program { program: encoded }
            }
            Err(e) => RequestResult::Error { error: e },
        },
    };

    serde_json::to_string(&result).map_err(|e| e.to_string())
}
