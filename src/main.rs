use clap::{Parser, Subcommand};
use clap_stdin::FileOrStdin;
use compilers_project::{
    assembler::assemble_and_get_executable,
    compiler::{assembly_generator, ir_generator, parser, tokenizer, type_checker},
};
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
            match call_compiler(&source_code) {
                Ok(executable) => {
                    fs::write(output, &executable).expect("Failed to write output file");
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
                    handle(stream);
                });
            }
            Err(e) => {
                eprintln!("Connection failed: {}", e);
            }
        }
    }
}

fn handle(mut stream: std::net::TcpStream) {
    use std::io::{Read, Write};

    let mut buffer = Vec::new();
    if let Err(e) = stream.read_to_end(&mut buffer) {
        eprintln!("Failed to read: {}", e);
        return;
    }

    let input = match String::from_utf8(buffer) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("Invalid UTF-8: {}", e);
            return;
        }
    };

    let result_str = process_request(&input);

    if let Err(e) = stream.write_all(result_str.as_bytes()) {
        eprintln!("Failed to write: {}", e);
    }
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

fn process_request(input: &str) -> String {
    let request: Request = match serde_json::from_str(&input) {
        Ok(r) => r,
        Err(e) => {
            return serde_json::to_string(&RequestResult::Error {
                error: e.to_string(),
            })
            .unwrap();
        }
    };

    let result = match request {
        Request::Ping => RequestResult::Empty {},
        Request::Compile { code } => match call_compiler(&code) {
            Ok(executable) => {
                use base64::{Engine as _, engine::general_purpose::STANDARD};
                let encoded = STANDARD.encode(&executable);
                RequestResult::Program { program: encoded }
            }
            Err(e) => RequestResult::Error { error: e },
        },
    };

    serde_json::to_string(&result).unwrap()
}

fn call_compiler(source_code: &str) -> Result<Vec<u8>, String> {
    let tokens = tokenizer::tokenize(source_code)?;
    let mut expression = parser::parse(tokens)?;
    type_checker::typecheck(&mut expression)?;
    let instructions = ir_generator::generate_ir(&mut expression)?;
    let assembly_code = assembly_generator::generate_assembly(instructions)?;

    assemble_and_get_executable(&assembly_code, None)
}
