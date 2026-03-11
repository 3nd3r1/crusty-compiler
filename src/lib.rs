use std::path::Path;

pub mod assembler;
pub mod compiler;

pub fn compile(source_code: &str, workdir: Option<&Path>) -> Result<Vec<u8>, String> {
    use crate::compiler::{assembly_generator, ir_generator, parser, tokenizer, type_checker};
    let tokens = tokenizer::tokenize(source_code)?;
    let mut module = parser::parse(tokens)?;
    type_checker::typecheck(&mut module)?;
    let function_irs = ir_generator::generate_ir(&mut module)?;
    let assembly_code = assembly_generator::generate_assembly(function_irs)?;

    assembler::assemble_and_get_executable(&assembly_code, workdir)
}
