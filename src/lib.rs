pub mod assembler;
pub mod compiler;

pub fn compile(source_code: &str) -> Result<Vec<u8>, String> {
    use crate::compiler::{assembly_generator, ir_generator, parser, tokenizer, type_checker};
    let tokens = tokenizer::tokenize(source_code)?;
    let mut expression = parser::parse(tokens)?;
    type_checker::typecheck(&mut expression)?;
    let instructions = ir_generator::generate_ir(&mut expression)?;
    let assembly_code = assembly_generator::generate_assembly(instructions)?;

    assembler::assemble_and_get_executable(&assembly_code, None)
}
