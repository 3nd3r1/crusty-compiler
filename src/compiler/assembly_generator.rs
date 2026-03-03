use std::collections::{HashMap, HashSet};

use crate::compiler::ir;

struct Locals {
    var_to_location: HashMap<ir::IRVar, String>,
    stack_used: u32,
}

impl Locals {
    fn new(variables: Vec<ir::IRVar>) -> Self {
        let mut var_to_location: HashMap<ir::IRVar, String> = HashMap::new();
        let mut stack_used: u32 = 0;

        for var in variables {
            stack_used += 1;
            var_to_location.insert(var, format!("-{}(%rbp)", stack_used * 8));
        }

        Self {
            var_to_location,
            stack_used,
        }
    }

    fn from_instructions(instructions: &Vec<ir::Instruction>) -> Self {
        let mut variables: HashSet<ir::IRVar> = HashSet::new();
        for instr in instructions {
            for var in instr.get_vars() {
                if !variables.contains(&var) {
                    variables.insert(var);
                }
            }
        }
        Self::new(variables.into_iter().collect())
    }

    fn get_ref(&self, var: &ir::IRVar) -> Result<String, String> {
        if let Some(location) = self.var_to_location.get(var) {
            Ok(location.clone())
        } else {
            Err(format!("undefined var: {}", var))
        }
    }
}

struct AssemblyGenerator {
    lines: Vec<String>,
    locals: Locals,
}

impl AssemblyGenerator {
    fn new(instructions: &Vec<ir::Instruction>) -> Self {
        Self {
            lines: Vec::new(),
            locals: Locals::from_instructions(instructions),
        }
    }

    fn emit(&mut self, line: &str) {
        self.lines.push(line.to_string());
    }

    fn generate(&mut self, instructions: Vec<ir::Instruction>) -> Result<String, String> {
        self.emit(".extern print_int");
        self.emit(".extern print_bool");
        self.emit(".extern read_int");
        self.emit(".global main");
        self.emit(".type main, @function");
        self.emit(".section .text");
        self.emit("main:");
        self.emit("pushq %rbp");
        self.emit("movq %rsp, %rbp");
        self.emit(&format!("subq ${}, %rsp", self.locals.stack_used*8));

        for instr in instructions {
            self.emit(&format!("# {}", instr));
            match &instr.kind {
                ir::InstructionKind::LoadIntConst { value, dest } => {
                    let dest_ref = self.locals.get_ref(dest)?;
                    if -(2 << 31) <= *value && *value <= 2 << 31 {
                        self.emit(&format!("movq ${}, {}", value, dest_ref));
                    } else {
                        self.emit(&format!("movabsq ${}, %rax", value));
                        self.emit(&format!("movq %rax, {}", dest_ref));
                    }
                }
                ir::InstructionKind::LoadBoolConst { value, dest } => {
                    let dest_ref = self.locals.get_ref(dest)?;
                    self.emit(&format!("movq ${}, {}", *value as i32, dest_ref));
                }
                ir::InstructionKind::Copy { source, dest } => {
                    let source_ref = self.locals.get_ref(source)?;
                    let dest_ref = self.locals.get_ref(dest)?;
                    self.emit(&format!("movq {}, %rax", source_ref));
                    self.emit(&format!("movq %rax, {}", dest_ref));
                }
                ir::InstructionKind::Label { label } => {
                    self.emit("");
                    self.emit(&format!(".L{}:", label.name));
                }
                ir::InstructionKind::Jump { label } => {
                    self.emit(&format!("jmp .L{}", label.name));
                }
                ir::InstructionKind::CondJump {
                    cond,
                    then_label,
                    else_label,
                } => {
                    let cond_ref = self.locals.get_ref(cond)?;
                    self.emit(&format!("cmpq $0, {}", cond_ref));
                    self.emit(&format!("jne {}", then_label.name));
                    self.emit(&format!("jmp {}", else_label.name));
                }
                _ => return Err(format!("unexpected instruction: {}", instr)),
            }
        }

        self.emit("movq %rbp, %rsp");
        self.emit("popq %rbp");
        self.emit("ret");

        Ok(self.lines.join("\n"))
    }
}

pub fn generate_assembly(instructions: Vec<ir::Instruction>) -> Result<String, String> {
    let mut assembly_generator = AssemblyGenerator::new(&instructions);
    assembly_generator.generate(instructions)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::ir_generator::tests::*;

    fn ga(instructions: Vec<ir::Instruction>) -> Result<String, String> {
        generate_assembly(instructions)
    }

    fn assert_assembly_eq(left: String, right: String) {
        assert!(left == right, "expected:\n{}\ngot:\n{}", right, left);
    }

    #[test]
    fn test_assembly_generator_basic() {
        // Manual test
        assert_assembly_eq(
            ga(vec![
                ilbc(true, "x"),
                icopy("x", "x2"),
                icondjump("x2", "then", "else"),
                ilabel("then"),
                ilic(1, "x4"),
                icopy("x4", "x3"),
                ijump("if_end"),
                ilabel("else"),
                ilic(2, "x5"),
                icopy("x5", "x3"),
                ilabel("if_end"),
            ])
            .unwrap(),
            "".to_string()
        )
    }
}
