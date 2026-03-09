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
        let mut seen: HashSet<ir::IRVar> = HashSet::new();
        let mut variables: Vec<ir::IRVar> = Vec::new();
        for instr in instructions {
            for var in instr.get_vars() {
                if !seen.contains(&var) {
                    seen.insert(var.clone());
                    variables.push(var);
                }
            }
        }
        Self::new(variables)
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
    all_intrinsics: HashMap<String, intrinsics::IntrinsicFun>,
}

impl AssemblyGenerator {
    fn new(instructions: &Vec<ir::Instruction>) -> Self {
        Self {
            lines: Vec::new(),
            locals: Locals::from_instructions(instructions),
            all_intrinsics: intrinsics::build_all_instrinsics(),
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
        self.emit(&format!("subq ${}, %rsp", self.locals.stack_used * 8));

        for instr in instructions {
            self.emit(&format!("# {}", instr));
            match &instr.kind {
                ir::InstructionKind::LoadIntConst { value, dest } => {
                    let dest_ref = self.locals.get_ref(dest)?;
                    if i32::MIN as i64 <= *value && *value <= i32::MAX as i64 {
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
                    self.emit(&format!("jne .L{}", then_label.name));
                    self.emit(&format!("jmp .L{}", else_label.name));
                }
                ir::InstructionKind::Call { fun, args, dest } => {
                    let dest_ref = self.locals.get_ref(dest)?;
                    let mut arg_refs = Vec::new();
                    for arg in args {
                        arg_refs.push(self.locals.get_ref(arg)?);
                    }

                    if let Some(intrinsic_fun) = self.all_intrinsics.get(&fun.name) {
                        intrinsic_fun(arg_refs, "%rax".to_string(), &mut self.lines);
                    } else {
                        let arg_registers = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
                        if arg_registers.len() < arg_refs.len() {
                            return Err(format!(
                                "more than {} args in Call is not supported",
                                arg_registers.len()
                            ));
                        }
                        for (arg_ref, arg_register) in arg_refs.iter().zip(arg_registers.iter()) {
                            self.emit(&format!("movq {}, {}", arg_ref, arg_register));
                        }
                        self.emit(&format!("callq {}", fun.name));
                    }

                    self.emit(&format!("movq %rax, {}", dest_ref));
                }
            }
            self.emit("");
        }

        self.emit("movq %rbp, %rsp");
        self.emit("popq %rbp");
        self.emit("ret");

        Ok(self.lines.join("\n") + "\n")
    }
}

pub fn generate_assembly(instructions: Vec<ir::Instruction>) -> Result<String, String> {
    let mut assembly_generator = AssemblyGenerator::new(&instructions);
    assembly_generator.generate(instructions)
}

mod intrinsics {
    use std::collections::HashMap;

    pub type IntrinsicFun = fn(Vec<String>, String, &mut Vec<String>);

    fn _int_comparison(
        arg_refs: Vec<String>,
        result_register: String,
        out: &mut Vec<String>,
        setcc_insn: &str,
    ) {
        out.push(format!("xor %rax, %rax"));
        out.push(format!("movq {}, %rdx", arg_refs[0]));
        out.push(format!("cmpq {}, %rdx", arg_refs[1]));
        out.push(format!("{} %al", setcc_insn));
        if result_register != "%rax" {
            out.push(format!("movq %rax, {}", result_register));
        }
    }

    fn addition(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        if result_register != arg_refs[0] {
            out.push(format!("movq {}, {}", arg_refs[0], result_register));
        }
        out.push(format!("addq {}, {}", arg_refs[1], result_register));
    }

    fn substraction(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        if result_register != arg_refs[0] {
            out.push(format!("movq {}, {}", arg_refs[0], result_register));
        }
        out.push(format!("subq {}, {}", arg_refs[1], result_register));
    }

    fn multiplication(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        if result_register != arg_refs[0] {
            out.push(format!("movq {}, {}", arg_refs[0], result_register));
        }
        out.push(format!("imulq {}, {}", arg_refs[1], result_register));
    }

    fn division(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        out.push(format!("movq {}, %rax", arg_refs[0]));
        out.push(format!("cqto"));
        out.push(format!("idivq {}", arg_refs[1]));
        if result_register != "%rax" {
            out.push(format!("movq %rax, {}", result_register));
        }
    }

    fn modulo(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        out.push(format!("movq {}, %rax", arg_refs[0]));
        out.push(format!("cqto"));
        out.push(format!("idivq {}", arg_refs[1]));
        if result_register != "%rdx" {
            out.push(format!("movq %rdx, {}", result_register));
        }
    }

    fn less_than(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        _int_comparison(arg_refs, result_register, out, "setl");
    }

    fn greater_than(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        _int_comparison(arg_refs, result_register, out, "setg");
    }

    fn equal(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        _int_comparison(arg_refs, result_register, out, "sete");
    }

    fn not_equal(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        _int_comparison(arg_refs, result_register, out, "setne");
    }

    fn less_than_or_equal(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        _int_comparison(arg_refs, result_register, out, "setle");
    }

    fn greater_than_or_equal(
        arg_refs: Vec<String>,
        result_register: String,
        out: &mut Vec<String>,
    ) {
        _int_comparison(arg_refs, result_register, out, "setge");
    }

    fn unary_neg(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        out.push(format!("movq {}, {}", arg_refs[0], result_register));
        out.push(format!("negq {}", result_register));
    }

    fn unary_not(arg_refs: Vec<String>, result_register: String, out: &mut Vec<String>) {
        out.push(format!("movq {}, {}", arg_refs[0], result_register));
        out.push(format!("xorq $1, {}", result_register));
    }

    pub fn build_all_instrinsics() -> HashMap<String, IntrinsicFun> {
        use crate::compiler::ast::Operation::*;
        use crate::compiler::ast::UnaryOperation::*;

        let mut all: HashMap<String, IntrinsicFun> = HashMap::new();

        all.insert(format!("{}", Addition), addition);
        all.insert(format!("{}", Substraction), substraction);
        all.insert(format!("{}", Multiplication), multiplication);
        all.insert(format!("{}", Division), division);
        all.insert(format!("{}", Modulo), modulo);
        all.insert(format!("{}", LessThan), less_than);
        all.insert(format!("{}", GreaterThan), greater_than);
        all.insert(format!("{}", Equal), equal);
        all.insert(format!("{}", NotEqual), not_equal);
        all.insert(format!("{}", LessThanOrEqual), less_than_or_equal);
        all.insert(format!("{}", GreaterThanOrEqual), greater_than_or_equal);

        all.insert(format!("unary_{}", Neg), unary_neg);
        all.insert(format!("unary_{}", Not), unary_not);

        all
    }
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

    fn make_asm(stack_used: u32, body: &str) -> String {
        format!(
            ".extern print_int\n\
             .extern print_bool\n\
             .extern read_int\n\
             .global main\n\
             .type main, @function\n\
             .section .text\n\
             main:\n\
             pushq %rbp\n\
             movq %rsp, %rbp\n\
             subq ${}, %rsp\n\
             {}\n\
             movq %rbp, %rsp\n\
             popq %rbp\n\
             ret",
            stack_used, body
        )
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
            "".to_string(),
        )
    }

    #[test]
    fn test_assembly_generator_call() {
        assert_assembly_eq(
            ga(vec![ilic(42, "a"), icall("my_func", vec!["a"], "res")]).unwrap(),
            make_asm(
                24,
                "# LoadIntCost(42, a)\n\
                 movq $42, -8(%rbp)\n\
                 \n\
                 # Call(my_func, [IRVar { name: \"a\" }], res)\n\
                 movq -8(%rbp), %rdi\n\
                 callq my_func\n",
            ),
        )
    }
}
