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

        let builtin_vars = vec![
            ("None", "$0"),
            ("print_int", "$print_int"),
            ("print_bool", "$print_bool"),
            ("read_int", "$read_int"),
            ("read_bool", "$read_bool"),
        ];

        for (name, location) in builtin_vars {
            var_to_location.insert(
                ir::IRVar {
                    name: name.to_string(),
                },
                location.to_string(),
            );
        }

        for var in variables {
            if !var_to_location.contains_key(&var) {
                stack_used += 1;
                var_to_location.insert(var, format!("-{}(%rbp)", stack_used * 8));
            }
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
    all_intrinsics: HashMap<String, intrinsics::IntrinsicFun>,
}

impl AssemblyGenerator {
    fn new() -> Self {
        Self {
            lines: Vec::new(),
            all_intrinsics: intrinsics::build_all_instrinsics(),
        }
    }

    fn emit(&mut self, line: &str) {
        self.lines.push(line.to_string());
    }

    fn generate(&mut self, function_irs: Vec<ir::FunctionIR>) -> Result<String, String> {
        self.emit(".extern print_int");
        self.emit(".extern print_bool");
        self.emit(".extern read_int");
        self.emit(".section .text");

        for function_ir in function_irs {
            self.generate_function(&function_ir)?;
        }

        Ok(self.lines.join("\n") + "\n")
    }

    fn generate_function(&mut self, function_ir: &ir::FunctionIR) -> Result<(), String> {
        let locals = Locals::from_instructions(&function_ir.instructions);

        self.emit(&format!("# {}()", function_ir.name));
        self.emit(&format!(".global {}", function_ir.name));
        self.emit(&format!(".type {}, @function", function_ir.name));
        self.emit(&format!("{}:", function_ir.name));
        self.emit("pushq %rbp");
        self.emit("movq %rsp, %rbp");

        let mut arg_refs = Vec::new();
        for arg in &function_ir.arguments {
            arg_refs.push(locals.get_ref(arg)?);
        }
        let arg_registers = vec!["%rdi", "%rsi", "%rdx", "%rcx", "%r8", "%r9"];
        if arg_registers.len() < function_ir.arguments.len() {
            return Err(format!(
                "more than {} args is not supported",
                arg_registers.len()
            ));
        }
        for (arg_ref, arg_register) in arg_refs.into_iter().zip(arg_registers.iter()) {
            self.emit(&format!("movq {}, {}", arg_register, arg_ref));
        }

        self.emit(&format!("subq ${}, %rsp", locals.stack_used * 8));
        self.emit("");

        for instr in &function_ir.instructions {
            self.emit(&format!("# {}", instr));
            match &instr.kind {
                ir::InstructionKind::LoadIntConst { value, dest } => {
                    let dest_ref = locals.get_ref(dest)?;
                    if i32::MIN as i128 <= *value && *value <= i32::MAX as i128 {
                        self.emit(&format!("movq ${}, {}", value, dest_ref));
                    } else {
                        self.emit(&format!("movabsq ${}, %rax", value));
                        self.emit(&format!("movq %rax, {}", dest_ref));
                    }
                }
                ir::InstructionKind::LoadBoolConst { value, dest } => {
                    let dest_ref = locals.get_ref(dest)?;
                    self.emit(&format!("movq ${}, {}", *value as i32, dest_ref));
                }
                ir::InstructionKind::Copy { source, dest } => {
                    let source_ref = locals.get_ref(source)?;
                    let dest_ref = locals.get_ref(dest)?;
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
                    let cond_ref = locals.get_ref(cond)?;
                    self.emit(&format!("cmpq $0, {}", cond_ref));
                    self.emit(&format!("jne .L{}", then_label.name));
                    self.emit(&format!("jmp .L{}", else_label.name));
                }
                ir::InstructionKind::Call { fun, args, dest } => {
                    let dest_ref = locals.get_ref(dest)?;
                    let mut arg_refs = Vec::new();
                    for arg in args {
                        arg_refs.push(locals.get_ref(arg)?);
                    }

                    if let Some(intrinsic_fun) = self.all_intrinsics.get(&fun.name) {
                        intrinsic_fun(arg_refs, "%rax".to_string(), &mut self.lines);
                        self.emit(&format!("movq %rax, {}", dest_ref));
                    } else {
                        if locals.stack_used % 2 != 0 {
                            self.emit("subq $8, %rsp");
                        }
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

                        let fun_ref = match locals.get_ref(fun) {
                            Ok(r) if r.starts_with("$") => r[1..].to_string(),
                            Ok(r) => format!("*{}", r),
                            Err(_) => fun.name.clone(),
                        };

                        self.emit(&format!("callq {}", fun_ref));
                        self.emit(&format!("movq %rax, {}", dest_ref));
                        if locals.stack_used % 2 != 0 {
                            self.emit("add $8, %rsp");
                        }
                    }
                }
                ir::InstructionKind::Return { value } => {
                    let value_ref = if let Some(value) = value {
                        locals.get_ref(value)?
                    } else {
                        "$0".to_string()
                    };
                    self.emit(&format!("movq {}, %rax", value_ref));
                    self.emit("movq %rbp, %rsp");
                    self.emit("popq %rbp");
                    self.emit("ret");
                }
            }
            self.emit("");
        }

        Ok(())
    }
}

pub fn generate_assembly(function_irs: Vec<ir::FunctionIR>) -> Result<String, String> {
    AssemblyGenerator::new().generate(function_irs)
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
    use crate::compiler::{ir::IRVar, ir_generator::tests::*};

    fn ga(instructions: Vec<ir::Instruction>) -> Result<String, String> {
        generate_assembly(vec![ir::FunctionIR {
            name: "main".to_string(),
            arguments: Vec::new(),
            instructions,
        }])
    }

    fn assert_assembly_eq(left: String, right: String) {
        pretty_assertions::assert_eq!(left, right);
    }

    fn make_fn(
        name: &str,
        stack_used: u32,
        params_body: Option<&str>,
        body: Option<&str>,
        return_body: Option<&str>,
    ) -> String {
        let params_body = params_body.unwrap_or("");
        let body = body.unwrap_or("");
        let return_body = return_body.unwrap_or(
            "# Return(None)\n\
             movq $0, %rax\n\
             movq %rbp, %rsp\n\
             popq %rbp\n\
             ret\n",
        );

        format!(
            "# {name}()\n\
             .global {name}\n\
             .type {name}, @function\n\
             {name}:\n\
             pushq %rbp\n\
             movq %rsp, %rbp\n\
             {params_body}\
             subq ${stack_used}, %rsp\n\
             \n\
             {body}\
             {return_body}\n\
            "
        )
    }

    fn make_asm(body: &str) -> String {
        format!(
            ".extern print_int\n\
             .extern print_bool\n\
             .extern read_int\n\
             .section .text\n\
             {}",
            body
        )
    }

    fn make_main(stack_used: u32, body: &str) -> String {
        make_asm(&make_fn("main", stack_used, None, Some(body), None))
    }

    #[test]
    fn test_assembly_generator_if() {
        assert_assembly_eq(
            ga(vec![
                ilbc(true, "x"),
                icondjump("x", "then", "else"),
                ilabel("then"),
                ilic(1, "x3"),
                icopy("x3", "x2"),
                ijump("if_end"),
                ilabel("else"),
                ilic(2, "x4"),
                icopy("x4", "x2"),
                ilabel("if_end"),
                iprint_int("x2", "x5"),
                ireturn(None),
            ])
            .unwrap(),
            make_main(
                40,
                "# LoadBoolConst(true, x)\n\
                 movq $1, -8(%rbp)\n\
                 \n\
                 # CondJump(x, then, else)\n\
                 cmpq $0, -8(%rbp)\n\
                 jne .Lthen\n\
                 jmp .Lelse\n\
                 \n\
                 # Label(then)\n\
                 .Lthen:\n\
                 \n\
                 # LoadIntConst(1, x3)\n\
                 movq $1, -16(%rbp)\n\
                 \n\
                 # Copy(x3, x2)\n\
                 movq -16(%rbp), %rax\n\
                 movq %rax, -24(%rbp)\n\
                 \n\
                 # Jump(if_end)\n\
                 jmp .Lif_end\n\
                 \n\
                 # Label(else)\n\
                 .Lelse:\n\
                 \n\
                 # LoadIntConst(2, x4)\n\
                 movq $2, -32(%rbp)\n\
                 \n\
                 # Copy(x4, x2)\n\
                 movq -32(%rbp), %rax\n\
                 movq %rax, -24(%rbp)\n\
                 \n\
                 # Label(if_end)\n\
                 .Lif_end:\n\
                 \n\
                 # Call(print_int, [x2], x5)\n\
                 subq $8, %rsp\n\
                 movq -24(%rbp), %rdi\n\
                 callq print_int\n\
                 movq %rax, -40(%rbp)\n\
                 add $8, %rsp\n\n\
                 ",
            ),
        )
    }

    #[test]
    fn test_assembly_generator_functions() {
        assert_assembly_eq(
            generate_assembly(vec![
                ir::FunctionIR {
                    name: "foo".to_string(),
                    arguments: vec![IRVar {
                        name: "a".to_string(),
                    }],
                    instructions: vec![ireturn(Some("a"))],
                },
                ir::FunctionIR {
                    name: "main".to_string(),
                    arguments: Vec::new(),
                    instructions: vec![
                        ilic(1, "x"),
                        icall("foo", vec!["x"], "x2"),
                        iprint_int("x2", "x3"),
                        ireturn(None),
                    ],
                },
            ])
            .unwrap(),
            make_asm(&format!(
                "{}{}",
                make_fn(
                    "foo",
                    8,
                    Some("movq %rdi, -8(%rbp)\n"),
                    None,
                    Some(
                        "# Return(a)\n\
                     movq -8(%rbp), %rax\n\
                     movq %rbp, %rsp\n\
                     popq %rbp\n\
                     ret\n"
                    )
                ),
                make_fn(
                    "main",
                    24,
                    None,
                    Some(
                        "# LoadIntConst(1, x)\n\
                     movq $1, -8(%rbp)\n\
                     \n\
                     # Call(foo, [x], x2)\n\
                     subq $8, %rsp\n\
                     movq -8(%rbp), %rdi\n\
                     callq foo\n\
                     movq %rax, -16(%rbp)\n\
                     add $8, %rsp\n\
                     \n\
                     # Call(print_int, [x2], x3)\n\
                     subq $8, %rsp\n\
                     movq -16(%rbp), %rdi\n\
                     callq print_int\n\
                     movq %rax, -24(%rbp)\n\
                     add $8, %rsp\n\n\
                     "
                    ),
                    None
                )
            )),
        );
    }

    #[test]
    fn test_assembly_generator_function_type_var() {
        assert_assembly_eq(
            ga(vec![
                icopy("print_int", "x"),
                ilic(4, "x2"),
                icall("x", vec!["x2"], "x3"),
                ireturn(None),
            ])
            .unwrap(),
            make_main(
                24,
                "# Copy(print_int, x)\n\
                 movq $print_int, %rax\n\
                 movq %rax, -8(%rbp)\n\
                 \n\
                 # LoadIntConst(4, x2)\n\
                 movq $4, -16(%rbp)\n\
                 \n\
                 # Call(x, [x2], x3)\n\
                 subq $8, %rsp\n\
                 movq -16(%rbp), %rdi\n\
                 callq *-8(%rbp)\n\
                 movq %rax, -24(%rbp)\n\
                 add $8, %rsp\n\n\
                 ",
            ),
        )
    }

    #[test]
    fn test_assembly_generator_call() {
        assert_assembly_eq(
            ga(vec![
                ilic(42, "a"),
                icall("my_func", vec!["a"], "res"),
                ireturn(None),
            ])
            .unwrap(),
            make_main(
                16,
                "# LoadIntConst(42, a)\n\
                 movq $42, -8(%rbp)\n\
                 \n\
                 # Call(my_func, [a], res)\n\
                 movq -8(%rbp), %rdi\n\
                 callq my_func\n\
                 movq %rax, -16(%rbp)\n\n\
                 ",
            ),
        )
    }
}
