use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::compiler::{ast, common::SymTab, ir, types::Type};

type IrSymTab = SymTab<ir::IRVar>;

struct IrGenerator {
    instructions: Vec<ir::Instruction>,
    symtab: Rc<RefCell<IrSymTab>>,
    var_counter: u32,
    label_counter: u32,
    active_loop_labels: Option<(ir::Label, ir::Label)>,
}

impl IrGenerator {
    fn new(root_symtab: IrSymTab) -> Self {
        IrGenerator {
            instructions: Vec::new(),
            symtab: Rc::new(RefCell::new(root_symtab)),
            var_counter: 0,
            label_counter: 0,
            active_loop_labels: None,
        }
    }

    fn emit(&mut self, ins: ir::Instruction) {
        self.instructions.push(ins);
    }

    fn new_var(&mut self) -> ir::IRVar {
        self.var_counter += 1;
        if self.var_counter == 1 {
            if !self.symtab.borrow().contains("x") {
                ir::IRVar {
                    name: "x".to_string(),
                }
            } else {
                self.new_var()
            }
        } else {
            while self
                .symtab
                .borrow()
                .contains(&format!("x{}", self.var_counter))
            {
                self.var_counter += 1;
            }
            ir::IRVar {
                name: format!("x{}", self.var_counter),
            }
        }
    }

    fn new_label(&mut self) -> ir::Label {
        self.label_counter += 1;
        if self.label_counter == 1 {
            ir::Label {
                name: "label".to_string(),
            }
        } else {
            ir::Label {
                name: format!("label{}", self.label_counter),
            }
        }
    }

    fn unit_var(&self) -> ir::IRVar {
        ir::IRVar {
            name: "None".to_string(),
        }
    }

    fn generate(&mut self, module: &mut ast::Module) -> Result<Vec<ir::FunctionIR>, String> {
        let mut function_irs = Vec::new();
        if let Some((main, functions)) = module.functions.split_last_mut() {
            for function in functions.iter() {
                self.symtab.borrow_mut().declare(
                    &function.name.clone(),
                    ir::IRVar {
                        name: function.name.clone(),
                    },
                )?;
            }
            for function in functions {
                function_irs.push(self.generate_function(function)?);
            }

            // Generate main
            let old_symtab = Rc::clone(&self.symtab);
            self.symtab = Rc::new(RefCell::new(IrSymTab {
                locals: HashMap::new(),
                parent: Some(Rc::clone(&self.symtab)),
            }));

            self.instructions = Vec::new();
            self.var_counter = 0;
            let var_main_result = self.visit(&mut main.body)?;
            if let Some(print_fn) = match &main.body.return_type {
                Some(Type::Int) => Some("print_int"),
                Some(Type::Bool) => Some("print_bool"),
                _ => None,
            } {
                let var_print_fn = self.symtab.borrow().lookup(print_fn)?;
                let var_result = self.new_var();
                self.emit(ir::Instruction::call(
                    var_print_fn,
                    vec![var_main_result],
                    var_result,
                    main.body.loc.clone(),
                ));
            }
            self.emit(ir::Instruction::ret(None, main.body.loc.clone()));
            self.symtab = old_symtab;

            function_irs.push(ir::FunctionIR {
                name: main.name.clone(),
                arguments: Vec::new(),
                instructions: std::mem::take(&mut self.instructions),
            });
        } else {
            return Err("module must contain at least one function".to_string());
        }
        Ok(function_irs)
    }

    fn generate_function(
        &mut self,
        function: &mut ast::FunctionDeclaration,
    ) -> Result<ir::FunctionIR, String> {
        self.var_counter = 0;
        let old_symtab = Rc::clone(&self.symtab);
        self.symtab = Rc::new(RefCell::new(IrSymTab {
            locals: HashMap::new(),
            parent: Some(Rc::clone(&self.symtab)),
        }));

        for (p_name, _) in function.params.iter() {
            self.symtab.borrow_mut().declare(
                p_name,
                ir::IRVar {
                    name: p_name.clone(),
                },
            )?;
        }

        self.instructions = Vec::new();
        self.visit(&mut function.body)?;

        if function.return_type == Type::Unit {
            self.emit(ir::Instruction::ret(None, function.body.loc.clone()));
        }

        self.symtab = old_symtab;
        Ok(ir::FunctionIR {
            name: function.name.clone(),
            arguments: function
                .params
                .iter()
                .map(|(p_name, _)| ir::IRVar {
                    name: p_name.clone(),
                })
                .collect(),
            instructions: std::mem::take(&mut self.instructions),
        })
    }

    fn visit(&mut self, node: &mut ast::Expression) -> Result<ir::IRVar, String> {
        match &mut node.kind {
            ast::ExpressionKind::NoneLiteral { .. } => Ok(self.unit_var()),
            ast::ExpressionKind::IntLiteral { value } => {
                let var = self.new_var();
                self.emit(ir::Instruction::load_int_const(
                    value.clone(),
                    var.clone(),
                    node.loc.clone(),
                ));
                Ok(var)
            }
            ast::ExpressionKind::BoolLiteral { value } => {
                let var = self.new_var();
                self.emit(ir::Instruction::load_bool_const(
                    value.clone(),
                    var.clone(),
                    node.loc.clone(),
                ));
                Ok(var)
            }
            ast::ExpressionKind::Identifier { value } => self.symtab.borrow().lookup(&value),
            ast::ExpressionKind::BinaryOp { left, right, op } => {
                let var_left = self.visit(left)?;
                match &op {
                    ast::Operation::Or | ast::Operation::And => {
                        let l_right = self.new_label();
                        let l_skip = self.new_label();
                        let l_end = self.new_label();
                        if matches!(op, ast::Operation::Or) {
                            self.emit(ir::Instruction::cond_jump(
                                var_left.clone(),
                                l_skip.clone(),
                                l_right.clone(),
                                node.loc.clone(),
                            ));
                        } else {
                            self.emit(ir::Instruction::cond_jump(
                                var_left.clone(),
                                l_right.clone(),
                                l_skip.clone(),
                                node.loc.clone(),
                            ));
                        }

                        self.emit(ir::Instruction::label(l_right, node.loc.clone()));
                        let var_right = self.visit(right)?;
                        let var_result = self.new_var();
                        self.emit(ir::Instruction::copy(
                            var_right,
                            var_result.clone(),
                            node.loc.clone(),
                        ));
                        self.emit(ir::Instruction::jump(l_end.clone(), node.loc.clone()));

                        self.emit(ir::Instruction::label(l_skip, node.loc.clone()));
                        self.emit(ir::Instruction::load_bool_const(
                            matches!(op, ast::Operation::Or),
                            var_result.clone(),
                            node.loc.clone(),
                        ));
                        self.emit(ir::Instruction::jump(l_end.clone(), node.loc.clone()));

                        self.emit(ir::Instruction::label(l_end, node.loc.clone()));

                        Ok(var_result)
                    }
                    _ => {
                        let var_fun = self.symtab.borrow().lookup(&op.to_string())?;
                        let var_right = self.visit(right)?;
                        let var_result = self.new_var();
                        self.emit(ir::Instruction::call(
                            var_fun,
                            vec![var_left, var_right],
                            var_result.clone(),
                            node.loc.clone(),
                        ));
                        Ok(var_result)
                    }
                }
            }
            ast::ExpressionKind::UnaryOp { operand, op } => {
                let var_op = self.symtab.borrow().lookup(&format!("unary_{}", op))?;
                let var_operand = self.visit(operand)?;
                let var_result = self.new_var();
                self.emit(ir::Instruction::call(
                    var_op,
                    vec![var_operand],
                    var_result.clone(),
                    node.loc.clone(),
                ));
                Ok(var_result)
            }
            ast::ExpressionKind::If {
                condition,
                then_expression,
                else_expression,
            } => {
                if let Some(else_expression) = else_expression {
                    let l_then = self.new_label();
                    let l_else = self.new_label();
                    let l_end = self.new_label();
                    let var_cond = self.visit(&mut *condition)?;
                    let var_result = self.new_var();

                    self.emit(ir::Instruction::cond_jump(
                        var_cond,
                        l_then.clone(),
                        l_else.clone(),
                        node.loc.clone(),
                    ));

                    self.emit(ir::Instruction::label(l_then, node.loc.clone()));

                    let var_then_result = self.visit(&mut *then_expression)?;
                    self.emit(ir::Instruction::copy(
                        var_then_result,
                        var_result.clone(),
                        node.loc.clone(),
                    ));

                    self.emit(ir::Instruction::jump(l_end.clone(), node.loc.clone()));

                    self.emit(ir::Instruction::label(l_else, node.loc.clone()));

                    let var_else_result = self.visit(&mut *else_expression)?;
                    self.emit(ir::Instruction::copy(
                        var_else_result,
                        var_result.clone(),
                        node.loc.clone(),
                    ));

                    self.emit(ir::Instruction::label(l_end, node.loc.clone()));

                    Ok(var_result)
                } else {
                    let l_then = self.new_label();
                    let l_end = self.new_label();
                    let var_cond = self.visit(&mut *condition)?;

                    self.emit(ir::Instruction::cond_jump(
                        var_cond,
                        l_then.clone(),
                        l_end.clone(),
                        node.loc.clone(),
                    ));

                    self.emit(ir::Instruction::label(l_then, node.loc.clone()));

                    self.visit(&mut *then_expression)?;

                    self.emit(ir::Instruction::label(l_end, node.loc.clone()));

                    Ok(self.unit_var())
                }
            }
            ast::ExpressionKind::VarDeclaration { name, value, .. } => {
                let var_value = self.visit(&mut *value)?;
                let var_var = self.new_var();

                self.symtab.borrow_mut().declare(name, var_var.clone())?;
                self.emit(ir::Instruction::copy(
                    var_value,
                    var_var.clone(),
                    node.loc.clone(),
                ));

                Ok(var_var)
            }
            ast::ExpressionKind::Assignment { name, right } => {
                let var_right = self.visit(&mut *right)?;
                let var_var = self.symtab.borrow().lookup(&name)?;
                self.emit(ir::Instruction::copy(
                    var_right,
                    var_var.clone(),
                    node.loc.clone(),
                ));

                Ok(var_var)
            }
            ast::ExpressionKind::Block { expressions } => {
                let old_symtab = Rc::clone(&self.symtab);
                self.symtab = Rc::new(RefCell::new(IrSymTab {
                    locals: HashMap::new(),
                    parent: Some(Rc::clone(&self.symtab)),
                }));

                let last_var = if let Some((last, expressions)) = expressions.split_last_mut() {
                    for expression in expressions {
                        self.visit(expression)?;
                    }
                    self.visit(last)?
                } else {
                    self.unit_var()
                };

                self.symtab = old_symtab;
                Ok(last_var)
            }
            ast::ExpressionKind::While {
                condition,
                do_expression,
            } => {
                let l_start = self.new_label();
                let l_do = self.new_label();
                let l_end = self.new_label();

                self.emit(ir::Instruction::label(l_start.clone(), node.loc.clone()));

                let var_cond = self.visit(&mut *condition)?;

                self.emit(ir::Instruction::cond_jump(
                    var_cond,
                    l_do.clone(),
                    l_end.clone(),
                    node.loc.clone(),
                ));
                self.emit(ir::Instruction::label(l_do, node.loc.clone()));

                let old_active_loop_labels = self.active_loop_labels.clone();
                self.active_loop_labels = Some((l_start.clone(), l_end.clone()));
                self.visit(&mut *do_expression)?;
                self.active_loop_labels = old_active_loop_labels;

                self.emit(ir::Instruction::jump(l_start.clone(), node.loc.clone()));

                self.emit(ir::Instruction::label(l_end, node.loc.clone()));

                Ok(self.unit_var())
            }
            ast::ExpressionKind::FunctionCall { name, arguments } => {
                let var_fun = self.symtab.borrow().lookup(&name)?;

                let mut var_args: Vec<ir::IRVar> = Vec::new();
                for argument in arguments {
                    var_args.push(self.visit(argument)?);
                }

                let var_dest = self.new_var();
                self.emit(ir::Instruction::call(
                    var_fun,
                    var_args,
                    var_dest.clone(),
                    node.loc.clone(),
                ));

                Ok(var_dest)
            }
            ast::ExpressionKind::Return { value } => {
                let var_result = self.visit(&mut *value)?;
                self.emit(ir::Instruction::ret(
                    Some(var_result.clone()),
                    node.loc.clone(),
                ));
                Ok(var_result)
            }
            ast::ExpressionKind::Break {} => {
                if let Some((_, l_end)) = &self.active_loop_labels {
                    self.emit(ir::Instruction::jump(l_end.clone(), node.loc.clone()));
                    Ok(self.unit_var())
                } else {
                    Err(format!(
                        "{}: break can only be inside a loop",
                        node.loc.clone()
                    ))
                }
            }
            ast::ExpressionKind::Continue {} => {
                if let Some((l_start, _)) = &self.active_loop_labels {
                    self.emit(ir::Instruction::jump(l_start.clone(), node.loc.clone()));
                    Ok(self.unit_var())
                } else {
                    Err(format!(
                        "{}: continue can only be inside a loop",
                        node.loc.clone()
                    ))
                }
            }
        }
    }
}

pub fn generate_ir(module: &mut ast::Module) -> Result<Vec<ir::FunctionIR>, String> {
    use ast::Operation::*;
    use ast::UnaryOperation::*;

    let reserved_names = [
        format!("{}", Addition),
        format!("{}", Substraction),
        format!("{}", Multiplication),
        format!("{}", Division),
        format!("{}", Modulo),
        format!("{}", Equal),
        format!("{}", NotEqual),
        format!("{}", LessThan),
        format!("{}", GreaterThan),
        format!("{}", LessThanOrEqual),
        format!("{}", GreaterThanOrEqual),
        format!("{}", Or),
        format!("{}", And),
        format!("unary_{}", Neg),
        format!("unary_{}", Not),
        "print_int".to_string(),
        "print_bool".to_string(),
        "read_int".to_string(),
        "read_bool".to_string(),
    ];

    let mut root_symtab = IrSymTab {
        locals: HashMap::new(),
        parent: None,
    };
    for name in reserved_names {
        root_symtab.declare(&name, ir::IRVar { name: name.clone() })?;
    }

    IrGenerator::new(root_symtab).generate(module)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::compiler::parser::tests::*;
    use crate::compiler::tokenizer::tests::loc;
    use pretty_assertions::assert_eq;

    fn gi(
        mut node: ast::Expression,
        return_type: Option<Type>,
    ) -> Result<Vec<ir::Instruction>, String> {
        node.return_type = return_type.or_else(|| Some(Type::Unit));
        let mut module = emodule(vec![efunction("main", vec![], node, None)]);
        generate_ir(&mut module).map(|functions| functions.into_iter().last().unwrap().instructions)
    }

    fn assert_ir_eq(left: Vec<ir::Instruction>, right: Vec<ir::Instruction>) {
        assert_eq!(
            left.iter()
                .map(|i| format!("{}", i))
                .collect::<Vec<_>>()
                .join("\n"),
            right
                .iter()
                .map(|i| format!("{}", i))
                .collect::<Vec<_>>()
                .join("\n")
        )
    }

    pub fn ilic(value: i128, dest: &str) -> ir::Instruction {
        ir::Instruction::load_int_const(
            value,
            ir::IRVar {
                name: dest.to_string(),
            },
            loc(),
        )
    }

    pub fn ilbc(value: bool, dest: &str) -> ir::Instruction {
        ir::Instruction::load_bool_const(
            value,
            ir::IRVar {
                name: dest.to_string(),
            },
            loc(),
        )
    }

    pub fn imul(left: &str, right: &str, dest: &str) -> ir::Instruction {
        icall(
            &ast::Operation::Multiplication.to_string(),
            vec![left, right],
            dest,
        )
    }

    pub fn iadd(left: &str, right: &str, dest: &str) -> ir::Instruction {
        icall(
            &ast::Operation::Addition.to_string(),
            vec![left, right],
            dest,
        )
    }

    pub fn iprint_int(var: &str, dest: &str) -> ir::Instruction {
        icall("print_int", vec![var], dest)
    }

    pub fn icall(fun: &str, args: Vec<&str>, dest: &str) -> ir::Instruction {
        ir::Instruction::call(
            ir::IRVar {
                name: fun.to_string(),
            },
            args.iter()
                .map(|a| ir::IRVar {
                    name: a.to_string(),
                })
                .collect(),
            ir::IRVar {
                name: dest.to_string(),
            },
            loc(),
        )
    }

    pub fn icopy(source: &str, dest: &str) -> ir::Instruction {
        ir::Instruction::copy(
            ir::IRVar {
                name: source.to_string(),
            },
            ir::IRVar {
                name: dest.to_string(),
            },
            loc(),
        )
    }

    pub fn icondjump(cond: &str, then_label: &str, else_label: &str) -> ir::Instruction {
        ir::Instruction::cond_jump(
            ir::IRVar {
                name: cond.to_string(),
            },
            ir::Label {
                name: then_label.to_string(),
            },
            ir::Label {
                name: else_label.to_string(),
            },
            loc(),
        )
    }

    pub fn ijump(label: &str) -> ir::Instruction {
        ir::Instruction::jump(
            ir::Label {
                name: label.to_string(),
            },
            loc(),
        )
    }

    pub fn ilabel(label: &str) -> ir::Instruction {
        ir::Instruction::label(
            ir::Label {
                name: label.to_string(),
            },
            loc(),
        )
    }

    pub fn ireturn(value: Option<&str>) -> ir::Instruction {
        if let Some(value) = value {
            ir::Instruction::ret(
                Some(ir::IRVar {
                    name: value.to_string(),
                }),
                loc(),
            )
        } else {
            ir::Instruction::ret(None, loc())
        }
    }

    #[test]
    fn test_ir_generator_math() {
        assert_ir_eq(
            gi(eadd(eint(1), emul(eint(2), eint(3))), Some(Type::Int)).unwrap(),
            vec![
                ilic(1, "x"),
                ilic(2, "x2"),
                ilic(3, "x3"),
                imul("x2", "x3", "x4"),
                iadd("x", "x4", "x5"),
                iprint_int("x5", "x6"),
                ireturn(None),
            ],
        );
    }

    #[test]
    fn test_ir_generator_if() {
        assert_ir_eq(
            gi(eif(ebool(true), eint(0), Some(eint(1))), Some(Type::Int)).unwrap(),
            vec![
                ilbc(true, "x"),
                icondjump("x", "label", "label2"),
                ilabel("label"),
                ilic(0, "x3"),
                icopy("x3", "x2"),
                ijump("label3"),
                ilabel("label2"),
                ilic(1, "x4"),
                icopy("x4", "x2"),
                ilabel("label3"),
                iprint_int("x2", "x5"),
                ireturn(None),
            ],
        );
    }

    #[test]
    fn test_ir_generator_var() {
        assert_ir_eq(
            gi(
                eblock(vec![evar("a", eint(1), None), eassign("a", eint(2))]),
                Some(Type::Int),
            )
            .unwrap(),
            vec![
                ilic(1, "x"),
                icopy("x", "x2"),
                ilic(2, "x3"),
                icopy("x3", "x2"),
                iprint_int("x2", "x4"),
                ireturn(None),
            ],
        );
    }

    #[test]
    fn test_ir_generator_functions() {
        let mut module = emodule(vec![
            efunction(
                "foo",
                vec![("a", Type::Int)],
                ereturn(eide("a")),
                Some(Type::Int),
            ),
            efunction(
                "main",
                vec![],
                ewithtype(eblock(vec![ecall("foo", vec![eint(1)])]), Type::Int),
                None,
            ),
        ]);

        let function_irs = generate_ir(&mut module).unwrap();

        assert_eq!(function_irs.len(), 2);
        assert_eq!(function_irs[0].name, "foo");
        assert_eq!(function_irs[1].name, "main");

        assert_ir_eq(
            function_irs[0].instructions.clone(),
            vec![ireturn(Some("a"))],
        );
        assert_ir_eq(
            function_irs[1].instructions.clone(),
            vec![
                ilic(1, "x"),
                icall("foo", vec!["x"], "x2"),
                iprint_int("x2", "x3"),
                ireturn(None),
            ],
        );
    }

    #[test]
    fn test_ir_generator_function_type_var() {
        assert_ir_eq(
            gi(
                eblock(vec![
                    evar("x", eide("print_int"), None),
                    ecall("x", vec![eint(4)]),
                ]),
                None,
            )
            .unwrap(),
            vec![
                icopy("print_int", "x"),
                ilic(4, "x2"),
                icall("x", vec!["x2"], "x3"),
                ireturn(None),
            ],
        );
    }

    #[test]
    fn test_ir_generator_f_plus_one() {
        let mut module = emodule(vec![
            efunction(
                "f",
                vec![("x", Type::Int)],
                eblock(vec![ereturn(eadd(eide("x"), eint(1)))]),
                Some(Type::Int),
            ),
            efunction(
                "main",
                vec![],
                ewithtype(ecall("f", vec![eint(3)]), Type::Int),
                None,
            ),
        ]);

        let function_irs = generate_ir(&mut module).unwrap();

        assert_eq!(function_irs.len(), 2);
        assert_eq!(function_irs[0].name, "f");
        assert_eq!(function_irs[1].name, "main");
        assert_eq!(
            function_irs[0].arguments,
            vec![ir::IRVar {
                name: "x".to_string()
            }]
        );

        assert_ir_eq(
            function_irs[0].instructions.clone(),
            vec![
                ilic(1, "x2"),
                icall("+", vec!["x", "x2"], "x3"),
                ireturn(Some("x3")),
            ],
        );
        assert_ir_eq(
            function_irs[1].instructions.clone(),
            vec![
                ilic(3, "x"),
                icall("f", vec!["x"], "x2"),
                iprint_int("x2", "x3"),
                ireturn(None),
            ],
        );
    }

    #[test]
    fn test_ir_generator_short_circuit() {
        assert_ir_eq(
            gi(eif(eand(ebool(false), ebool(true)), eint(1), None), None).unwrap(),
            vec![
                ilbc(false, "x"),
                icondjump("x", "label3", "label4"),
                ilabel("label3"),
                ilbc(true, "x2"),
                icopy("x2", "x3"),
                ijump("label5"),
                ilabel("label4"),
                ilbc(false, "x3"),
                ijump("label5"),
                ilabel("label5"),
                icondjump("x3", "label", "label2"),
                ilabel("label"),
                ilic(1, "x4"),
                ilabel("label2"),
                ireturn(None),
            ],
        );
    }
}
