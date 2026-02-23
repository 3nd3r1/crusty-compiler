use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::compiler::{ast, common::SymTab, ir};

type IrSymTab = SymTab<ir::IRVar>;

struct IrGenerator {
    ins: Vec<ir::Instruction>,
    symtab: Rc<RefCell<IrSymTab>>,
    var_counter: u32,
    label_counter: u32,
}

impl IrGenerator {
    fn new_var(&mut self) -> ir::IRVar {
        self.var_counter += 1;
        if self.var_counter == 1 {
            ir::IRVar {
                name: "x".to_string(),
            }
        } else {
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

    fn unit_var(&mut self) -> ir::IRVar {
        ir::IRVar {
            name: "unit".to_string(),
        }
    }

    fn generate(&mut self, node: &mut ast::Expression) -> Result<Vec<ir::Instruction>, String> {
        self.visit(node)?;
        Ok(self.ins.clone())
    }

    fn visit(&mut self, node: &mut ast::Expression) -> Result<ir::IRVar, String> {
        match &mut node.kind {
            ast::ExpressionKind::NoneLiteral { .. } => Ok(ir::IRVar {
                name: "unit".to_string(),
            }),
            ast::ExpressionKind::IntLiteral { value } => {
                let var = self.new_var();
                self.ins.push(ir::Instruction::load_int_const(
                    value.clone(),
                    var.clone(),
                    node.loc.clone(),
                ));
                Ok(var)
            }
            ast::ExpressionKind::BoolLiteral { value } => {
                let var = self.new_var();
                self.ins.push(ir::Instruction::load_bool_const(
                    value.clone(),
                    var.clone(),
                    node.loc.clone(),
                ));
                Ok(var)
            }
            ast::ExpressionKind::Identifier { value } => self.symtab.borrow().lookup(&value),
            ast::ExpressionKind::BinaryOp { left, right, op } => {
                let var_op = self.symtab.borrow().lookup(&op.to_string())?;
                let var_left = self.visit(left)?;
                let var_right = self.visit(right)?;
                let var_result = self.new_var();
                self.ins.push(ir::Instruction::call(
                    var_op,
                    vec![var_left, var_right],
                    var_result.clone(),
                    node.loc.clone(),
                ));
                Ok(var_result)
            }
            ast::ExpressionKind::If {
                condition,
                then_expression,
                else_expression,
            } => match else_expression {
                Some(else_expression) => {
                    let l_then = self.new_label();
                    let l_else = self.new_label();
                    let l_end = self.new_label();
                    let var_cond = self.visit(&mut *condition)?;

                    self.ins.push(ir::Instruction::cond_jump(
                        var_cond,
                        l_then.clone(),
                        l_else.clone(),
                        node.loc.clone(),
                    ));

                    self.ins
                        .push(ir::Instruction::label(l_then, node.loc.clone()));

                    self.visit(&mut *then_expression)?;

                    self.ins
                        .push(ir::Instruction::jump(l_end.clone(), node.loc.clone()));

                    self.ins
                        .push(ir::Instruction::label(l_else, node.loc.clone()));

                    self.visit(&mut *else_expression)?;

                    self.ins
                        .push(ir::Instruction::label(l_end, node.loc.clone()));

                    Ok(self.unit_var())
                }
                _ => {
                    let l_then = self.new_label();
                    let l_end = self.new_label();
                    let var_cond = self.visit(&mut *condition)?;

                    self.ins.push(ir::Instruction::cond_jump(
                        var_cond,
                        l_then.clone(),
                        l_end.clone(),
                        node.loc.clone(),
                    ));

                    self.ins
                        .push(ir::Instruction::label(l_then, node.loc.clone()));

                    self.visit(&mut *then_expression)?;

                    self.ins
                        .push(ir::Instruction::label(l_end, node.loc.clone()));

                    Ok(self.unit_var())
                }
            },
            _ => Err(format!(
                "{:?}: unsupported expression: {:?}",
                node.loc, node
            )),
        }
    }
}

pub fn generate_ir(
    reserved_names: HashSet<String>,
    root_expr: &mut ast::Expression,
) -> Result<Vec<ir::Instruction>, String> {
    let mut root_symtab = IrSymTab {
        locals: HashMap::new(),
        parent: None,
    };
    for name in reserved_names {
        root_symtab.declare(&name, ir::IRVar { name: name.clone() });
    }

    let mut ir_generator = IrGenerator {
        ins: Vec::new(),
        symtab: Rc::new(RefCell::new(root_symtab)),
        var_counter: 0,
        label_counter: 0,
    };
    ir_generator.generate(root_expr)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::tests::*;
    use crate::compiler::tokenizer::tests::loc;

    fn get_reserved_names() -> HashSet<String> {
        use ast::Operation::*;
        use ast::UnaryOperation::*;

        let mut reserved_names = HashSet::new();

        reserved_names.insert(format!("{}", Addition));
        reserved_names.insert(format!("{}", Substraction));
        reserved_names.insert(format!("{}", Multiplication));
        reserved_names.insert(format!("{}", Division));
        reserved_names.insert(format!("{}", Modulo));
        reserved_names.insert(format!("{}", LessThan));
        reserved_names.insert(format!("{}", GreaterThan));
        reserved_names.insert(format!("{}", LessThanOrEqual));
        reserved_names.insert(format!("{}", GreaterThanOrEqual));
        reserved_names.insert(format!("{}", Or));
        reserved_names.insert(format!("{}", And));
        reserved_names.insert(format!("unary_{}", Neg));
        reserved_names.insert(format!("unary_{}", Not));

        reserved_names
    }

    fn gi(mut node: ast::Expression) -> Result<Vec<ir::Instruction>, String> {
        generate_ir(get_reserved_names(), &mut node)
    }

    fn assert_ir_eq(left: Vec<ir::Instruction>, right: Vec<ir::Instruction>) {
        assert!(
            left == right,
            "expected:\n{}\ngot:\n{}\n",
            right
                .iter()
                .map(|i| format!("{}", i))
                .collect::<Vec<_>>()
                .join("\n"),
            left.iter()
                .map(|i| format!("{}", i))
                .collect::<Vec<_>>()
                .join("\n"),
        )
    }

    fn ilic(value: i32, dest: &str) -> ir::Instruction {
        ir::Instruction::load_int_const(
            value,
            ir::IRVar {
                name: dest.to_string(),
            },
            loc(),
        )
    }

    fn imul(left: &str, right: &str, dest: &str) -> ir::Instruction {
        icall(
            &ast::Operation::Multiplication.to_string(),
            vec![left, right],
            dest,
        )
    }

    fn iadd(left: &str, right: &str, dest: &str) -> ir::Instruction {
        icall(
            &ast::Operation::Addition.to_string(),
            vec![left, right],
            dest,
        )
    }

    fn iprint(var: &str, dest: &str) -> ir::Instruction {
        icall("print_int", vec![var], dest)
    }

    fn icall(fun: &str, args: Vec<&str>, dest: &str) -> ir::Instruction {
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

    #[test]
    fn test_ir_generator_math() {
        assert_ir_eq(
            gi(eadd(eint(1), emul(eint(2), eint(3)))).unwrap(),
            vec![
                ilic(1, "x"),
                ilic(2, "x2"),
                ilic(3, "x3"),
                imul("x2", "x3", "x4"),
                iadd("x", "x4", "x5"),
                iprint("x5", "x6"),
            ],
        );
    }
}
