use std::{
    cell::RefCell,
    collections::{HashMap, HashSet},
    rc::Rc,
};

use crate::compiler::{ast, ir};

#[derive(Debug, Clone, PartialEq)]
pub struct IrSymTab {
    pub locals: HashMap<String, ir::IRVar>,
    pub parent: Option<Rc<RefCell<IrSymTab>>>,
}

impl IrSymTab {
    fn lookup(&self, identifier: &str) -> Result<ir::IRVar, String> {
        if let Some(value) = self.locals.get(identifier) {
            Ok(value.clone())
        } else {
            if let Some(parent) = &self.parent {
                parent.borrow().lookup(identifier)
            } else {
                Err(format!("undefined identifier: {}", identifier))
            }
        }
    }

    fn declare(&mut self, identifier: &str, value: ir::IRVar) {
        self.locals.insert(identifier.to_string(), value);
    }

    fn assign(&mut self, identifier: &str, value: ir::IRVar) -> Result<(), String> {
        if let Some(_) = self.locals.get(identifier) {
            self.locals.insert(identifier.to_string(), value.clone());
            Ok(())
        } else {
            if let Some(parent) = &self.parent {
                parent.borrow_mut().assign(identifier, value)
            } else {
                Err(format!("undefined identifier: {}", identifier))
            }
        }
    }
}

struct IrGenerator {
    ins: Vec<ir::Instruction>,
    symtab: Rc<RefCell<IrSymTab>>,
}

impl IrGenerator {
    fn new_var(&mut self) -> ir::IRVar {
        todo!()
    }

    fn generate(&mut self, node: &mut ast::Expression) -> Result<Vec<ir::Instruction>, String> {
        let var_final_result = self.visit(node);
        Ok(self.ins.clone())
    }

    fn visit(&mut self, node: &mut ast::Expression) -> Result<ir::IRVar, String> {
        match &mut node.kind {
            ast::ExpressionKind::NoneLiteral { .. } => Ok(ir::IRVar {
                name: "unit".to_string(),
            }),
            ast::ExpressionKind::IntLiteral { value } => {
                let var = self.new_var();
                self.ins.push(ir::Instruction {
                    kind: ir::InstructionKind::LoadIntConst {
                        value: *value,
                        dest: var.clone(),
                    },
                    location: node.loc.clone(),
                });
                Ok(var)
            }
            ast::ExpressionKind::BoolLiteral { value } => {
                let var = self.new_var();
                self.ins.push(ir::Instruction {
                    kind: ir::InstructionKind::LoadBoolConst {
                        value: *value,
                        dest: var.clone(),
                    },
                    location: node.loc.clone(),
                });
                Ok(var)
            }
            ast::ExpressionKind::Identifier { value } => self.symtab.borrow().lookup(&value),
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
    };
    ir_generator.generate(root_expr)
}
