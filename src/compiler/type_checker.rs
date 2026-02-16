use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::compiler::ast;

pub mod types {
    #[derive(Debug, Clone, PartialEq)]
    pub enum Type {
        Int,
        Bool,
        Unit,
        Function {
            params: Vec<Type>,
            return_type: Box<Type>,
        },
    }
}

use types::Type;

#[derive(Debug, Clone, PartialEq)]
pub struct TypeSymTab {
    pub locals: HashMap<String, Type>,
    pub parent: Option<Rc<RefCell<TypeSymTab>>>,
}

impl TypeSymTab {
    fn lookup(&self, identifier: &str) -> Result<Type, String> {
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

    fn declare(&mut self, identifier: &str, value: Type) {
        self.locals.insert(identifier.to_string(), value);
    }

    fn assign(&mut self, identifier: &str, value: Type) -> Result<(), String> {
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

pub fn typecheck(node: &ast::Expression, symtab: &Rc<RefCell<TypeSymTab>>) -> Result<Type, String> {
    match &node.kind {
        ast::ExpressionKind::NoneLiteral { .. } => Ok(Type::Unit),
        ast::ExpressionKind::IntLiteral { .. } => Ok(Type::Int),
        ast::ExpressionKind::BoolLiteral { .. } => Ok(Type::Bool),
        ast::ExpressionKind::Identifier { value } => symtab.borrow().lookup(value),
        ast::ExpressionKind::BinaryOp { left, right, op } => {
            let left_type = typecheck(&*left, symtab)?;
            let right_type = typecheck(&*right, symtab)?;

            match &op {
                ast::Operation::Equal | ast::Operation::NotEqual => {
                    if left_type != right_type {
                        Err(format!(
                            "operator {} expected types {:?} and {:?} to be equal",
                            op, left_type, right_type
                        ))
                    } else {
                        Ok(Type::Bool)
                    }
                }
                _ => {
                    let identifier = op.to_string();
                    let func = symtab.borrow().lookup(&identifier)?;

                    if let Type::Function {
                        params,
                        return_type,
                    } = func
                    {
                        if params.len() != 2 || params[0] != left_type || params[1] != right_type {
                            Err(format!(
                                "operator {} expected {:?} got ({:?}, {:?})",
                                op, params, left, right
                            ))
                        } else {
                            Ok(*return_type)
                        }
                    } else {
                        Err(format!("unexpected operator {}", op))
                    }
                }
            }
        }
        ast::ExpressionKind::UnaryOp { operand, op } => {
            let operand = typecheck(&*operand, symtab)?;

            let identifier = format!("unary_{}", op);
            let func = symtab.borrow().lookup(&identifier)?;
            if let Type::Function {
                params,
                return_type,
            } = func
            {
                if params.len() != 1 || params[0] != operand {
                    Err(format!(
                        "operator {} expected {:?} got ({:?})",
                        op, params, operand
                    ))
                } else {
                    Ok(*return_type)
                }
            } else {
                Err(format!("unexpected operator {}", op))
            }
        }
        ast::ExpressionKind::If {
            condition,
            then_expression,
            else_expression,
        } => {
            let condition_type = typecheck(&*condition, symtab)?;
            if condition_type != Type::Bool {
                Err(format!(
                    "expected condition to be of type bool got {:?}",
                    condition_type
                ))
            } else {
                let then_type = typecheck(&*then_expression, symtab)?;
                if let Some(else_expression) = else_expression {
                    let else_type = typecheck(&*else_expression, symtab)?;

                    if then_type != else_type {
                        return Err(format!(
                            "expected then_expression and else_expression to be same type, got {:?} and {:?}",
                            then_type, else_type
                        ));
                    }
                }
                Ok(then_type)
            }
        }
        ast::ExpressionKind::VarDeclaration { name, value } => {
            let value_type = typecheck(&*value, symtab)?;
            symtab.borrow_mut().declare(name, value_type);
            Ok(Type::Unit)
        }
        ast::ExpressionKind::Assignment { name, right } => {
            let value_type = typecheck(&*right, symtab)?;
            symtab.borrow_mut().assign(name, value_type.clone())?;
            Ok(value_type)
        }
        ast::ExpressionKind::Block { expressions } => {
            let block_symtab = Rc::new(RefCell::new(TypeSymTab {
                locals: HashMap::new(),
                parent: Some(Rc::clone(symtab)),
            }));

            if let Some((last, expressions)) = expressions.split_last() {
                for expression in expressions {
                    typecheck(expression, &block_symtab)?;
                }
                typecheck(last, &block_symtab)
            } else {
                Ok(Type::Unit)
            }
        }
        ast::ExpressionKind::While {
            condition,
            do_expression: _,
        } => {
            let condition_type = typecheck(&*condition, symtab)?;
            if condition_type == Type::Bool {
                Err(format!(
                    "expected condition to be of type bool got {:?}",
                    condition_type
                ))
            } else {
                Ok(Type::Unit)
            }
        }
        ast::ExpressionKind::FunctionCall { .. } => {
            todo!()
        }
    }
}
