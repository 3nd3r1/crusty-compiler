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

    impl std::fmt::Display for Type {
        fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            let s = match self {
                Type::Int => "Int".to_string(),
                Type::Bool => "Bool".to_string(),
                Type::Unit => "Unit".to_string(),
                Type::Function {
                    params,
                    return_type,
                } => {
                    let params_str: Vec<String> = params.iter().map(|p| format!("{}", p)).collect();
                    format!("({}) -> {}", params_str.join(", "), *return_type)
                }
            };
            write!(f, "{}", s)
        }
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
                            "operator {} expected types {} and {} to be equal",
                            op, left_type, right_type
                        ))
                    } else {
                        Ok(Type::Bool)
                    }
                }
                _ => {
                    let identifier = op.to_string();
                    let func_type = symtab.borrow().lookup(&identifier)?;

                    if let Type::Function {
                        params,
                        return_type,
                    } = func_type
                    {
                        if params.len() != 2 {
                            Err(format!(
                                "operator {} expected {} params, got 2",
                                op,
                                params.len()
                            ))
                        } else if params[0] != left_type || params[1] != right_type {
                            Err(format!(
                                "operator {} expected ({}, {}), got ({}, {})",
                                op, params[0], params[1], left_type, right_type
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
            let operand_type = typecheck(&*operand, symtab)?;

            let identifier = format!("unary_{}", op);
            let func = symtab.borrow().lookup(&identifier)?;
            if let Type::Function {
                params,
                return_type,
            } = func
            {
                if params.len() != 1 {
                    Err(format!(
                        "operator {} expected {} params, got 1",
                        op,
                        params.len()
                    ))
                } else if params[0] != operand_type {
                    Err(format!(
                        "operator {} expected ({}), got ({})",
                        op, params[0], operand_type
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
                    "expected condition to be of type bool got {}",
                    condition_type
                ))
            } else {
                let then_type = typecheck(&*then_expression, symtab)?;
                if let Some(else_expression) = else_expression {
                    let else_type = typecheck(&*else_expression, symtab)?;

                    if then_type != else_type {
                        return Err(format!(
                            "expected then_expression and else_expression to be same type, got {} and {}",
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
            if condition_type != Type::Bool {
                Err(format!(
                    "expected condition to be of type {} got {}",
                    Type::Bool,
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

pub mod builtin_types {
    use super::*;

    pub fn build_builtin_types() -> HashMap<String, Type> {
        use ast::Operation::*;
        use ast::UnaryOperation::*;

        let mut types = HashMap::new();

        let int_int_to_int = Type::Function {
            params: vec![Type::Int, Type::Int],
            return_type: Box::new(Type::Int),
        };
        let int_int_to_bool = Type::Function {
            params: vec![Type::Int, Type::Int],
            return_type: Box::new(Type::Bool),
        };
        let bool_bool_to_bool = Type::Function {
            params: vec![Type::Bool, Type::Bool],
            return_type: Box::new(Type::Bool),
        };

        let int_to_int = Type::Function {
            params: vec![Type::Int],
            return_type: Box::new(Type::Int),
        };
        let bool_to_bool = Type::Function {
            params: vec![Type::Bool],
            return_type: Box::new(Type::Bool),
        };

        types.insert(format!("{}", Addition), int_int_to_int.clone());
        types.insert(format!("{}", Substraction), int_int_to_int.clone());
        types.insert(format!("{}", Multiplication), int_int_to_int.clone());
        types.insert(format!("{}", Division), int_int_to_int.clone());
        types.insert(format!("{}", Modulo), int_int_to_int.clone());
        types.insert(format!("{}", LessThan), int_int_to_bool.clone());
        types.insert(format!("{}", GreaterThan), int_int_to_bool.clone());
        types.insert(format!("{}", LessThanOrEqual), int_int_to_bool.clone());
        types.insert(format!("{}", GreaterThanOrEqual), int_int_to_bool.clone());
        types.insert(format!("{}", Or), bool_bool_to_bool.clone());
        types.insert(format!("{}", And), bool_bool_to_bool.clone());

        types.insert(format!("unary_{}", Neg), int_to_int.clone());
        types.insert(format!("unary_{}", Not), bool_to_bool.clone());

        types
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::tests::*;

    fn empty_symtab() -> TypeSymTab {
        TypeSymTab {
            locals: builtin_types::build_builtin_types(),
            parent: None,
        }
    }

    fn tc(node: &ast::Expression) -> Result<Type, String> {
        typecheck(node, &Rc::new(RefCell::new(empty_symtab())))
    }

    #[test]
    fn test_typechecker_binary() {
        assert_eq!(tc(&eadd(eint(2), eint(3))).unwrap(), Type::Int);
        assert!(
            tc(&eadd(eint(2), ebool(false)))
                .unwrap_err()
                .contains("operator + expected (Int, Int), got (Int, Bool)")
        );
    }

    #[test]
    fn test_typechecker_unary() {
        assert_eq!(tc(&eneg(eint(2))).unwrap(), Type::Int);
        assert!(
            tc(&enot(eint(2)))
                .unwrap_err()
                .contains("operator not expected (Bool), got (Int)")
        );
    }

    #[test]
    fn test_typechecker_assignment() {
        assert_eq!(
            tc(&eblock(vec![
                evar("a", eint(3)),
                evar("b", eint(2)),
                eadd(eide("a"), eide("b"))
            ]))
            .unwrap(),
            Type::Int
        );
        assert!(
            tc(&eblock(vec![
                evar("a", eint(3)),
                eadd(eide("a"), eide("b"))
            ]))
            .unwrap_err()
            .contains("undefined identifier: b")
        );
    }

    #[test]
    fn test_typechecker_while() {
        assert_eq!(
            tc(&eblock(vec![
                evar("a", eint(0)),
                ewhile(
                    elt(eide("a"), eint(5)),
                    eassign("a", eadd(eide("a"), eint(1)))
                ),
                eide("a")
            ]))
            .unwrap(),
            Type::Int
        );
    }
}
