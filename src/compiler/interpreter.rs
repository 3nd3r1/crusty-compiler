use std::{collections::HashMap, rc::Rc};

use crate::compiler::ast;

#[derive(Debug, Clone)]
pub enum Value {
    Int(i32),
    Bool(bool),
    BuiltInFunction(fn(Vec<Value>) -> Result<Value, String>),
    None,
}

impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Value::Int(a), Value::Int(b)) => a == b,
            (Value::Bool(a), Value::Bool(b)) => a == b,
            (Value::None, Value::None) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymTab {
    pub locals: HashMap<String, Value>,
    pub parent: Option<Rc<SymTab>>,
}

pub fn interpret(node: &ast::Expression, symtab: &mut SymTab) -> Result<Value, String> {
    match &node.kind {
        ast::ExpressionKind::NoneLiteral { .. } => Ok(Value::None),
        ast::ExpressionKind::IntLiteral { value } => Ok(Value::Int(*value)),
        ast::ExpressionKind::BoolLiteral { value } => Ok(Value::Bool(*value)),
        ast::ExpressionKind::BinaryOp { left, right, op } => {
            let left = interpret(&*left, symtab)?;
            let right = interpret(&*right, symtab)?;

            let key = op.to_string();

            if let Some(Value::BuiltInFunction(func)) = symtab.locals.get(&key) {
                func(vec![left, right])
            } else {
                Err(format!("unexpected operator {:?}", op))
            }
        }
        ast::ExpressionKind::UnaryOp { operand, op } => {
            let operand = interpret(&*operand, symtab)?;
            let key = format!("unary_{}", op);

            if let Some(Value::BuiltInFunction(func)) = symtab.locals.get(&key) {
                func(vec![operand])
            } else {
                Err(format!("unexpected operator {:?}", op))
            }
        }
        ast::ExpressionKind::If {
            condition,
            then_expression,
            else_expression,
        } => {
            let cond = interpret(&*condition, symtab)?;
            match (cond, then_expression, else_expression) {
                (Value::Bool(true), then_expression, _) => interpret(&*then_expression, symtab),
                (Value::Bool(false), _, Some(else_expression)) => {
                    interpret(&*else_expression, symtab)
                }
                (cond, _, _) => Err(format!("unexpected condition {:?}", cond)),
            }
        }
        ast::ExpressionKind::VarDeclaration { name, value } => {
            let value = interpret(&*value, symtab)?;
            symtab.locals.insert(name.clone(), value);
            Ok(Value::None)
        }
        ast::ExpressionKind::Block { expressions } => {
            let mut block_symtab = SymTab {
                locals: HashMap::new(),
                parent: Some(Rc::new(symtab.clone())),
            };

            if let Some((last, expressions)) = expressions.split_last() {
                for expression in expressions {
                    interpret(expression, &mut block_symtab)?;
                }
                interpret(last, symtab)
            } else {
                Ok(Value::None)
            }
        }
        kind => Err(format!("unexpected expression {:?}", kind)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::tests::*;

    fn empty_symtab() -> SymTab {
        SymTab {
            locals: HashMap::new(),
            parent: None,
        }
    }

    #[test]
    fn test_interpreter_basics() {
        assert_eq!(
            interpret(&eadd(eint(2), eint(3)), &mut empty_symtab()).unwrap(),
            Value::Int(5)
        );
    }
}
