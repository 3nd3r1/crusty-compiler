use std::{cell::RefCell, collections::HashMap, rc::Rc};

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
    pub parent: Option<Rc<RefCell<SymTab>>>,
}

impl SymTab {
    fn lookup(&self, identifier: &str) -> Result<Value, String> {
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

    fn declare(&mut self, identifier: &str, value: Value) {
        self.locals.insert(identifier.to_string(), value);
    }

    fn assign(&mut self, identifier: &str, value: Value) -> Result<(), String> {
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

pub fn interpret(node: &ast::Expression, symtab: &Rc<RefCell<SymTab>>) -> Result<Value, String> {
    match &node.kind {
        ast::ExpressionKind::NoneLiteral { .. } => Ok(Value::None),
        ast::ExpressionKind::IntLiteral { value } => Ok(Value::Int(*value)),
        ast::ExpressionKind::BoolLiteral { value } => Ok(Value::Bool(*value)),
        ast::ExpressionKind::Identifier { value } => symtab.borrow().lookup(value),
        ast::ExpressionKind::BinaryOp { left, right, op } => {
            let left = interpret(&*left, symtab)?;
            let right = interpret(&*right, symtab)?;
            let identifier = op.to_string();

            let func = symtab.borrow().lookup(&identifier)?;
            if let Value::BuiltInFunction(func) = func {
                func(vec![left, right])
            } else {
                Err(format!("unexpected operator {}", op))
            }
        }
        ast::ExpressionKind::UnaryOp { operand, op } => {
            let operand = interpret(&*operand, symtab)?;
            let identifier = format!("unary_{}", op);

            let func = symtab.borrow().lookup(&identifier)?;
            if let Value::BuiltInFunction(func) = func {
                func(vec![operand])
            } else {
                Err(format!("unexpected operator {}", op))
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
            symtab.borrow_mut().declare(name, value);
            Ok(Value::None)
        }
        ast::ExpressionKind::Assignment { name, right } => {
            let value = interpret(&*right, symtab)?;
            symtab.borrow_mut().assign(name, value.clone())?;
            Ok(value)
        }
        ast::ExpressionKind::Block { expressions } => {
            let block_symtab = Rc::new(RefCell::new(SymTab {
                locals: HashMap::new(),
                parent: Some(Rc::clone(symtab)),
            }));

            if let Some((last, expressions)) = expressions.split_last() {
                for expression in expressions {
                    interpret(expression, &block_symtab)?;
                }
                interpret(last, &block_symtab)
            } else {
                Ok(Value::None)
            }
        }
        kind => Err(format!("unexpected expression {:?}", kind)),
    }
}

pub mod builtins {
    use super::*;

    pub fn addition(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left + right)),
            _ => Err(format!("expected two integers")),
        }
    }

    pub fn substraction(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left - right)),
            _ => Err(format!("expected two integers")),
        }
    }

    pub fn multiplication(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left * right)),
            _ => Err(format!("expected two integers")),
        }
    }

    pub fn division(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left / right)),
            _ => Err(format!("expected two integers")),
        }
    }

    pub fn modulo(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Int(left % right)),
            _ => Err(format!("expected two integers")),
        }
    }

    pub fn less_than(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left < right)),
            _ => Err(format!("expected two integers")),
        }
    }

    pub fn greater_than(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left > right)),
            _ => Err(format!("expected two integers")),
        }
    }

    pub fn equal(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (left, right) => Ok(Value::Bool(left == right)),
        }
    }

    pub fn not_equal(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (left, right) => Ok(Value::Bool(left != right)),
        }
    }

    pub fn less_than_or_equal(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left <= right)),
            _ => Err(format!("expected two integers")),
        }
    }

    pub fn greater_than_or_equal(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Int(left), Value::Int(right)) => Ok(Value::Bool(left >= right)),
            _ => Err(format!("expected two integers")),
        }
    }

    pub fn or(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(*left || *right)),
            _ => Err(format!("expected two bools")),
        }
    }

    pub fn and(args: Vec<Value>) -> Result<Value, String> {
        match (&args[0], &args[1]) {
            (Value::Bool(left), Value::Bool(right)) => Ok(Value::Bool(*left && *right)),
            _ => Err(format!("expected two bools")),
        }
    }

    pub fn unary_neg(args: Vec<Value>) -> Result<Value, String> {
        match &args[0] {
            Value::Int(operand) => Ok(Value::Int(-operand)),
            _ => Err(format!("expected one integer")),
        }
    }

    pub fn unary_not(args: Vec<Value>) -> Result<Value, String> {
        match &args[0] {
            Value::Bool(operand) => Ok(Value::Bool(!operand)),
            _ => Err(format!("expected one bool")),
        }
    }

    pub fn build_builtin_lib() -> HashMap<String, Value> {
        use Value::BuiltInFunction;
        use ast::Operation::*;
        use ast::UnaryOperation::*;

        let mut lib = HashMap::new();

        lib.insert(format!("{}", Addition), BuiltInFunction(addition));
        lib.insert(format!("{}", Substraction), BuiltInFunction(substraction));
        lib.insert(
            format!("{}", Multiplication),
            BuiltInFunction(multiplication),
        );
        lib.insert(format!("{}", Division), BuiltInFunction(division));
        lib.insert(format!("{}", Modulo), BuiltInFunction(modulo));
        lib.insert(format!("{}", LessThan), BuiltInFunction(less_than));
        lib.insert(format!("{}", GreaterThan), BuiltInFunction(greater_than));
        lib.insert(format!("{}", Equal), BuiltInFunction(equal));
        lib.insert(format!("{}", NotEqual), BuiltInFunction(not_equal));
        lib.insert(
            format!("{}", LessThanOrEqual),
            BuiltInFunction(less_than_or_equal),
        );
        lib.insert(
            format!("{}", GreaterThanOrEqual),
            BuiltInFunction(greater_than_or_equal),
        );
        lib.insert(format!("{}", Or), BuiltInFunction(or));
        lib.insert(format!("{}", And), BuiltInFunction(and));

        lib.insert(format!("unary_{}", Neg), BuiltInFunction(unary_neg));
        lib.insert(format!("unary_{}", Not), BuiltInFunction(unary_not));

        lib
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::tests::*;

    fn empty_symtab() -> SymTab {
        SymTab {
            locals: builtins::build_builtin_lib(),
            parent: None,
        }
    }

    fn ip(node: &ast::Expression) -> Result<Value, String> {
        interpret(node, &Rc::new(RefCell::new(empty_symtab())))
    }

    #[test]
    fn test_interpreter_binary() {
        assert_eq!(ip(&eadd(eint(2), eint(3))).unwrap(), Value::Int(5));
        assert_eq!(ip(&esub(eint(2), eint(3))).unwrap(), Value::Int(-1));
        assert_eq!(
            ip(&eadd(eint(2), emul(eint(2), eint(3)))).unwrap(),
            Value::Int(8)
        );
    }

    #[test]
    fn test_interpreter_unary() {
        assert_eq!(ip(&eneg(eint(2))).unwrap(), Value::Int(-2));
        assert_eq!(ip(&eneg(eneg(eint(2)))).unwrap(), Value::Int(2));
        assert_eq!(ip(&enot(ebool(true))).unwrap(), Value::Bool(false));
    }

    #[test]
    fn test_interpreter_assignment() {
        assert_eq!(
            ip(&eblock(vec![
                evar("a", eint(3)),
                evar("b", eint(2)),
                eadd(eide("a"), eide("b"))
            ]))
            .unwrap(),
            Value::Int(5)
        );

        assert_eq!(
            ip(&eblock(vec![
                evar("a", eint(3)),
                evar("b", eint(2)),
                eassign("a", eassign("b", eint(6)))
            ]))
            .unwrap(),
            Value::Int(6)
        );
    }

    #[test]
    fn test_interpreter_short_circuit() {
        assert_eq!(
            ip(&eblock(vec![
                evar("a", ebool(false)),
                eor(
                    ebool(true),
                    eblock(vec![eassign("a", ebool(true)), ebool(true)])
                ),
                eide("a")
            ]))
            .unwrap(),
            Value::Bool(false)
        );
    }
}
