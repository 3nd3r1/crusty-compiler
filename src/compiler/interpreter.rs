use crate::compiler::ast;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Int(i32),
    Bool(bool),
    None,
}

pub fn interpret(node: ast::Expression) -> Result<Value, String> {
    match node.kind {
        ast::ExpressionKind::NoneLiteral { .. } => Ok(Value::None),
        ast::ExpressionKind::IntLiteral { value } => Ok(Value::Int(value)),
        ast::ExpressionKind::BoolLiteral { value } => Ok(Value::Bool(value)),
        ast::ExpressionKind::BinaryOp { left, right, op } => {
            let a = interpret(*left)?;
            let b = interpret(*right)?;
            match (a, b, op) {
                (Value::Int(a), Value::Int(b), ast::Operation::Addition) => Ok(Value::Int(a + b)),
                (_, _, op) => Err(format!("unexpected operation {:?}", op)),
            }
        }
        ast::ExpressionKind::If {
            condition,
            then_expression,
            else_expression,
        } => {
            let cond = interpret(*condition)?;
            match (cond, then_expression, else_expression) {
                (Value::Bool(true), then_expression, _) => interpret(*then_expression),
                (Value::Bool(false), _, Some(else_expression)) => interpret(*else_expression),
                (cond, _, _) => Err(format!("unexpected condition {:?}", cond)),
            }
        }
        kind => Err(format!("unexpected expression {:?}", kind)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::compiler::parser::tests::*;

    #[test]
    fn test_interpreter_basics() {
        assert_eq!(interpret(eadd(eint(2), eint(3))).unwrap(), Value::Int(5));
    }
}
