#[derive(Debug, Clone, PartialEq)]
pub enum Operation {
    Addition,
    Substraction,
    Multiplication,
    Division,
    Modulo,

    LessThan,
    GreaterThan,
    Equal,
    NotEqual,
    LessThanOrEqual,
    GreaterThanOrEqual,

    Or,
    And,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    IntLiteral {
        value: i32,
    },
    BoolLiteral {
        value: bool,
    },
    Identifier {
        value: String,
    },
    BinaryOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operation,
    },

    If {
        condition: Box<Expression>,
        then_expression: Box<Expression>,
        else_expression: Option<Box<Expression>>,
    },

    FunctionCall {
        name: String,
        arguments: Vec<Expression>,
    },
}
