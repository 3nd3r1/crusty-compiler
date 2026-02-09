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
pub enum UnaryOperation {
    Neg,
    Not,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    IntLiteral {
        value: i32,
    },
    BoolLiteral {
        value: bool,
    },
    NoneLiteral,
    Identifier {
        value: String,
    },
    BinaryOp {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operation,
    },
    UnaryOp {
        operand: Box<Expression>,
        op: UnaryOperation,
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
    Assignment {
        left: Box<Expression>,
        right: Box<Expression>,
    },
    Block {
        expressions: Vec<Expression>,
    },
    While {
        condition: Box<Expression>,
        do_expression: Box<Expression>,
    },
    VarDeclaration {
        name: String,
        value: Box<Expression>,
    },
}
