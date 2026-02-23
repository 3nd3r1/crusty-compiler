use crate::compiler::common::Location;
use crate::compiler::types::Type;

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

impl std::fmt::Display for Operation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            Operation::Addition => "+",
            Operation::Substraction => "-",
            Operation::Multiplication => "*",
            Operation::Division => "/",
            Operation::Modulo => "%",
            Operation::LessThan => "<",
            Operation::GreaterThan => ">",
            Operation::Equal => "==",
            Operation::NotEqual => "!=",
            Operation::LessThanOrEqual => "<=",
            Operation::GreaterThanOrEqual => ">=",
            Operation::Or => "or",
            Operation::And => "and",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperation {
    Neg,
    Not,
}

impl std::fmt::Display for UnaryOperation {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            UnaryOperation::Neg => "-",
            UnaryOperation::Not => "not",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Expression {
    pub kind: ExpressionKind,
    pub loc: Location,
    pub return_type: Option<Type>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExpressionKind {
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
        name: String,
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
        value_type: Option<Box<Type>>,
    },
}
