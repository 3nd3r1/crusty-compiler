use crate::compiler::common::Location;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Keyword,
    IntLiteral,
    BoolLiteral,
    Operator,
    Identifier,
    Punctuation,
    End,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub loc: Location,
}
