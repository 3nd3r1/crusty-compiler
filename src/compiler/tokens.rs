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

impl std::fmt::Display for TokenKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let s = match self {
            TokenKind::Keyword => "Keyword",
            TokenKind::IntLiteral => "IntLiteral",
            TokenKind::BoolLiteral => "BoolLiteral",
            TokenKind::Operator => "Operator",
            TokenKind::Identifier => "Identifier",
            TokenKind::Punctuation => "Punctuation",
            TokenKind::End => "End",
        };
        write!(f, "{}", s)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub loc: Location,
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}({:?})", self.kind, self.text)
    }
}
