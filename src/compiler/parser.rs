use crate::compiler::{ast, tokenizer};

struct Parser {
    tokens: Vec<tokenizer::Token>,
    pos: usize,
}

impl Parser {
    fn peek(&self) -> &tokenizer::Token {
        self.tokens.get(self.pos).unwrap()
    }

    fn consume(
        &mut self,
        token_kind: tokenizer::TokenKind,
        expected: Option<&str>,
    ) -> Result<&tokenizer::Token, String> {
        let token = &self.tokens[self.pos];

        if token.kind != token_kind {
            return Err(format!("{:?}: expected an {:?}", token.loc, token_kind));
        }

        if expected.is_some_and(|value| token.text != value) {
            return Err(format!(
                "{:?}: expected '{:?}'",
                token.loc,
                expected.unwrap()
            ));
        }

        self.pos += 1;
        if self.pos >= self.tokens.len() {
            self.pos = self.tokens.len() - 1;
        }

        Ok(token)
    }

    fn parse_int_literal(&mut self) -> Result<ast::Expression, String> {
        let token = self.consume(tokenizer::TokenKind::IntLiteral, None)?;
        Ok(ast::Expression::Literal {
            value: token
                .text
                .parse::<i32>()
                .map_err(|_| format!("{:?}: invalid number", token.loc))?,
        })
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression, String> {
        let token = self.consume(tokenizer::TokenKind::Identifier, None)?;
        Ok(ast::Expression::Identifier {
            value: token.text.clone(),
        })
    }

    fn parse_term(&mut self) -> Result<ast::Expression, String> {
        let mut left = self.parse_factor()?;

        while self.peek().kind == tokenizer::TokenKind::Operator
            && matches!(self.peek().text.as_str(), "*" | "/")
        {
            let operation = self.parse_operator()?;
            let right = self.parse_factor()?;

            left = ast::Expression::BinaryOp {
                left: Box::new(left),
                right: Box::new(right),
                op: operation,
            }
        }

        Ok(left)
    }

    fn parse_factor(&mut self) -> Result<ast::Expression, String> {
        match self.peek().kind {
            tokenizer::TokenKind::IntLiteral => return self.parse_int_literal(),
            tokenizer::TokenKind::Identifier => return self.parse_identifier(),
            _ => {
                return Err(format!(
                    "{:?}: expected an integer or identifier",
                    self.peek().loc
                ));
            }
        }
    }

    fn parse_operator(&mut self) -> Result<ast::Operation, String> {
        let token = self.consume(tokenizer::TokenKind::Operator, None)?;
        match token.text.as_str() {
            "+" => return Ok(ast::Operation::Addition),
            "-" => return Ok(ast::Operation::Substraction),
            "*" => return Ok(ast::Operation::Multiplication),
            "/" => return Ok(ast::Operation::Division),
            _ => {
                return Err(format!("{:?}: expected an operator", token.loc));
            }
        }
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, String> {
        let mut left = self.parse_term()?;

        while self.peek().kind == tokenizer::TokenKind::Operator
            && matches!(self.peek().text.as_str(), "+" | "-")
        {
            let operation = self.parse_operator()?;
            let right = self.parse_term()?;

            left = ast::Expression::BinaryOp {
                left: Box::new(left),
                right: Box::new(right),
                op: operation,
            }
        }

        Ok(left)
    }
}

pub fn parse(tokens: Vec<tokenizer::Token>) -> Result<ast::Expression, String> {
    let mut parser = Parser { tokens, pos: 0 };
    parser.parse_expression()
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokenizer::tokenize;

    fn lit(value: i32) -> Box<ast::Expression> {
        Box::new(ast::Expression::Literal { value })
    }

    fn add(value_a: i32, value_b: i32) -> ast::Expression {
        ast::Expression::BinaryOp {
            left: lit(value_a),
            right: lit(value_b),
            op: ast::Operation::Addition,
        }
    }

    fn sub(value_a: i32, value_b: i32) -> ast::Expression {
        ast::Expression::BinaryOp {
            left: lit(value_a),
            right: lit(value_b),
            op: ast::Operation::Substraction,
        }
    }

    #[test]
    fn test_parse_addition() {
        assert_eq!(parse(tokenize("1+1").unwrap()).unwrap(), add(1, 1));
    }

    #[test]
    fn test_parse_substraction() {
        assert_eq!(parse(tokenize("1-1").unwrap()).unwrap(), sub(1, 1));
    }
}
