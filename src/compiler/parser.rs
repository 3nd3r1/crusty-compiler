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
        Ok(ast::Expression::IntLiteral {
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
            tokenizer::TokenKind::Punctuation if self.peek().text.as_str() == "(" => {
                return self.parse_parenthesized();
            }
            _ => {
                return Err(format!(
                    "{:?}: expected an integer, identifier or '('",
                    self.peek().loc
                ));
            }
        }
    }

    fn parse_parenthesized(&mut self) -> Result<ast::Expression, String> {
        self.consume(tokenizer::TokenKind::Punctuation, Some("("))?;
        let expression = self.parse_expression()?;
        self.consume(tokenizer::TokenKind::Punctuation, Some(")"))?;

        Ok(expression)
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

    fn parse(&mut self) -> Result<ast::Expression, String> {
        let expression = self.parse_expression()?;

        if self.peek().kind != tokenizer::TokenKind::End {
            return Err(format!(
                "{:?}: unexpected token {:?} expected end",
                self.peek().loc,
                self.peek().text
            ));
        }

        Ok(expression)
    }
}

pub fn parse(tokens: Vec<tokenizer::Token>) -> Result<ast::Expression, String> {
    let mut parser = Parser { tokens, pos: 0 };
    parser.parse()
}

#[cfg(test)]
mod tests {
    use super::*;
    use tokenizer::tokenize;

    fn int(value: i32) -> ast::Expression {
        ast::Expression::IntLiteral { value }
    }

    fn bool(value: bool) -> ast::Expression {
        ast::Expression::BoolLiteral { value }
    }

    fn ide(value: &str) -> ast::Expression {
        ast::Expression::Identifier {
            value: value.to_string(),
        }
    }

    fn add(expression_a: ast::Expression, expression_b: ast::Expression) -> ast::Expression {
        ast::Expression::BinaryOp {
            left: Box::new(expression_a),
            right: Box::new(expression_b),
            op: ast::Operation::Addition,
        }
    }

    fn sub(expression_a: ast::Expression, expression_b: ast::Expression) -> ast::Expression {
        ast::Expression::BinaryOp {
            left: Box::new(expression_a),
            right: Box::new(expression_b),
            op: ast::Operation::Substraction,
        }
    }

    fn if_then_else(
        condition: ast::Expression,
        then_expression: ast::Expression,
        else_expression: ast::Expression,
    ) -> ast::Expression {
        ast::Expression::If {
            condition: Box::new(condition),
            then_expression: Box::new(then_expression),
            else_expression: Box::new(else_expression),
        }
    }

    #[test]
    fn test_parse_addition() {
        assert_eq!(
            parse(tokenize("1+1").unwrap()).unwrap(),
            add(int(1), int(1))
        );
    }

    #[test]
    fn test_parse_substraction() {
        assert_eq!(
            parse(tokenize("1-1").unwrap()).unwrap(),
            sub(int(1), int(1))
        );
    }

    #[test]
    fn test_parse_invalid() {
        assert!(
            parse(tokenize("").unwrap())
                .unwrap_err()
                .contains("expected an integer, identifier or '('"),
        );
        assert!(
            parse(tokenize("a+b c").unwrap())
                .unwrap_err()
                .contains("unexpected token")
        );
    }

    #[test]
    fn test_parse_associativity() {
        assert_eq!(
            parse(tokenize("1-2+3").unwrap()).unwrap(),
            add(sub(int(1), int(2)), int(3))
        );
    }

    #[test]
    fn test_parse_paranthesis() {
        assert_eq!(
            parse(tokenize("1-(2+3)").unwrap()).unwrap(),
            sub(int(1), add(int(2), int(3)))
        );
        assert_eq!(
            parse(tokenize("(1-(2+2))+1").unwrap()).unwrap(),
            add(sub(int(1), add(int(2), int(2))), int(1))
        );
    }

    #[test]
    fn test_parse_if_statement() {
        assert_eq!(
            parse(tokenize("if true then 1 else 0").unwrap()).unwrap(),
            if_then_else(bool(true), int(1), int(0))
        );
    }
}
