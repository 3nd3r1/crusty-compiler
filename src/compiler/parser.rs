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

    fn parse(&mut self) -> Result<ast::Expression, String> {
        let expression = self.parse_comparison()?;

        if self.peek().kind != tokenizer::TokenKind::End {
            return Err(format!(
                "{:?}: unexpected token {:?} expected end",
                self.peek().loc,
                self.peek().text
            ));
        }

        Ok(expression)
    }

    fn parse_comparison(&mut self) -> Result<ast::Expression, String> {
        let mut left = self.parse_expression()?;

        while self.peek().kind == tokenizer::TokenKind::Operator
            && matches!(
                self.peek().text.as_str(),
                "<" | ">" | "==" | "!=" | "<=" | ">="
            )
        {
            let operation = self.parse_operator()?;
            let right = self.parse_expression()?;

            left = ast::Expression::BinaryOp {
                left: Box::new(left),
                right: Box::new(right),
                op: operation,
            }
        }

        Ok(left)
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
            tokenizer::TokenKind::BoolLiteral => return self.parse_bool_literal(),
            tokenizer::TokenKind::Punctuation if self.peek().text.as_str() == "(" => {
                return self.parse_parenthesized();
            }
            tokenizer::TokenKind::Keyword if self.peek().text.as_str() == "if" => {
                return self.parse_if();
            }
            _ => {
                return Err(format!(
                    "{:?}: expected a literal, identifier or '('",
                    self.peek().loc
                ));
            }
        }
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
        let name = self
            .consume(tokenizer::TokenKind::Identifier, None)?
            .text
            .clone();

        if self.peek().kind == tokenizer::TokenKind::Punctuation && self.peek().text.as_str() == "("
        {
            return self.parse_function_call(name);
        }

        Ok(ast::Expression::Identifier { value: name })
    }

    fn parse_bool_literal(&mut self) -> Result<ast::Expression, String> {
        let token = self.consume(tokenizer::TokenKind::BoolLiteral, None)?;
        Ok(ast::Expression::BoolLiteral {
            value: token
                .text
                .parse::<bool>()
                .map_err(|_| format!("{:?}: invalid boolean", token.loc))?,
        })
    }

    fn parse_parenthesized(&mut self) -> Result<ast::Expression, String> {
        self.consume(tokenizer::TokenKind::Punctuation, Some("("))?;
        let expression = self.parse_expression()?;
        self.consume(tokenizer::TokenKind::Punctuation, Some(")"))?;

        Ok(expression)
    }

    fn parse_if(&mut self) -> Result<ast::Expression, String> {
        self.consume(tokenizer::TokenKind::Keyword, Some("if"))?;
        let condition = self.parse_comparison()?;
        self.consume(tokenizer::TokenKind::Keyword, Some("then"))?;
        let then_expression = self.parse_expression()?;

        let else_expression: Option<ast::Expression> = if self.peek().kind
            == tokenizer::TokenKind::Keyword
            && self.peek().text.as_str() == "else"
        {
            self.consume(tokenizer::TokenKind::Keyword, Some("else"))?;
            Some(self.parse_expression()?)
        } else {
            None
        };

        Ok(ast::Expression::If {
            condition: Box::new(condition),
            then_expression: Box::new(then_expression),
            else_expression: else_expression.map(Box::new),
        })
    }

    fn parse_function_call(&mut self, name: String) -> Result<ast::Expression, String> {
        self.consume(tokenizer::TokenKind::Punctuation, Some("("))?;

        let mut arguments: Vec<ast::Expression> = vec![];
        if self.peek().text != ")" {
            loop {
                arguments.push(self.parse_expression()?);
                if self.peek().text != "," {
                    break;
                } else {
                    self.consume(tokenizer::TokenKind::Punctuation, Some(","))?;
                }
            }
        }
        self.consume(tokenizer::TokenKind::Punctuation, Some(")"))?;

        Ok(ast::Expression::FunctionCall { name, arguments })
    }

    fn parse_operator(&mut self) -> Result<ast::Operation, String> {
        let token = self.consume(tokenizer::TokenKind::Operator, None)?;
        match token.text.as_str() {
            "+" => return Ok(ast::Operation::Addition),
            "-" => return Ok(ast::Operation::Substraction),
            "*" => return Ok(ast::Operation::Multiplication),
            "/" => return Ok(ast::Operation::Division),
            "<" => return Ok(ast::Operation::LessThan),
            ">" => return Ok(ast::Operation::GreaterThan),
            "==" => return Ok(ast::Operation::Equal),
            "!=" => return Ok(ast::Operation::NotEqual),
            "<=" => return Ok(ast::Operation::LessThanOrEqual),
            ">=" => return Ok(ast::Operation::GreaterThanOrEqual),
            _ => {
                return Err(format!("{:?}: expected an operator", token.loc));
            }
        }
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

    fn add(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        binaryop(left, right, ast::Operation::Addition)
    }

    fn sub(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        binaryop(left, right, ast::Operation::Substraction)
    }

    fn mul(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        binaryop(left, right, ast::Operation::Multiplication)
    }

    fn lt(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        binaryop(left, right, ast::Operation::LessThan)
    }

    fn binaryop(
        left: ast::Expression,
        right: ast::Expression,
        op: ast::Operation,
    ) -> ast::Expression {
        ast::Expression::BinaryOp {
            left: Box::new(left),
            right: Box::new(right),
            op,
        }
    }

    fn if_then_else(
        condition: ast::Expression,
        then_expression: ast::Expression,
        else_expression: Option<ast::Expression>,
    ) -> ast::Expression {
        ast::Expression::If {
            condition: Box::new(condition),
            then_expression: Box::new(then_expression),
            else_expression: else_expression.map(Box::new),
        }
    }

    fn function_call(name: &str, arguments: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::FunctionCall {
            name: name.to_string(),
            arguments,
        }
    }

    #[test]
    fn test_parser_addition() {
        assert_eq!(
            parse(tokenize("1+1").unwrap()).unwrap(),
            add(int(1), int(1))
        );
    }

    #[test]
    fn test_parser_substraction() {
        assert_eq!(
            parse(tokenize("1-1").unwrap()).unwrap(),
            sub(int(1), int(1))
        );
    }

    #[test]
    fn test_parser_comparison() {
        assert_eq!(
            parse(tokenize("a<2").unwrap()).unwrap(),
            lt(ide("a"), int(2))
        );
    }

    #[test]
    fn test_parser_invalid() {
        assert!(
            parse(tokenize("").unwrap())
                .unwrap_err()
                .contains("expected a literal, identifier or '('"),
        );
        assert!(
            parse(tokenize("a+b c").unwrap())
                .unwrap_err()
                .contains("unexpected token")
        );
    }

    #[test]
    fn test_parser_associativity() {
        assert_eq!(
            parse(tokenize("1-2+3").unwrap()).unwrap(),
            add(sub(int(1), int(2)), int(3))
        );
    }

    #[test]
    fn test_parser_paranthesis() {
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
    fn test_parser_if_statement() {
        assert_eq!(
            parse(tokenize("if true then 1 else 0").unwrap()).unwrap(),
            if_then_else(bool(true), int(1), Some(int(0)))
        );
        assert_eq!(
            parse(tokenize("if true then 1").unwrap()).unwrap(),
            if_then_else(bool(true), int(1), None)
        );
        assert_eq!(
            parse(tokenize("if a then b + c else x * y").unwrap()).unwrap(),
            if_then_else(
                ide("a"),
                add(ide("b"), ide("c")),
                Some(mul(ide("x"), ide("y")))
            )
        );
        assert_eq!(
            parse(tokenize("1 + if true then 2 else 3").unwrap()).unwrap(),
            add(int(1), if_then_else(bool(true), int(2), Some(int(3))))
        );
    }

    #[test]
    fn test_parser_function_call() {
        assert_eq!(
            parse(tokenize("parse()").unwrap()).unwrap(),
            function_call("parse", vec![])
        );
        assert_eq!(
            parse(tokenize("f(a,b)").unwrap()).unwrap(),
            function_call("f", vec![ide("a"), ide("b")])
        );
        assert_eq!(
            parse(tokenize("hello(1+2)").unwrap()).unwrap(),
            function_call("hello", vec![add(int(1), int(2))])
        );
        assert_eq!(
            parse(tokenize("hello(1 + if true then 2 else 3, c)").unwrap()).unwrap(),
            function_call(
                "hello",
                vec![
                    add(int(1), if_then_else(bool(true), int(2), Some(int(3)))),
                    ide("c")
                ]
            )
        );
    }
}
