use crate::compiler::{ast, tokenizer};

struct Parser {
    tokens: Vec<tokenizer::Token>,
    pos: usize,
    binary_operators: Vec<Vec<String>>,
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
            return Err(format!(
                "{:?}: expected {:?} got {:?}",
                token.loc, token_kind, token.kind
            ));
        }

        if expected.is_some_and(|value| token.text != value) {
            return Err(format!(
                "{:?}: expected '{:?}' got '{:?}'",
                token.loc,
                expected.unwrap(),
                token.text
            ));
        }

        self.pos += 1;
        if self.pos >= self.tokens.len() {
            self.pos = self.tokens.len() - 1;
        }

        Ok(token)
    }

    fn parse(&mut self) -> Result<ast::Expression, String> {
        let mut expressions = vec![];

        while self.peek().kind != tokenizer::TokenKind::End {
            if self.peek().kind == tokenizer::TokenKind::Keyword
                && self.peek().text.as_str() == "var"
            {
                expressions.push(self.parse_var_declaration()?);
            } else {
                expressions.push(self.parse_expression()?);
            }

            if self.peek().text != ";" {
                break;
            } else {
                self.consume(tokenizer::TokenKind::Punctuation, Some(";"))?;
            }
        }

        if self.peek().kind != tokenizer::TokenKind::End {
            return Err(format!(
                "{:?}: expected End got {:?}",
                self.peek().loc,
                self.peek().kind
            ));
        }

        if expressions.len() == 1 {
            return Ok(expressions.into_iter().next().unwrap());
        } else {
            return Ok(ast::Expression::Block { expressions });
        }
    }

    fn parse_var_declaration(&mut self) -> Result<ast::Expression, String> {
        self.consume(tokenizer::TokenKind::Keyword, Some("var"))?;
        let name = self
            .consume(tokenizer::TokenKind::Identifier, None)?
            .text
            .clone();
        self.consume(tokenizer::TokenKind::Operator, Some("="))?;
        let value = self.parse_expression()?;

        Ok(ast::Expression::VarDeclaration {
            name,
            value: Box::new(value),
        })
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, String> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<ast::Expression, String> {
        let left = self.parse_binary_operation(0)?;
        if self.peek().kind == tokenizer::TokenKind::Operator && self.peek().text.as_str() == "=" {
            self.consume(tokenizer::TokenKind::Operator, Some("="))?;
            let right = self.parse_assignment()?;

            return Ok(ast::Expression::Assignment {
                left: Box::new(left),
                right: Box::new(right),
            });
        }
        Ok(left)
    }

    fn parse_binary_operation(&mut self, level: usize) -> Result<ast::Expression, String> {
        if level >= self.binary_operators.len() {
            return self.parse_unary();
        }

        let operators = self.binary_operators[level].clone();

        let mut left = self.parse_binary_operation(level + 1)?;

        while self.peek().kind == tokenizer::TokenKind::Operator
            && operators.contains(&self.peek().text)
        {
            let operation = self.parse_operation()?;
            let right = self.parse_binary_operation(level + 1)?;

            left = ast::Expression::BinaryOp {
                left: Box::new(left),
                right: Box::new(right),
                op: operation,
            }
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<ast::Expression, String> {
        if matches!(self.peek().text.as_str(), "not" | "-") {
            let operation = self.parse_unary_operation()?;
            let operand = self.parse_unary()?;

            return Ok(ast::Expression::UnaryOp {
                operand: Box::new(operand),
                op: operation,
            });
        }
        self.parse_primary()
    }

    fn parse_primary(&mut self) -> Result<ast::Expression, String> {
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
            tokenizer::TokenKind::Punctuation if self.peek().text.as_str() == "{" => {
                return self.parse_block();
            }
            tokenizer::TokenKind::Keyword if self.peek().text.as_str() == "while" => {
                return self.parse_while();
            }
            _ => {
                return Err(format!(
                    "{:?}: expected a literal, identifier, '(', 'if', '{{' or 'while' got {:?}",
                    self.peek().loc,
                    self.peek().text
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
        let condition = self.parse_expression()?;
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

    fn parse_block(&mut self) -> Result<ast::Expression, String> {
        self.consume(tokenizer::TokenKind::Punctuation, Some("{"))?;
        let mut expressions: Vec<ast::Expression> = vec![];
        loop {
            if self.peek().kind == tokenizer::TokenKind::Keyword
                && self.peek().text.as_str() == "var"
            {
                expressions.push(self.parse_var_declaration()?);
            } else {
                expressions.push(self.parse_expression()?);
            }
            if self.peek().text != ";" {
                break;
            } else {
                self.consume(tokenizer::TokenKind::Punctuation, Some(";"))?;
                if self.peek().text.as_str() == "}" {
                    expressions.push(ast::Expression::NoneLiteral);
                    break;
                }
            }
        }
        self.consume(tokenizer::TokenKind::Punctuation, Some("}"))?;

        Ok(ast::Expression::Block { expressions })
    }

    fn parse_while(&mut self) -> Result<ast::Expression, String> {
        self.consume(tokenizer::TokenKind::Keyword, Some("while"))?;
        let condition = self.parse_expression()?;
        self.consume(tokenizer::TokenKind::Keyword, Some("do"))?;
        let do_expression = self.parse_expression()?;

        Ok(ast::Expression::While {
            condition: Box::new(condition),
            do_expression: Box::new(do_expression),
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

    fn parse_operation(&mut self) -> Result<ast::Operation, String> {
        let token = self.consume(tokenizer::TokenKind::Operator, None)?;
        match token.text.as_str() {
            "+" => return Ok(ast::Operation::Addition),
            "-" => return Ok(ast::Operation::Substraction),
            "*" => return Ok(ast::Operation::Multiplication),
            "/" => return Ok(ast::Operation::Division),
            "%" => return Ok(ast::Operation::Modulo),
            "<" => return Ok(ast::Operation::LessThan),
            ">" => return Ok(ast::Operation::GreaterThan),
            "==" => return Ok(ast::Operation::Equal),
            "!=" => return Ok(ast::Operation::NotEqual),
            "<=" => return Ok(ast::Operation::LessThanOrEqual),
            ">=" => return Ok(ast::Operation::GreaterThanOrEqual),
            "or" => return Ok(ast::Operation::Or),
            "and" => return Ok(ast::Operation::And),
            _ => {
                return Err(format!("{:?}: expected an operator", token.loc));
            }
        }
    }

    fn parse_unary_operation(&mut self) -> Result<ast::UnaryOperation, String> {
        let token = self.consume(self.peek().kind.clone(), None)?;

        match token.text.as_str() {
            "-" => return Ok(ast::UnaryOperation::Neg),
            "not" => return Ok(ast::UnaryOperation::Not),
            _ => {
                return Err(format!("{:?}: expected '-' or 'not'", token.loc));
            }
        }
    }
}

pub fn parse(tokens: Vec<tokenizer::Token>) -> Result<ast::Expression, String> {
    let binary_operators = vec![
        vec!["or"],
        vec!["and"],
        vec!["==", "!="],
        vec!["<", "<=", ">", ">="],
        vec!["+", "-"],
        vec!["*", "/", "%"],
    ]
    .iter()
    .map(|row| row.iter().map(|&s| s.to_string()).collect())
    .collect();

    let mut parser = Parser {
        tokens,
        pos: 0,
        binary_operators,
    };

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

    fn none() -> ast::Expression {
        ast::Expression::NoneLiteral
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

    fn and(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        binaryop(left, right, ast::Operation::And)
    }

    fn or(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        binaryop(left, right, ast::Operation::Or)
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

    fn assignment(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        ast::Expression::Assignment {
            left: Box::new(left),
            right: Box::new(right),
        }
    }

    fn neg(operand: ast::Expression) -> ast::Expression {
        unaryop(operand, ast::UnaryOperation::Neg)
    }

    fn not(operand: ast::Expression) -> ast::Expression {
        unaryop(operand, ast::UnaryOperation::Not)
    }

    fn unaryop(operand: ast::Expression, op: ast::UnaryOperation) -> ast::Expression {
        ast::Expression::UnaryOp {
            operand: Box::new(operand),
            op,
        }
    }

    fn block(expressions: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression::Block { expressions }
    }

    fn while_do(condition: ast::Expression, do_expression: ast::Expression) -> ast::Expression {
        ast::Expression::While {
            condition: Box::new(condition),
            do_expression: Box::new(do_expression),
        }
    }

    fn var_decl(name: &str, value: ast::Expression) -> ast::Expression {
        ast::Expression::VarDeclaration {
            name: name.to_string(),
            value: Box::new(value),
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
        assert_eq!(parse(tokenize("").unwrap()).unwrap(), block(vec![]));
        assert!(
            parse(tokenize("a+b c").unwrap())
                .unwrap_err()
                .contains("expected End got Identifier")
        );
        assert!(
            parse(tokenize("{ 1 a }").unwrap())
                .unwrap_err()
                .contains("expected Punctuation got Identifier")
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

        assert_eq!(
            parse(tokenize("if true and false then a else 3").unwrap()).unwrap(),
            if_then_else(and(bool(true), bool(false)), ide("a"), Some(int(3)))
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

    #[test]
    fn test_parser_assignment() {
        assert_eq!(
            parse(tokenize("a = 3").unwrap()).unwrap(),
            assignment(ide("a"), int(3))
        );
        assert_eq!(
            parse(tokenize("hello = a+3").unwrap()).unwrap(),
            assignment(ide("hello"), add(ide("a"), int(3)))
        );
        assert_eq!(
            parse(tokenize("a = b = c").unwrap()).unwrap(),
            assignment(ide("a"), assignment(ide("b"), ide("c")))
        );
        assert_eq!(
            parse(tokenize("f(a+b = b-c)").unwrap()).unwrap(),
            function_call(
                "f",
                vec![assignment(add(ide("a"), ide("b")), sub(ide("b"), ide("c")))]
            )
        );
    }

    #[test]
    fn test_parser_unary() {
        assert_eq!(
            parse(tokenize("not true").unwrap()).unwrap(),
            not(bool(true))
        );
        assert_eq!(
            parse(tokenize("not not a").unwrap()).unwrap(),
            not(not(ide("a")))
        );
        assert_eq!(
            parse(tokenize("-a + b").unwrap()).unwrap(),
            add(neg(ide("a")), ide("b"))
        );
        assert_eq!(
            parse(tokenize("(not not a and b and c) or (not b)").unwrap()).unwrap(),
            or(
                and(and(not(not(ide("a"))), ide("b")), ide("c")),
                not(ide("b"))
            )
        );
    }

    #[test]
    fn test_parser_block() {
        assert_eq!(
            parse(tokenize("{ 1 }").unwrap()).unwrap(),
            block(vec![int(1)])
        );
        assert_eq!(
            parse(tokenize("{ 1; 2 }").unwrap()).unwrap(),
            block(vec![int(1), int(2)])
        );
        assert_eq!(
            parse(tokenize("{ 1; }").unwrap()).unwrap(),
            block(vec![int(1), none()])
        );
        assert_eq!(
            parse(tokenize("{ 1; 2; 3 }").unwrap()).unwrap(),
            block(vec![int(1), int(2), int(3)])
        );
        assert_eq!(
            parse(tokenize("{ a = 1; b = 2; a + b }").unwrap()).unwrap(),
            block(vec![
                assignment(ide("a"), int(1)),
                assignment(ide("b"), int(2)),
                add(ide("a"), ide("b"))
            ])
        );
        assert_eq!(
            parse(tokenize("{ { 1 } }").unwrap()).unwrap(),
            block(vec![block(vec![int(1)])])
        );
        assert_eq!(
            parse(tokenize("1 + { 2 }").unwrap()).unwrap(),
            add(int(1), block(vec![int(2)]))
        );
        assert_eq!(
            parse(tokenize("x = { f(a); b }").unwrap()).unwrap(),
            assignment(
                ide("x"),
                block(vec![function_call("f", vec![ide("a")]), ide("b")])
            )
        );
        assert_eq!(
            parse(
                tokenize(
                    "{
                        while f() do {
                            x = 10;
                            y = if g(x) then {
                                x = x + 1;
                                x
                            } else {
                                g(x)
                            };
                            g(y)
                        };
                        123
                    }"
                )
                .unwrap()
            )
            .unwrap(),
            block(vec![
                while_do(
                    function_call("f", vec![]),
                    block(vec![
                        assignment(ide("x"), int(10)),
                        assignment(
                            ide("y"),
                            if_then_else(
                                function_call("g", vec![ide("x")]),
                                block(vec![assignment(ide("x"), add(ide("x"), int(1))), ide("x")]),
                                Some(block(vec![function_call("g", vec![ide("x")])]))
                            )
                        ),
                        function_call("g", vec![ide("y")])
                    ])
                ),
                int(123)
            ])
        );
    }

    #[test]
    fn test_parser_var_declaration() {
        assert_eq!(
            parse(tokenize("var a = 1").unwrap()).unwrap(),
            var_decl("a", int(1))
        );
        assert_eq!(
            parse(tokenize("var x = 1 + 2").unwrap()).unwrap(),
            var_decl("x", add(int(1), int(2)))
        );
        assert_eq!(
            parse(tokenize("var a = 1; var b = 2").unwrap()).unwrap(),
            block(vec![var_decl("a", int(1)), var_decl("b", int(2))])
        );
        assert_eq!(
            parse(tokenize("{ var a = 1 }").unwrap()).unwrap(),
            block(vec![var_decl("a", int(1))])
        );
        assert_eq!(
            parse(tokenize("{ var a = { var b = 1; b } }").unwrap()).unwrap(),
            block(vec![var_decl("a", block(vec![var_decl("b", int(1)), ide("b")]))])
        );

        // Invalid
        assert!(
            parse(tokenize("if true then var a = 1").unwrap())
                .unwrap_err()
                .contains("expected")
        );
        assert!(
            parse(tokenize("if true then var a = 1 else 2").unwrap())
                .unwrap_err()
                .contains("expected")
        );
        assert!(
            parse(tokenize("f(var a = 1)").unwrap())
                .unwrap_err()
                .contains("expected")
        );
    }
}
