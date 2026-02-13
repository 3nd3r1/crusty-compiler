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
        let loc = self.peek().loc.clone();
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
                if self.ends_with_block(expressions.last().unwrap()) {
                    continue;
                } else {
                    break;
                }
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
            return Ok(ast::Expression {
                loc,
                kind: ast::ExpressionKind::Block { expressions },
            });
        }
    }

    fn parse_var_declaration(&mut self) -> Result<ast::Expression, String> {
        let loc = self
            .consume(tokenizer::TokenKind::Keyword, Some("var"))?
            .loc
            .clone();
        let name = self
            .consume(tokenizer::TokenKind::Identifier, None)?
            .text
            .clone();
        self.consume(tokenizer::TokenKind::Operator, Some("="))?;
        let value = self.parse_expression()?;

        Ok(ast::Expression {
            loc,
            kind: ast::ExpressionKind::VarDeclaration {
                name,
                value: Box::new(value),
            },
        })
    }

    fn parse_expression(&mut self) -> Result<ast::Expression, String> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<ast::Expression, String> {
        let left = self.parse_binary_operation(0)?;
        if self.peek().kind == tokenizer::TokenKind::Operator && self.peek().text.as_str() == "=" {
            let loc = self
                .consume(tokenizer::TokenKind::Operator, Some("="))?
                .loc
                .clone();
            let right = self.parse_assignment()?;

            return Ok(ast::Expression {
                loc,
                kind: ast::ExpressionKind::Assignment {
                    left: Box::new(left),
                    right: Box::new(right),
                },
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
            let loc = self.peek().loc.clone();
            let operation = self.parse_operation()?;
            let right = self.parse_binary_operation(level + 1)?;

            left = ast::Expression {
                loc,
                kind: ast::ExpressionKind::BinaryOp {
                    left: Box::new(left),
                    right: Box::new(right),
                    op: operation,
                },
            }
        }

        Ok(left)
    }

    fn parse_unary(&mut self) -> Result<ast::Expression, String> {
        if matches!(self.peek().text.as_str(), "not" | "-") {
            let loc = self.peek().loc.clone();
            let operation = self.parse_unary_operation()?;
            let operand = self.parse_unary()?;

            return Ok(ast::Expression {
                loc,
                kind: ast::ExpressionKind::UnaryOp {
                    operand: Box::new(operand),
                    op: operation,
                },
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
        let loc = token.loc.clone();
        Ok(ast::Expression {
            loc,
            kind: ast::ExpressionKind::IntLiteral {
                value: token
                    .text
                    .parse::<i32>()
                    .map_err(|_| format!("{:?}: invalid number", token.loc))?,
            },
        })
    }

    fn parse_identifier(&mut self) -> Result<ast::Expression, String> {
        let token = self.consume(tokenizer::TokenKind::Identifier, None)?;
        let loc = token.loc.clone();
        let name = token.text.clone();

        if self.peek().kind == tokenizer::TokenKind::Punctuation && self.peek().text.as_str() == "("
        {
            return self.parse_function_call(name);
        }

        Ok(ast::Expression {
            loc,
            kind: ast::ExpressionKind::Identifier { value: name },
        })
    }

    fn parse_bool_literal(&mut self) -> Result<ast::Expression, String> {
        let token = self.consume(tokenizer::TokenKind::BoolLiteral, None)?;
        let loc = token.loc.clone();
        Ok(ast::Expression {
            loc,
            kind: ast::ExpressionKind::BoolLiteral {
                value: token
                    .text
                    .parse::<bool>()
                    .map_err(|_| format!("{:?}: invalid boolean", token.loc))?,
            },
        })
    }

    fn parse_parenthesized(&mut self) -> Result<ast::Expression, String> {
        self.consume(tokenizer::TokenKind::Punctuation, Some("("))?;
        let expression = self.parse_expression()?;
        self.consume(tokenizer::TokenKind::Punctuation, Some(")"))?;

        Ok(expression)
    }

    fn parse_if(&mut self) -> Result<ast::Expression, String> {
        let loc = self
            .consume(tokenizer::TokenKind::Keyword, Some("if"))?
            .loc
            .clone();
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

        Ok(ast::Expression {
            loc,
            kind: ast::ExpressionKind::If {
                condition: Box::new(condition),
                then_expression: Box::new(then_expression),
                else_expression: else_expression.map(Box::new),
            },
        })
    }

    fn parse_block(&mut self) -> Result<ast::Expression, String> {
        let loc = self
            .consume(tokenizer::TokenKind::Punctuation, Some("{"))?
            .loc
            .clone();
        let mut expressions = vec![];
        while self.peek().text.as_str() != "}" {
            if self.peek().kind == tokenizer::TokenKind::Keyword
                && self.peek().text.as_str() == "var"
            {
                expressions.push(self.parse_var_declaration()?);
            } else {
                expressions.push(self.parse_expression()?);
            }

            if self.peek().text != ";" {
                if self.ends_with_block(expressions.last().unwrap()) {
                    continue;
                } else {
                    break;
                }
            } else {
                self.consume(tokenizer::TokenKind::Punctuation, Some(";"))?;
                if self.peek().text.as_str() == "}" {
                    let none_loc = self.peek().loc.clone();
                    expressions.push(ast::Expression {
                        loc: none_loc,
                        kind: ast::ExpressionKind::NoneLiteral,
                    });
                    break;
                }
            }
        }
        self.consume(tokenizer::TokenKind::Punctuation, Some("}"))?;

        Ok(ast::Expression {
            loc,
            kind: ast::ExpressionKind::Block { expressions },
        })
    }

    fn parse_while(&mut self) -> Result<ast::Expression, String> {
        let loc = self
            .consume(tokenizer::TokenKind::Keyword, Some("while"))?
            .loc
            .clone();
        let condition = self.parse_expression()?;
        self.consume(tokenizer::TokenKind::Keyword, Some("do"))?;
        let do_expression = self.parse_expression()?;

        Ok(ast::Expression {
            loc,
            kind: ast::ExpressionKind::While {
                condition: Box::new(condition),
                do_expression: Box::new(do_expression),
            },
        })
    }

    fn parse_function_call(&mut self, name: String) -> Result<ast::Expression, String> {
        let loc = self
            .consume(tokenizer::TokenKind::Punctuation, Some("("))?
            .loc
            .clone();

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

        Ok(ast::Expression {
            loc,
            kind: ast::ExpressionKind::FunctionCall { name, arguments },
        })
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

    fn ends_with_block(&self, expression: &ast::Expression) -> bool {
        match &expression.kind {
            ast::ExpressionKind::Block { .. } => true,
            ast::ExpressionKind::If {
                then_expression,
                else_expression,
                ..
            } => {
                if let Some(else_expression) = else_expression {
                    matches!(else_expression.kind, ast::ExpressionKind::Block { .. })
                } else {
                    matches!(then_expression.kind, ast::ExpressionKind::Block { .. })
                }
            }
            ast::ExpressionKind::While { do_expression, .. } => {
                matches!(do_expression.kind, ast::ExpressionKind::Block { .. })
            }
            _ => false,
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
pub mod tests {
    use super::*;
    use crate::compiler::tokenizer::tests::*;

    pub fn eint(value: i32) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::IntLiteral { value },
        }
    }

    pub fn ebool(value: bool) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::BoolLiteral { value },
        }
    }

    pub fn eide(value: &str) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::Identifier {
                value: value.to_string(),
            },
        }
    }

    pub fn enone() -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::NoneLiteral,
        }
    }

    pub fn eadd(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        ebinaryop(left, right, ast::Operation::Addition)
    }

    pub fn esub(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        ebinaryop(left, right, ast::Operation::Substraction)
    }

    pub fn emul(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        ebinaryop(left, right, ast::Operation::Multiplication)
    }

    pub fn elt(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        ebinaryop(left, right, ast::Operation::LessThan)
    }

    pub fn eand(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        ebinaryop(left, right, ast::Operation::And)
    }

    pub fn eor(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        ebinaryop(left, right, ast::Operation::Or)
    }

    pub fn ebinaryop(
        left: ast::Expression,
        right: ast::Expression,
        op: ast::Operation,
    ) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::BinaryOp {
                left: Box::new(left),
                right: Box::new(right),
                op,
            },
        }
    }

    pub fn eif(
        condition: ast::Expression,
        then_expression: ast::Expression,
        else_expression: Option<ast::Expression>,
    ) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::If {
                condition: Box::new(condition),
                then_expression: Box::new(then_expression),
                else_expression: else_expression.map(Box::new),
            },
        }
    }

    pub fn ecall(name: &str, arguments: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::FunctionCall {
                name: name.to_string(),
                arguments,
            },
        }
    }

    pub fn eassign(left: ast::Expression, right: ast::Expression) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::Assignment {
                left: Box::new(left),
                right: Box::new(right),
            },
        }
    }

    pub fn eneg(operand: ast::Expression) -> ast::Expression {
        eunaryop(operand, ast::UnaryOperation::Neg)
    }

    pub fn enot(operand: ast::Expression) -> ast::Expression {
        eunaryop(operand, ast::UnaryOperation::Not)
    }

    pub fn eunaryop(operand: ast::Expression, op: ast::UnaryOperation) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::UnaryOp {
                operand: Box::new(operand),
                op,
            },
        }
    }

    pub fn eblock(expressions: Vec<ast::Expression>) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::Block { expressions },
        }
    }

    pub fn ewhile(condition: ast::Expression, do_expression: ast::Expression) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::While {
                condition: Box::new(condition),
                do_expression: Box::new(do_expression),
            },
        }
    }

    pub fn evar(name: &str, value: ast::Expression) -> ast::Expression {
        ast::Expression {
            loc: loc(),
            kind: ast::ExpressionKind::VarDeclaration {
                name: name.to_string(),
                value: Box::new(value),
            },
        }
    }

    #[test]
    fn test_parser_addition() {
        assert_eq!(
            parse(vec![tint("1"), tope("+"), tint("1"), tend()]).unwrap(),
            eadd(eint(1), eint(1))
        );
    }

    #[test]
    fn test_parser_substraction() {
        assert_eq!(
            parse(vec![tint("1"), tope("-"), tint("1"), tend()]).unwrap(),
            esub(eint(1), eint(1))
        );
    }

    #[test]
    fn test_parser_comparison() {
        assert_eq!(
            parse(vec![tide("a"), tope("<"), tint("2"), tend()]).unwrap(),
            elt(eide("a"), eint(2))
        );
    }

    #[test]
    fn test_parser_invalid() {
        assert_eq!(parse(vec![tend()]).unwrap(), eblock(vec![]));
        assert!(
            parse(vec![tide("a"), tope("+"), tide("b"), tide("c"), tend()])
                .unwrap_err()
                .contains("expected End got Identifier")
        );
        assert!(
            parse(vec![tpunc("{"), tint("1"), tide("a"), tpunc("}"), tend()])
                .unwrap_err()
                .contains("expected Punctuation got Identifier")
        );
    }

    #[test]
    fn test_parser_associativity() {
        assert_eq!(
            parse(vec![
                tint("1"),
                tope("-"),
                tint("2"),
                tope("+"),
                tint("3"),
                tend()
            ])
            .unwrap(),
            eadd(esub(eint(1), eint(2)), eint(3))
        );
    }

    #[test]
    fn test_parser_paranthesis() {
        assert_eq!(
            parse(vec![
                tint("1"),
                tope("-"),
                tpunc("("),
                tint("2"),
                tope("+"),
                tint("3"),
                tpunc(")"),
                tend()
            ])
            .unwrap(),
            esub(eint(1), eadd(eint(2), eint(3)))
        );
        assert_eq!(
            parse(vec![
                tpunc("("),
                tint("1"),
                tope("-"),
                tpunc("("),
                tint("2"),
                tope("+"),
                tint("2"),
                tpunc(")"),
                tpunc(")"),
                tope("+"),
                tint("1"),
                tend()
            ])
            .unwrap(),
            eadd(esub(eint(1), eadd(eint(2), eint(2))), eint(1))
        );
    }

    #[test]
    fn test_parser_if_statement() {
        assert_eq!(
            parse(vec![
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tint("1"),
                tkeyw("else"),
                tint("0"),
                tend()
            ])
            .unwrap(),
            eif(ebool(true), eint(1), Some(eint(0)))
        );
        assert_eq!(
            parse(vec![
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tint("1"),
                tend()
            ])
            .unwrap(),
            eif(ebool(true), eint(1), None)
        );
        assert_eq!(
            parse(vec![
                tkeyw("if"),
                tide("a"),
                tkeyw("then"),
                tide("b"),
                tope("+"),
                tide("c"),
                tkeyw("else"),
                tide("x"),
                tope("*"),
                tide("y"),
                tend()
            ])
            .unwrap(),
            eif(
                eide("a"),
                eadd(eide("b"), eide("c")),
                Some(emul(eide("x"), eide("y")))
            )
        );
        assert_eq!(
            parse(vec![
                tint("1"),
                tope("+"),
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tint("2"),
                tkeyw("else"),
                tint("3"),
                tend()
            ])
            .unwrap(),
            eadd(eint(1), eif(ebool(true), eint(2), Some(eint(3))))
        );
        assert_eq!(
            parse(vec![
                tkeyw("if"),
                tbool("true"),
                tope("and"),
                tbool("false"),
                tkeyw("then"),
                tide("a"),
                tkeyw("else"),
                tint("3"),
                tend()
            ])
            .unwrap(),
            eif(eand(ebool(true), ebool(false)), eide("a"), Some(eint(3)))
        );
    }

    #[test]
    fn test_parser_function_call() {
        assert_eq!(
            parse(vec![tide("parse"), tpunc("("), tpunc(")"), tend()]).unwrap(),
            ecall("parse", vec![])
        );
        assert_eq!(
            parse(vec![
                tide("f"),
                tpunc("("),
                tide("a"),
                tpunc(","),
                tide("b"),
                tpunc(")"),
                tend()
            ])
            .unwrap(),
            ecall("f", vec![eide("a"), eide("b")])
        );
        assert_eq!(
            parse(vec![
                tide("hello"),
                tpunc("("),
                tint("1"),
                tope("+"),
                tint("2"),
                tpunc(")"),
                tend()
            ])
            .unwrap(),
            ecall("hello", vec![eadd(eint(1), eint(2))])
        );
        assert_eq!(
            parse(vec![
                tide("hello"),
                tpunc("("),
                tint("1"),
                tope("+"),
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tint("2"),
                tkeyw("else"),
                tint("3"),
                tpunc(","),
                tide("c"),
                tpunc(")"),
                tend()
            ])
            .unwrap(),
            ecall(
                "hello",
                vec![
                    eadd(eint(1), eif(ebool(true), eint(2), Some(eint(3)))),
                    eide("c")
                ]
            )
        );
    }

    #[test]
    fn test_parser_assignment() {
        assert_eq!(
            parse(vec![tide("a"), tope("="), tint("3"), tend()]).unwrap(),
            eassign(eide("a"), eint(3))
        );
        assert_eq!(
            parse(vec![
                tide("hello"),
                tope("="),
                tide("a"),
                tope("+"),
                tint("3"),
                tend()
            ])
            .unwrap(),
            eassign(eide("hello"), eadd(eide("a"), eint(3)))
        );
        assert_eq!(
            parse(vec![
                tide("a"),
                tope("="),
                tide("b"),
                tope("="),
                tide("c"),
                tend()
            ])
            .unwrap(),
            eassign(eide("a"), eassign(eide("b"), eide("c")))
        );
        assert_eq!(
            parse(vec![
                tide("f"),
                tpunc("("),
                tide("a"),
                tope("+"),
                tide("b"),
                tope("="),
                tide("b"),
                tope("-"),
                tide("c"),
                tpunc(")"),
                tend()
            ])
            .unwrap(),
            ecall(
                "f",
                vec![eassign(
                    eadd(eide("a"), eide("b")),
                    esub(eide("b"), eide("c"))
                )]
            )
        );
    }

    #[test]
    fn test_parser_unary() {
        assert_eq!(
            parse(vec![tkeyw("not"), tbool("true"), tend()]).unwrap(),
            enot(ebool(true))
        );
        assert_eq!(
            parse(vec![tkeyw("not"), tkeyw("not"), tide("a"), tend()]).unwrap(),
            enot(enot(eide("a")))
        );
        assert_eq!(
            parse(vec![tope("-"), tide("a"), tope("+"), tide("b"), tend()]).unwrap(),
            eadd(eneg(eide("a")), eide("b"))
        );
        assert_eq!(
            parse(vec![
                tpunc("("),
                tkeyw("not"),
                tkeyw("not"),
                tide("a"),
                tope("and"),
                tide("b"),
                tope("and"),
                tide("c"),
                tpunc(")"),
                tope("or"),
                tpunc("("),
                tkeyw("not"),
                tide("b"),
                tpunc(")"),
                tend()
            ])
            .unwrap(),
            eor(
                eand(eand(enot(enot(eide("a"))), eide("b")), eide("c")),
                enot(eide("b"))
            )
        );
    }

    #[test]
    fn test_parser_block() {
        assert_eq!(
            parse(vec![tpunc("{"), tint("1"), tpunc("}"), tend()]).unwrap(),
            eblock(vec![eint(1)])
        );
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tint("1"),
                tpunc(";"),
                tint("2"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![eint(1), eint(2)])
        );
        assert_eq!(
            parse(vec![tpunc("{"), tint("1"), tpunc(";"), tpunc("}"), tend()]).unwrap(),
            eblock(vec![eint(1), enone()])
        );
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tint("1"),
                tpunc(";"),
                tint("2"),
                tpunc(";"),
                tint("3"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![eint(1), eint(2), eint(3)])
        );
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tide("a"),
                tope("="),
                tint("1"),
                tpunc(";"),
                tide("b"),
                tope("="),
                tint("2"),
                tpunc(";"),
                tide("a"),
                tope("+"),
                tide("b"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![
                eassign(eide("a"), eint(1)),
                eassign(eide("b"), eint(2)),
                eadd(eide("a"), eide("b"))
            ])
        );
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tpunc("{"),
                tint("1"),
                tpunc("}"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![eblock(vec![eint(1)])])
        );
        assert_eq!(
            parse(vec![
                tint("1"),
                tope("+"),
                tpunc("{"),
                tint("2"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eadd(eint(1), eblock(vec![eint(2)]))
        );
        assert_eq!(
            parse(vec![
                tide("x"),
                tope("="),
                tpunc("{"),
                tide("f"),
                tpunc("("),
                tide("a"),
                tpunc(")"),
                tpunc(";"),
                tide("b"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eassign(
                eide("x"),
                eblock(vec![ecall("f", vec![eide("a")]), eide("b")])
            )
        );
    }

    #[test]
    fn test_parser_while() {
        assert_eq!(
            parse(vec![
                tkeyw("while"),
                tide("f"),
                tpunc("("),
                tpunc(")"),
                tkeyw("do"),
                tpunc("{"),
                tide("x"),
                tope("="),
                tint("10"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            ewhile(
                ecall("f", vec![]),
                eblock(vec![eassign(eide("x"), eint(10))])
            )
        );
    }

    #[test]
    fn test_parser_var_declaration() {
        assert_eq!(
            parse(vec![tkeyw("var"), tide("a"), tope("="), tint("1"), tend()]).unwrap(),
            evar("a", eint(1))
        );
        assert_eq!(
            parse(vec![
                tkeyw("var"),
                tide("x"),
                tope("="),
                tint("1"),
                tope("+"),
                tint("2"),
                tend()
            ])
            .unwrap(),
            evar("x", eadd(eint(1), eint(2)))
        );
        assert_eq!(
            parse(vec![
                tkeyw("var"),
                tide("a"),
                tope("="),
                tint("1"),
                tpunc(";"),
                tkeyw("var"),
                tide("b"),
                tope("="),
                tint("2"),
                tend()
            ])
            .unwrap(),
            eblock(vec![evar("a", eint(1)), evar("b", eint(2))])
        );
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tkeyw("var"),
                tide("a"),
                tope("="),
                tint("1"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![evar("a", eint(1))])
        );
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tkeyw("var"),
                tide("a"),
                tope("="),
                tpunc("{"),
                tkeyw("var"),
                tide("b"),
                tope("="),
                tint("1"),
                tpunc(";"),
                tide("b"),
                tpunc("}"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![evar("a", eblock(vec![evar("b", eint(1)), eide("b")]))])
        );

        // Invalid
        assert!(
            parse(vec![
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tkeyw("var"),
                tide("a"),
                tope("="),
                tint("1"),
                tend()
            ])
            .unwrap_err()
            .contains("expected")
        );
        assert!(
            parse(vec![
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tkeyw("var"),
                tide("a"),
                tope("="),
                tint("1"),
                tkeyw("else"),
                tint("2"),
                tend()
            ])
            .unwrap_err()
            .contains("expected")
        );
        assert!(
            parse(vec![
                tide("f"),
                tpunc("("),
                tkeyw("var"),
                tide("a"),
                tope("="),
                tint("1"),
                tpunc(")"),
                tend()
            ])
            .unwrap_err()
            .contains("expected")
        );
    }

    #[test]
    fn test_parser_block_semicolon() {
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tpunc("{"),
                tide("a"),
                tpunc("}"),
                tpunc("{"),
                tide("b"),
                tpunc("}"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![eblock(vec![eide("a")]), eblock(vec![eide("b")])])
        );
        assert!(parse(vec![tpunc("{"), tide("a"), tide("b"), tpunc("}")]).is_err());
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tpunc("{"),
                tide("a"),
                tpunc("}"),
                tide("b"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![
                eif(ebool(true), eblock(vec![eide("a")]), None),
                eide("b")
            ])
        );
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tpunc("{"),
                tide("a"),
                tpunc("}"),
                tpunc(";"),
                tide("b"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![
                eif(ebool(true), eblock(vec![eide("a")]), None),
                eide("b")
            ])
        );
        assert!(
            parse(vec![
                tpunc("{"),
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tpunc("{"),
                tide("a"),
                tpunc("}"),
                tide("b"),
                tide("c"),
                tpunc("}"),
                tend()
            ])
            .is_err()
        );
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tpunc("{"),
                tide("a"),
                tpunc("}"),
                tide("b"),
                tpunc(";"),
                tide("c"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![
                eif(ebool(true), eblock(vec![eide("a")]), None),
                eide("b"),
                eide("c")
            ])
        );
        assert_eq!(
            parse(vec![
                tpunc("{"),
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tpunc("{"),
                tide("a"),
                tpunc("}"),
                tkeyw("else"),
                tpunc("{"),
                tide("b"),
                tpunc("}"),
                tide("c"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eblock(vec![
                eif(
                    ebool(true),
                    eblock(vec![eide("a")]),
                    Some(eblock(vec![eide("b")]))
                ),
                eide("c")
            ])
        );
        assert_eq!(
            parse(vec![
                tide("x"),
                tope("="),
                tpunc("{"),
                tpunc("{"),
                tide("f"),
                tpunc("("),
                tide("a"),
                tpunc(")"),
                tpunc("}"),
                tpunc("{"),
                tide("b"),
                tpunc("}"),
                tpunc("}"),
                tend()
            ])
            .unwrap(),
            eassign(
                eide("x"),
                eblock(vec![
                    eblock(vec![ecall("f", vec![eide("a")])]),
                    eblock(vec![eide("b")])
                ])
            )
        );
    }
}
