use crate::compiler::{
    common::Location,
    tokens::{Token, TokenKind},
};
use regex::Regex;

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, String> {
    let keyword = Regex::new(r"^(if|else|while|then|do|not|var)\b").unwrap();
    let int_literal = Regex::new(r"^[0-9]+").unwrap();
    let bool_literal = Regex::new(r"^(true|false)\b").unwrap();
    let operator = Regex::new(r"^(or\b|and\b|==|!=|<=|>=|[-<>+*/%=])").unwrap();
    let identifier = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    let punctuation = Regex::new(r"^[;,\(\){}]").unwrap();

    let comment = Regex::new(r"^(//|\#)[^\n]*").unwrap();
    let multi_line_comment = Regex::new(r"^/\*[\s\S]*\*/").unwrap();
    let whitespace = Regex::new(r"^\s+").unwrap();

    let null_patterns = vec![comment, multi_line_comment, whitespace];
    let token_patterns = vec![
        (TokenKind::Keyword, keyword),
        (TokenKind::IntLiteral, int_literal),
        (TokenKind::BoolLiteral, bool_literal),
        (TokenKind::Operator, operator),
        (TokenKind::Identifier, identifier),
        (TokenKind::Punctuation, punctuation),
    ];

    let mut remaining_code = source_code;
    let mut tokens = Vec::new();

    while !remaining_code.is_empty() {
        let mut null_match_found = false;
        for pattern in &null_patterns {
            if let Some(matched) = pattern.find(remaining_code) {
                remaining_code = &remaining_code[matched.end()..];
                null_match_found = true;
                break;
            }
        }
        if null_match_found {
            continue;
        }

        let mut token_match_found = false;
        for (token_kind, pattern) in &token_patterns {
            if let Some(matched) = pattern.find(remaining_code) {
                let token = Token {
                    kind: token_kind.clone(),
                    text: matched.as_str().to_string(),
                    loc: Location { line: 0, column: 0 },
                };
                tokens.push(token);
                remaining_code = &remaining_code[matched.end()..];
                token_match_found = true;
                break;
            }
        }

        if !token_match_found {
            return Err("No match found".to_string());
        }
    }

    tokens.push(Token {
        kind: TokenKind::End,
        text: String::new(),
        loc: tokens
            .last()
            .map(|t| t.loc.clone())
            .unwrap_or(Location { line: 1, column: 1 }),
    });
    Ok(tokens)
}

#[cfg(test)]
pub mod tests {
    use super::*;
    use crate::compiler::common;

    pub fn loc() -> common::Location {
        common::Location { line: 0, column: 0 }
    }

    fn tokenize_without_loc(source_code: &str) -> Vec<Token> {
        let mut tokens = tokenize(source_code).unwrap();
        tokens.pop();
        tokens
            .into_iter()
            .map(|t| Token {
                kind: t.kind,
                text: t.text,
                loc: loc(),
            })
            .collect()
    }

    pub fn tbool(value: &str) -> Token {
        Token {
            kind: TokenKind::BoolLiteral,
            text: value.into(),
            loc: loc(),
        }
    }

    pub fn tint(value: &str) -> Token {
        Token {
            kind: TokenKind::IntLiteral,
            text: value.into(),
            loc: loc(),
        }
    }

    pub fn tide(value: &str) -> Token {
        Token {
            kind: TokenKind::Identifier,
            text: value.into(),
            loc: loc(),
        }
    }

    pub fn tkeyw(value: &str) -> Token {
        Token {
            kind: TokenKind::Keyword,
            text: value.into(),
            loc: loc(),
        }
    }

    pub fn tope(value: &str) -> Token {
        Token {
            kind: TokenKind::Operator,
            text: value.into(),
            loc: loc(),
        }
    }

    pub fn tpunc(value: &str) -> Token {
        Token {
            kind: TokenKind::Punctuation,
            text: value.into(),
            loc: loc(),
        }
    }

    pub fn tend() -> Token {
        Token {
            kind: TokenKind::End,
            text: "".into(),
            loc: loc(),
        }
    }

    #[test]
    fn test_tokenizer_basics() {
        assert_eq!(
            tokenize_without_loc("if  3\nwhile"),
            vec![tkeyw("if"), tint("3"), tkeyw("while")]
        );
        assert_eq!(
            tokenize_without_loc("if true then 1 else 0"),
            vec![
                tkeyw("if"),
                tbool("true"),
                tkeyw("then"),
                tint("1"),
                tkeyw("else"),
                tint("0"),
            ]
        );
    }

    #[test]
    fn test_tokenizer_valid_identifiers() {
        assert_eq!(tokenize_without_loc("hello"), vec![tide("hello")]);
        assert_eq!(tokenize_without_loc("HELLO"), vec![tide("HELLO")]);
        assert_eq!(tokenize_without_loc("hEl_LO"), vec![tide("hEl_LO")]);
        assert_eq!(tokenize_without_loc("__hello"), vec![tide("__hello")]);
        assert_eq!(tokenize_without_loc("_hEl_LO"), vec![tide("_hEl_LO")]);
        assert_eq!(tokenize_without_loc("_hE12lLO"), vec![tide("_hE12lLO")]);
    }

    #[test]
    fn test_tokenizer_literals() {
        assert_eq!(tokenize_without_loc("0"), vec![tint("0")]);
        assert_eq!(tokenize_without_loc("123"), vec![tint("123")]);
        assert_eq!(
            tokenize_without_loc("123 123  11"),
            vec![tint("123"), tint("123"), tint("11")]
        );
        assert_eq!(
            tokenize_without_loc("000 123 123  11"),
            vec![tint("000"), tint("123"), tint("123"), tint("11")]
        );
    }

    #[test]
    fn test_tokenizer_whitespace() {
        assert_eq!(
            tokenize_without_loc("moi  miten\n menee\t sulla"),
            vec![tide("moi"), tide("miten"), tide("menee"), tide("sulla")]
        );
    }

    #[test]
    fn test_tokenizer_invalid_characters() {
        assert!(tokenize("@").is_err());
        assert!(tokenize("pete@gmail.fi").is_err());
        assert!(tokenize("^hi$").is_err());
        assert!(tokenize("hi$").is_err());
    }

    #[test]
    fn test_tokenizer_operators() {
        assert_eq!(tokenize_without_loc("+"), vec![tope("+")]);
        assert_eq!(tokenize_without_loc("<="), vec![tope("<=")]);
        assert_eq!(tokenize_without_loc("+-"), vec![tope("+"), tope("-")]);
        assert_eq!(tokenize_without_loc("<<"), vec![tope("<"), tope("<")]);
        assert_eq!(
            tokenize_without_loc("+-<<=>="),
            vec![tope("+"), tope("-"), tope("<"), tope("<="), tope(">=")]
        );
        assert_eq!(
            tokenize_without_loc("1+1=2"),
            vec![tint("1"), tope("+"), tint("1"), tope("="), tint("2")]
        );
        assert_eq!(
            tokenize_without_loc("true or false"),
            vec![tbool("true"), tope("or"), tbool("false")]
        );
    }

    #[test]
    fn test_tokenizer_punctuation() {
        assert_eq!(tokenize_without_loc(","), vec![tpunc(",")]);
        assert_eq!(tokenize_without_loc(";;"), vec![tpunc(";"), tpunc(";")]);
        assert_eq!(tokenize_without_loc("{}"), vec![tpunc("{"), tpunc("}")]);
        assert_eq!(tokenize_without_loc("()"), vec![tpunc("("), tpunc(")")]);
        assert_eq!(
            tokenize_without_loc("(){,;}"),
            vec![
                tpunc("("),
                tpunc(")"),
                tpunc("{"),
                tpunc(","),
                tpunc(";"),
                tpunc("}")
            ]
        );
        assert_eq!(
            tokenize_without_loc("1+1=2"),
            vec![tint("1"), tope("+"), tint("1"), tope("="), tint("2")]
        );
    }

    #[test]
    fn test_tokenizer_comments() {
        assert_eq!(tokenize_without_loc("# hello"), vec![]);
        assert_eq!(tokenize_without_loc("// hello"), vec![]);
        assert_eq!(tokenize_without_loc("/* hello */"), vec![]);
        assert_eq!(
            tokenize_without_loc("hello # hello hello"),
            vec![tide("hello")]
        );
        assert_eq!(
            tokenize_without_loc("hello # hello hello\n 1"),
            vec![tide("hello"), tint("1")]
        );
        assert_eq!(
            tokenize_without_loc("hello 1 # this is a comment\n a=2 // comment # comment\n 2"),
            vec![
                tide("hello"),
                tint("1"),
                tide("a"),
                tope("="),
                tint("2"),
                tint("2")
            ]
        );
        assert_eq!(
            tokenize_without_loc("hello /* hello\n hi=1 */ 1"),
            vec![tide("hello"), tint("1")]
        );
    }

    #[test]
    fn test_tokenizer_mixed() {
        assert_eq!(
            tokenize_without_loc(
                "moi1 123  hello 23 21 _var1 //this is a comment\n if 23 == 23 /* this\n is\n a multi-line comment */ 1+a=32"
            ),
            vec![
                tide("moi1"),
                tint("123"),
                tide("hello"),
                tint("23"),
                tint("21"),
                tide("_var1"),
                tkeyw("if"),
                tint("23"),
                tope("=="),
                tint("23"),
                tint("1"),
                tope("+"),
                tide("a"),
                tope("="),
                tint("32")
            ]
        );
        assert_eq!(
            tokenize_without_loc(
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
            ),
            vec![
                tpunc("{"),
                tkeyw("while"),
                tide("f"),
                tpunc("("),
                tpunc(")"),
                tkeyw("do"),
                tpunc("{"),
                tide("x"),
                tope("="),
                tint("10"),
                tpunc(";"),
                tide("y"),
                tope("="),
                tkeyw("if"),
                tide("g"),
                tpunc("("),
                tide("x"),
                tpunc(")"),
                tkeyw("then"),
                tpunc("{"),
                tide("x"),
                tope("="),
                tide("x"),
                tope("+"),
                tint("1"),
                tpunc(";"),
                tide("x"),
                tpunc("}"),
                tkeyw("else"),
                tpunc("{"),
                tide("g"),
                tpunc("("),
                tide("x"),
                tpunc(")"),
                tpunc("}"),
                tpunc(";"),
                tide("g"),
                tpunc("("),
                tide("y"),
                tpunc(")"),
                tpunc("}"),
                tpunc(";"),
                tint("123"),
                tpunc("}")
            ]
        );
    }
}
