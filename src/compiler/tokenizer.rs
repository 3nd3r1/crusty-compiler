use regex::Regex;

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
pub struct TokenLocation {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokenKind,
    pub text: String,
    pub loc: TokenLocation,
}

pub fn tokenize(source_code: &str) -> Result<Vec<Token>, String> {
    let keyword = Regex::new(r"^(if|else|while|then|do|not)\b").unwrap();
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
                    loc: TokenLocation { line: 0, column: 0 },
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
            .unwrap_or(TokenLocation { line: 1, column: 1 }),
    });
    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind::*;

    fn tokenize_without_loc(source_code: &str) -> Vec<(TokenKind, String)> {
        let mut tokens = tokenize(source_code).unwrap();
        tokens.pop();
        tokens.into_iter().map(|t| (t.kind, t.text)).collect()
    }

    fn bool(value: &str) -> (TokenKind, String) {
        (BoolLiteral, value.into())
    }

    fn int(value: &str) -> (TokenKind, String) {
        (IntLiteral, value.into())
    }

    fn ide(value: &str) -> (TokenKind, String) {
        (Identifier, value.into())
    }

    fn keyw(value: &str) -> (TokenKind, String) {
        (Keyword, value.into())
    }

    fn ope(value: &str) -> (TokenKind, String) {
        (Operator, value.into())
    }

    fn punc(value: &str) -> (TokenKind, String) {
        (Punctuation, value.into())
    }

    #[test]
    fn test_tokenizer_basics() {
        assert_eq!(
            tokenize_without_loc("if  3\nwhile"),
            vec![keyw("if"), int("3"), keyw("while")]
        );
        assert_eq!(
            tokenize_without_loc("if true then 1 else 0"),
            vec![
                keyw("if"),
                bool("true"),
                keyw("then"),
                int("1"),
                keyw("else"),
                int("0"),
            ]
        );
    }

    #[test]
    fn test_tokenizer_valid_identifiers() {
        assert_eq!(tokenize_without_loc("hello"), vec![ide("hello")]);
        assert_eq!(tokenize_without_loc("HELLO"), vec![ide("HELLO")]);
        assert_eq!(tokenize_without_loc("hEl_LO"), vec![ide("hEl_LO")]);
        assert_eq!(tokenize_without_loc("__hello"), vec![ide("__hello")]);
        assert_eq!(tokenize_without_loc("_hEl_LO"), vec![ide("_hEl_LO")]);
        assert_eq!(tokenize_without_loc("_hE12lLO"), vec![ide("_hE12lLO")]);
    }

    #[test]
    fn test_tokenizer_literals() {
        assert_eq!(tokenize_without_loc("0"), vec![int("0")]);
        assert_eq!(tokenize_without_loc("123"), vec![int("123")]);
        assert_eq!(
            tokenize_without_loc("123 123  11"),
            vec![int("123"), int("123"), int("11")]
        );
        assert_eq!(
            tokenize_without_loc("000 123 123  11"),
            vec![int("000"), int("123"), int("123"), int("11")]
        );
    }

    #[test]
    fn test_tokenizer_whitespace() {
        assert_eq!(
            tokenize_without_loc("moi  miten\n menee\t sulla"),
            vec![ide("moi"), ide("miten"), ide("menee"), ide("sulla")]
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
        assert_eq!(tokenize_without_loc("+"), vec![ope("+")]);
        assert_eq!(tokenize_without_loc("<="), vec![ope("<=")]);
        assert_eq!(tokenize_without_loc("+-"), vec![ope("+"), ope("-")]);
        assert_eq!(tokenize_without_loc("<<"), vec![ope("<"), ope("<")]);
        assert_eq!(
            tokenize_without_loc("+-<<=>="),
            vec![ope("+"), ope("-"), ope("<"), ope("<="), ope(">=")]
        );
        assert_eq!(
            tokenize_without_loc("1+1=2"),
            vec![int("1"), ope("+"), int("1"), ope("="), int("2")]
        );
        assert_eq!(
            tokenize_without_loc("true or false"),
            vec![bool("true"), ope("or"), bool("false")]
        );
    }

    #[test]
    fn test_tokenizer_punctuation() {
        assert_eq!(tokenize_without_loc(","), vec![punc(",")]);
        assert_eq!(tokenize_without_loc(";;"), vec![punc(";"), punc(";")]);
        assert_eq!(tokenize_without_loc("{}"), vec![punc("{"), punc("}")]);
        assert_eq!(tokenize_without_loc("()"), vec![punc("("), punc(")")]);
        assert_eq!(
            tokenize_without_loc("(){,;}"),
            vec![
                punc("("),
                punc(")"),
                punc("{"),
                punc(","),
                punc(";"),
                punc("}")
            ]
        );
        assert_eq!(
            tokenize_without_loc("1+1=2"),
            vec![int("1"), ope("+"), int("1"), ope("="), int("2")]
        );
    }

    #[test]
    fn test_tokenizer_comments() {
        assert_eq!(tokenize_without_loc("# hello"), vec![]);
        assert_eq!(tokenize_without_loc("// hello"), vec![]);
        assert_eq!(tokenize_without_loc("/* hello */"), vec![]);
        assert_eq!(
            tokenize_without_loc("hello # hello hello"),
            vec![ide("hello")]
        );
        assert_eq!(
            tokenize_without_loc("hello # hello hello\n 1"),
            vec![ide("hello"), int("1")]
        );
        assert_eq!(
            tokenize_without_loc("hello 1 # this is a comment\n a=2 // comment # comment\n 2"),
            vec![
                ide("hello"),
                int("1"),
                ide("a"),
                ope("="),
                int("2"),
                int("2")
            ]
        );
        assert_eq!(
            tokenize_without_loc("hello /* hello\n hi=1 */ 1"),
            vec![ide("hello"), int("1")]
        );
    }

    #[test]
    fn test_tokenizer_mixed() {
        assert_eq!(
            tokenize_without_loc(
                "moi1 123  hello 23 21 _var1 //this is a comment\n if 23 == 23 /* this\n is\n a multi-line comment */ 1+a=32"
            ),
            vec![
                ide("moi1"),
                int("123"),
                ide("hello"),
                int("23"),
                int("21"),
                ide("_var1"),
                keyw("if"),
                int("23"),
                ope("=="),
                int("23"),
                int("1"),
                ope("+"),
                ide("a"),
                ope("="),
                int("32")
            ]
        );
    }
}
