use regex::Regex;

#[derive(Debug, Clone, PartialEq)]
pub enum TokenKind {
    Identifier,
    IntLiteral,
    Operator,
    Punctutation,
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
    let identifier = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    let literal = Regex::new(r"^[0-9]+").unwrap();
    let operator = Regex::new(r"^(\=\=|\!\=|\<\=|\>\=|[\<\>\+\-\*\/\%\=])").unwrap();
    let punctuation = Regex::new(r"^[\;\,\(\)\{\}]").unwrap();

    let comment = Regex::new(r"^(\/\/|\#)[^\n]*").unwrap();
    let whitespace = Regex::new(r"^\s+").unwrap();

    let patterns = vec![
        (TokenKind::Identifier, identifier),
        (TokenKind::IntLiteral, literal),
        (TokenKind::Operator, operator),
        (TokenKind::Punctutation, punctuation),
    ];

    let mut remaining_code = source_code;
    let mut tokens = Vec::new();

    while !remaining_code.is_empty() {
        if let Some(matched) = whitespace.find(remaining_code) {
            remaining_code = &remaining_code[matched.end()..];
            continue;
        }
        if let Some(matched) = comment.find(remaining_code) {
            remaining_code = &remaining_code[matched.end()..];
            continue;
        }

        let mut match_found = false;
        for (token_kind, pattern) in &patterns {
            if let Some(matched) = pattern.find(remaining_code) {
                let token = Token {
                    kind: token_kind.clone(),
                    text: matched.as_str().to_string(),
                    loc: TokenLocation { line: 0, column: 0 },
                };
                tokens.push(token);
                remaining_code = &remaining_code[matched.end()..];
                match_found = true;
                break;
            }
        }

        if !match_found {
            return Err("No match found".to_string());
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;
    use TokenKind::*;

    fn tokenize_without_loc(source_code: &str) -> Vec<(TokenKind, String)> {
        tokenize(source_code)
            .unwrap()
            .into_iter()
            .map(|t| (t.kind, t.text))
            .collect()
    }

    #[test]
    fn test_tokenizer_basics() {
        assert_eq!(
            tokenize_without_loc("if  3\nwhile"),
            vec![
                (Identifier, "if".into()),
                (IntLiteral, "3".into()),
                (Identifier, "while".into())
            ]
        );
    }

    #[test]
    fn test_tokenizer_valid_identifiers() {
        assert_eq!(
            tokenize_without_loc("hello"),
            vec![(Identifier, "hello".into())]
        );
        assert_eq!(
            tokenize_without_loc("HELLO"),
            vec![(Identifier, "HELLO".into())]
        );
        assert_eq!(
            tokenize_without_loc("hEl_LO"),
            vec![(Identifier, "hEl_LO".into())]
        );
        assert_eq!(
            tokenize_without_loc("__hello"),
            vec![(Identifier, "__hello".into())]
        );
        assert_eq!(
            tokenize_without_loc("_hEl_LO"),
            vec![(Identifier, "_hEl_LO".into())]
        );
        assert_eq!(
            tokenize_without_loc("_hE12lLO"),
            vec![(Identifier, "_hE12lLO".into())]
        );
    }

    #[test]
    fn test_tokenizer_literals() {
        assert_eq!(tokenize_without_loc("0"), vec![(IntLiteral, "0".into())]);
        assert_eq!(
            tokenize_without_loc("123"),
            vec![(IntLiteral, "123".into())]
        );
        assert_eq!(
            tokenize_without_loc("123 123  11"),
            vec![
                (IntLiteral, "123".into()),
                (IntLiteral, "123".into()),
                (IntLiteral, "11".into())
            ]
        );
        assert_eq!(
            tokenize_without_loc("000 123 123  11"),
            vec![
                (IntLiteral, "000".into()),
                (IntLiteral, "123".into()),
                (IntLiteral, "123".into()),
                (IntLiteral, "11".into())
            ]
        );
    }

    #[test]
    fn test_tokenizer_whitespace() {
        assert_eq!(
            tokenize_without_loc("moi  miten\n menee\t sulla"),
            vec![
                (Identifier, "moi".into()),
                (Identifier, "miten".into()),
                (Identifier, "menee".into()),
                (Identifier, "sulla".into())
            ]
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
    fn test_tokenizer_mixed() {
        assert_eq!(
            tokenize_without_loc("moi1 123  hello 23 21 _var1\n if 23"),
            vec![
                (Identifier, "moi1".into()),
                (IntLiteral, "123".into()),
                (Identifier, "hello".into()),
                (IntLiteral, "23".into()),
                (IntLiteral, "21".into()),
                (Identifier, "_var1".into()),
                (Identifier, "if".into()),
                (IntLiteral, "23".into())
            ]
        );
    }
}
