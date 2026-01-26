use regex::Regex;

pub fn tokenize(source_code: &str) -> Result<Vec<String>, String> {
    let identifier = Regex::new(r"^[a-zA-Z_][a-zA-Z0-9_]*").unwrap();
    let literal = Regex::new(r"^[0-9]+").unwrap();
    let whitespace = Regex::new(r"^\s+").unwrap();

    let patterns = vec![identifier, literal];

    let mut remaining_code = source_code;
    let mut tokens = Vec::new();

    while !remaining_code.is_empty() {
        if let Some(matched) = whitespace.find(remaining_code) {
            remaining_code = &remaining_code[matched.end()..];
            continue;
        }

        let mut match_found = false;
        for pattern in &patterns {
            if let Some(matched) = pattern.find(remaining_code) {
                let token = matched.as_str();
                tokens.push(token.to_string());
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

    #[test]
    fn test_tokenizer_basics() {
        let input = "if  3\nwhile";
        let expected = vec!["if", "3", "while"];
        assert_eq!(tokenize(input).unwrap(), expected);
    }
}
