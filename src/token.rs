#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    /// A free-form set of characters without whitespace (WS) or . (DOT) within
    /// it. The text may represent a variable, string, number, boolean, or
    /// alternative literal value and must be handled in a manner consistent
    /// with the service's intention.
    Text(String),
    /// A quoted string which may or may not contain a special wildcard `*`
    /// character at the beginning or end of the string to indicate a prefix or
    /// suffix-based search within a restriction.
    Str(String),

    /// The equal operator (`=`).
    Eq,
    /// The greater than operator (`>`).
    Gt,
    /// The less than operator (`<`).
    Lt,
    /// The greater than or equal to operator (`>=`).
    Ge,
    /// The less than or equal to operator (`<=`).
    Le,
    /// The not equal to operator (`!=`).
    Ne,

    /// The and operator (`AND`).
    And,
    /// The or operator (`OR`).
    Or,
}

pub(crate) fn parse_token(input: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();
    
    while let Some(&ch) = chars.peek() {
        match ch {
            // Skip whitespace.
            c if c.is_whitespace() => {
                chars.next();
            }

            // String literal (single quotes).
            '\'' => {
                chars.next(); // consume opening quote
                let mut s = String::new();
                while let Some(&c) = chars.peek() {
                    if c == '\'' {
                        chars.next(); // consume closing quote
                        break;
                    }
                    s.push(c);
                    chars.next();
                }
                tokens.push(Token::Str(s));
            }

            // Operators.
            '=' => {
                chars.next();
                tokens.push(Token::Eq);
            }
            '>' => {
                chars.next();
                if let Some(&'=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Ge);
                } else {
                    tokens.push(Token::Gt);
                }
            }
            '<' => {
                chars.next();
                if let Some(&'=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Le);
                } else {
                    tokens.push(Token::Lt);
                }
            }
            '!' => {
                chars.next();
                if let Some(&'=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Ne);
                } else {
                    // Treat ! as part of text if not followed by =
                    let mut text = String::from('!');
                    while let Some(&c) = chars.peek() {
                        if c.is_whitespace() || c == '.' || c == '=' || c == '>' || c == '<' || c == '!' {
                            break;
                        }
                        text.push(c);
                        chars.next();
                    }
                    tokens.push(Token::Text(text));
                }
            }

            // Text (variable names, numbers, etc.)
            _ => {
                let mut text = String::new();
                while let Some(&c) = chars.peek() {
                    if c.is_whitespace() || c == '.' || c == '=' || c == '>' || c == '<' || c == '!' || c == '\'' {
                        break;
                    }
                    text.push(c);
                    chars.next();
                }
                
                // Check if it's a keyword
                let text_upper = text.to_uppercase();
                match text_upper.as_str() {
                    "AND" => tokens.push(Token::And),
                    "OR" => tokens.push(Token::Or),
                    _ => tokens.push(Token::Text(text)),
                }
            }
        }
    }
    
    tokens
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_token() {
        let input = "name = 'John' AND age > 18";
        let tokens = parse_token(input);
        assert_eq!(tokens, vec![
            Token::Text("name".to_string()),
            Token::Eq,
            Token::Str("John".to_string()),
            Token::And, Token::Text("age".to_string()),
            Token::Gt,
            Token::Text("18".to_string())
        ]);
    }
}
