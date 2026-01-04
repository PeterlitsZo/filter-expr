use crate::Error;

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    /// An identifier (variable name, field name, etc.).
    Ident(String),
    /// A string literal (single quotes or double quotes).
    Str(String),
    /// A float literal.
    F64(f64),
    /// A integer literal.
    I64(i64),
    /// A boolean literal.
    Bool(bool),
    /// A null literal.
    Null,

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
    /// The in operator (`IN`).
    In,

    /// The left parenthesis (`(`).
    LParen,
    /// The right parenthesis (`)`).
    RParen,
    /// The square bracket (`[`).
    LBracket,
    /// The square bracket (`]`).
    RBracket,
    /// The comma (`,`).
    Comma,
    /// The colon (`:`).
    Colon,
    /// The dot (`.`).
    Dot,

    /// The and operator (`&&` or `AND`).
    And,
    /// The or operator (`||` or `OR`).
    Or,
    /// The not operator (`!` or `NOT`).
    Not,
}

pub(crate) fn parse_token(input: &str) -> Result<Vec<Token>, Error> {
    let mut tokens = Vec::new();
    let mut chars = input.chars().peekable();

    while let Some(&ch) = chars.peek() {
        match ch {
            // Skip whitespace.
            c if c.is_whitespace() => {
                chars.next();
            }

            // String literal.
            '\'' => {
                chars.next();
                let mut s = String::new();
                let mut escaped = false;
                while let Some(c) = chars.next() {
                    match (escaped, c) {
                        (false, '\'') => {
                            break;
                        }
                        (false, '\\') => {
                            escaped = true;
                        }
                        (false, c) => {
                            s.push(c);
                        }

                        (true, '\'') => {
                            s.push('\'');
                            escaped = false;
                        }
                        (true, 'n') => {
                            s.push('\n');
                            escaped = false;
                        }
                        (true, 'r') => {
                            s.push('\r');
                            escaped = false;
                        }
                        (true, 't') => {
                            s.push('\t');
                            escaped = false;
                        }
                        (true, '\\') => {
                            s.push('\\');
                            escaped = false;
                        }
                        (true, c) => {
                            s.push(c);
                            escaped = false;
                        }
                    }
                }
                tokens.push(Token::Str(s));
            }
            '"' => {
                chars.next();
                let mut s = String::new();
                let mut escaped = false;
                while let Some(c) = chars.next() {
                    match (escaped, c) {
                        (false, '"') => {
                            break;
                        }
                        (false, '\\') => {
                            escaped = true;
                        }
                        (false, c) => {
                            s.push(c);
                        }

                        (true, '"') => {
                            s.push('"');
                            escaped = false;
                        }
                        (true, 'n') => {
                            s.push('\n');
                            escaped = false;
                        }
                        (true, 'r') => {
                            s.push('\r');
                            escaped = false;
                        }
                        (true, 't') => {
                            s.push('\t');
                            escaped = false;
                        }
                        (true, '\\') => {
                            s.push('\\');
                            escaped = false;
                        }
                        (true, c) => {
                            s.push(c);
                            escaped = false;
                        }
                    }
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
                    tokens.push(Token::Not);
                }
            }

            // Brackets and punctuation.
            '[' => {
                chars.next();
                tokens.push(Token::LBracket);
            }
            ']' => {
                chars.next();
                tokens.push(Token::RBracket);
            }
            '(' => {
                chars.next();
                tokens.push(Token::LParen);
            }
            ')' => {
                chars.next();
                tokens.push(Token::RParen);
            }
            ',' => {
                chars.next();
                tokens.push(Token::Comma);
            }
            ':' => {
                chars.next();
                tokens.push(Token::Colon);
            }
            '.' => {
                chars.next();
                tokens.push(Token::Dot);
            }

            // Float or Int.
            c if c.is_ascii_digit() || c == '.' => {
                let mut text = String::new();
                while let Some(&c) = chars.peek() {
                    if !(c.is_ascii_digit() || c == '.') {
                        break;
                    }
                    text.push(c);
                    chars.next();
                }
                if text.contains('.') {
                    tokens.push(Token::F64(text.parse::<f64>().unwrap()));
                } else {
                    tokens.push(Token::I64(text.parse::<i64>().unwrap()));
                }
            }

            // Ident.
            c if c.is_alphanumeric() || c == '_' => {
                let mut text = String::new();
                while let Some(&c) = chars.peek() {
                    if !(c.is_alphanumeric() || c == '_') {
                        break;
                    }
                    text.push(c);
                    chars.next();
                }

                // Check if it's a keyword or literal
                match text.as_str() {
                    "AND" => tokens.push(Token::And),
                    "OR" => tokens.push(Token::Or),
                    "IN" => tokens.push(Token::In),

                    "true" => tokens.push(Token::Bool(true)),
                    "false" => tokens.push(Token::Bool(false)),
                    "null" => tokens.push(Token::Null),

                    _ => tokens.push(Token::Ident(text)),
                }
            }

            // Unsupported characters.
            ch => {
                return Err(Error::UnsupportedCharacter(ch));
            }
        }
    }

    Ok(tokens)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_token() {
        let input = "name = 'John' AND age > 18";
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("name".to_string()),
                Token::Eq,
                Token::Str("John".to_string()),
                Token::And,
                Token::Ident("age".to_string()),
                Token::Gt,
                Token::I64(18),
            ]
        );

        let input = r#"name = "John" AND age IN [18, 19, 20, 21]"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("name".to_string()),
                Token::Eq,
                Token::Str("John".to_string()),
                Token::And,
                Token::Ident("age".to_string()),
                Token::In,
                Token::LBracket,
                Token::I64(18),
                Token::Comma,
                Token::I64(19),
                Token::Comma,
                Token::I64(20),
                Token::Comma,
                Token::I64(21),
                Token::RBracket,
            ]
        );

        let input = r#"matches(name, "^J.*n$")"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("matches".to_string()),
                Token::LParen,
                Token::Ident("name".to_string()),
                Token::Comma,
                Token::Str("^J.*n$".to_string()),
                Token::RParen,
            ]
        );

        let input = r#"name != null"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![Token::Ident("name".to_string()), Token::Ne, Token::Null,]
        );

        let input = r#"open = 1.5 AND age = 18 AND is_peter = true"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("open".to_string()),
                Token::Eq,
                Token::F64(1.5),
                Token::And,
                Token::Ident("age".to_string()),
                Token::Eq,
                Token::I64(18),
                Token::And,
                Token::Ident("is_peter".to_string()),
                Token::Eq,
                Token::Bool(true),
            ]
        );

        let input = r#"name.to_uppercase() = 'JOHN'"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("name".to_string()),
                Token::Dot,
                Token::Ident("to_uppercase".to_string()),
                Token::LParen,
                Token::RParen,
                Token::Eq,
                Token::Str("JOHN".to_string()),
            ]
        );

        let input = r#"name.contains('John')"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("name".to_string()),
                Token::Dot,
                Token::Ident("contains".to_string()),
                Token::LParen,
                Token::Str("John".to_string()),
                Token::RParen,
            ]
        );

        let input = r#"type(maybe_i64_or_f64) IN ['i64', 'f64']"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("type".to_string()),
                Token::LParen,
                Token::Ident("maybe_i64_or_f64".to_string()),
                Token::RParen,
                Token::In,
                Token::LBracket,
                Token::Str("i64".to_string()),
                Token::Comma,
                Token::Str("f64".to_string()),
                Token::RBracket,
            ]
        );

        let input = r#"type(foo.contains('bar')) = 'i64'"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("type".to_string()),
                Token::LParen,
                Token::Ident("foo".to_string()),
                Token::Dot,
                Token::Ident("contains".to_string()),
                Token::LParen,
                Token::Str("bar".to_string()),
                Token::RParen,
                Token::RParen,
                Token::Eq,
                Token::Str("i64".to_string()),
            ]
        );

        let input = r#"name = 'John\nDoe' OR name = "John\tDoe""#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("name".to_string()),
                Token::Eq,
                Token::Str("John\nDoe".to_string()),
                Token::Or,
                Token::Ident("name".to_string()),
                Token::Eq,
                Token::Str("John\tDoe".to_string()),
            ]
        );

        let input = r#"name = 'John\\Doe'"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("name".to_string()),
                Token::Eq,
                Token::Str("John\\Doe".to_string()),
            ]
        );

        let input = r#"magic = "\"\n\r\t\\" AND magic = '\'\n\r\t\\'"#;
        let tokens = parse_token(input).unwrap();
        assert_eq!(
            tokens,
            vec![
                Token::Ident("magic".to_string()),
                Token::Eq,
                Token::Str("\"\n\r\t\\".to_string()),
                Token::And,
                Token::Ident("magic".to_string()),
                Token::Eq,
                Token::Str("'\n\r\t\\".to_string()),
            ]
        );
    }
}
