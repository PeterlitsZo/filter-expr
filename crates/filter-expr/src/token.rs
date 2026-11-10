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

    /// The let keyword (`let`).
    Let,
    /// The then keyword (`then`).
    Then,
    /// The binding operator (`:=`).
    Bind,

    /// The addition or unary positive operator (`+`).
    Add,
    /// The subtraction or unary negative operator (`-`).
    Sub,
    /// The multiplication operator (`*`).
    Mul,
    /// The division operator (`/`).
    Div,
    /// The remainder operator (`%`).
    Rem,

    /// The equal operator (`=` or `==`).
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
    /// The in operator (`IN` or `in`).
    In,

    /// The left parenthesis (`(`).
    LParen,
    /// The right parenthesis (`)`).
    RParen,
    /// The left square bracket (`[`).
    LBracket,
    /// The right square bracket (`]`).
    RBracket,
    /// The left brace (`{`).
    LBrace,
    /// The right brace (`}`).
    RBrace,
    /// The comma (`,`).
    Comma,
    /// The colon (`:`).
    Colon,
    /// The namespace separator (`::`).
    DoubleColon,
    /// The dot (`.`).
    Dot,

    /// The and operator (`&&`, `AND`, or `and`).
    And,
    /// The or operator (`||`, `OR`, or `or`).
    Or,
    /// The not keyword (`NOT` or `not`).
    Not,
    /// The symbolic not operator (`!`).
    Bang,
}

struct CharCursor<'a> {
    chars: std::str::Chars<'a>,
}

impl<'a> CharCursor<'a> {
    fn new(input: &'a str) -> Self {
        Self { chars: input.chars() }
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    fn next(&mut self) -> Option<char> {
        self.chars.next()
    }

    fn as_str(&self) -> &'a str {
        self.chars.as_str()
    }
}

pub(crate) fn parse_token(input: &str) -> Result<Vec<Token>, Error> {
    let mut tokens = Vec::new();
    let mut chars = CharCursor::new(input);

    while let Some(ch) = chars.peek() {
        match ch {
            // Skip whitespace.
            c if c.is_whitespace() => {
                chars.next();
            }

            // Division or block comments.
            '/' => {
                chars.next();
                if chars.peek() != Some('*') {
                    tokens.push(Token::Div);
                    continue;
                }

                #[derive(Clone, Copy, PartialEq)]
                enum CommentState {
                    NeedStar,
                    ReadingComment,
                    NeedSlash,
                    Done,
                }
                let mut s = CommentState::NeedStar;

                while let Some(c) = chars.next() {
                    match (s, c) {
                        (CommentState::NeedStar, '*') => {
                            s = CommentState::ReadingComment;
                            continue;
                        }
                        (CommentState::NeedStar, _) => {
                            return Err(Error::UnsupportedCharacter('/'));
                        }
                        (CommentState::ReadingComment, '*') => {
                            s = CommentState::NeedSlash;
                            continue;
                        }
                        (CommentState::ReadingComment, _) => {
                            continue;
                        }
                        (CommentState::NeedSlash, '/') => {
                            s = CommentState::Done;
                            break;
                        }
                        (CommentState::NeedSlash, '*') => {
                            continue;
                        }
                        (CommentState::NeedSlash, _) => {
                            s = CommentState::ReadingComment;
                            continue;
                        }
                        (CommentState::Done, _) => {
                            unreachable!("s cannot be Done");
                        }
                    }
                }

                if s != CommentState::Done {
                    return Err(Error::UnexpectedEOF);
                }
            }

            // String literal.
            '\'' | '"' => {
                chars.next();
                let remaining = chars.as_str();
                let mut decoded: Option<String> = None;
                let value = loop {
                    let c = chars.next().ok_or(Error::UnexpectedEOF)?;
                    if c == ch {
                        break match decoded {
                            Some(value) => value,
                            None => remaining[..remaining.len() - chars.as_str().len() - 1].to_owned(),
                        };
                    }
                    if c == '\\' {
                        let value = decoded.get_or_insert_with(|| {
                            remaining[..remaining.len() - chars.as_str().len() - 1].to_owned()
                        });
                        let escaped = chars.next().ok_or(Error::UnexpectedEOF)?;
                        value.push(match escaped {
                            'n' => '\n',
                            'r' => '\r',
                            't' => '\t',
                            other => other,
                        });
                    } else if let Some(value) = &mut decoded {
                        value.push(c);
                    }
                };
                tokens.push(Token::Str(value));
            }

            // Operators.
            '+' => {
                chars.next();
                tokens.push(Token::Add);
            }
            '-' => {
                chars.next();
                tokens.push(Token::Sub);
            }
            '*' => {
                chars.next();
                tokens.push(Token::Mul);
            }
            '%' => {
                chars.next();
                tokens.push(Token::Rem);
            }

            '&' | '|' => {
                chars.next();
                if chars.peek() != Some(ch) {
                    return Err(Error::UnsupportedCharacter(ch));
                }
                chars.next();
                tokens.push(if ch == '&' { Token::And } else { Token::Or });
            }
            '=' => {
                chars.next();
                if let Some('=') = chars.peek() {
                    chars.next();
                }
                tokens.push(Token::Eq);
            }
            '>' => {
                chars.next();
                if let Some('=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Ge);
                } else {
                    tokens.push(Token::Gt);
                }
            }
            '<' => {
                chars.next();
                if let Some('=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Le);
                } else {
                    tokens.push(Token::Lt);
                }
            }
            '!' => {
                chars.next();
                if let Some('=') = chars.peek() {
                    chars.next();
                    tokens.push(Token::Ne);
                } else {
                    tokens.push(Token::Bang);
                }
            }

            // Brackets.
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
            '{' => {
                chars.next();
                tokens.push(Token::LBrace);
            }
            '}' => {
                chars.next();
                tokens.push(Token::RBrace);
            }

            // Punctuations.
            ',' => {
                chars.next();
                tokens.push(Token::Comma);
            }
            ':' => {
                chars.next();
                if chars.peek() == Some(':') {
                    chars.next();
                    tokens.push(Token::DoubleColon);
                } else if chars.peek() == Some('=') {
                    chars.next();
                    tokens.push(Token::Bind);
                } else {
                    tokens.push(Token::Colon);
                }
            }
            '.' => {
                chars.next();
                tokens.push(Token::Dot);
            }

            // Float or Int.
            c if c.is_ascii_digit() || c == '.' => {
                let remaining = chars.as_str();
                while let Some(c) = chars.peek() {
                    if !(c.is_ascii_digit() || c == '.') {
                        break;
                    }
                    chars.next();
                }
                let text = &remaining[..remaining.len() - chars.as_str().len()];
                if text.contains('.') {
                    tokens.push(Token::F64(text.parse::<f64>().unwrap()));
                } else {
                    tokens.push(Token::I64(text.parse::<i64>().unwrap()));
                }
            }

            // Ident.
            c if c.is_alphanumeric() || c == '_' => {
                let remaining = chars.as_str();
                while let Some(c) = chars.peek() {
                    if !(c.is_alphanumeric() || c == '_') {
                        break;
                    }
                    chars.next();
                }

                let text = &remaining[..remaining.len() - chars.as_str().len()];

                // Check if it's a keyword or literal
                match text {
                    "AND" | "and" => tokens.push(Token::And),
                    "OR" | "or" => tokens.push(Token::Or),
                    "NOT" | "not" => tokens.push(Token::Not),
                    "let" => tokens.push(Token::Let),
                    "then" => tokens.push(Token::Then),
                    "IN" | "in" => tokens.push(Token::In),

                    "true" => tokens.push(Token::Bool(true)),
                    "false" => tokens.push(Token::Bool(false)),
                    "null" => tokens.push(Token::Null),

                    _ => tokens.push(Token::Ident(text.to_owned())),
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

    #[track_caller]
    fn parse_and_check(input: &str, tokens: &[Token]) {
        let inner = parse_token(input).unwrap();
        assert_eq!(inner, tokens);
    }

    #[test]
    fn test_parse_token() {
        use Token::*;

        parse_and_check(
            r#"name = 'John' AND age > 18"#,
            &[
                Ident("name".to_string()),
                Eq,
                Str("John".to_string()),
                And,
                Ident("age".to_string()),
                Gt,
                I64(18),
            ],
        );

        parse_and_check(
            r#"name = "John" AND age IN [18, 19, 20, 21]"#,
            &[
                Ident("name".to_string()),
                Eq,
                Str("John".to_string()),
                And,
                Ident("age".to_string()),
                In,
                LBracket,
                I64(18),
                Comma,
                I64(19),
                Comma,
                I64(20),
                Comma,
                I64(21),
                RBracket,
            ],
        );

        parse_and_check(
            r#"matches(name, "^J.*n$")"#,
            &[
                Ident("matches".to_string()),
                LParen,
                Ident("name".to_string()),
                Comma,
                Str("^J.*n$".to_string()),
                RParen,
            ],
        );

        parse_and_check(r#"name != null"#, &[Ident("name".to_string()), Ne, Null]);

        parse_and_check(
            r#"open = 1.5 AND age = 18 AND is_peter = true"#,
            &[
                Ident("open".to_string()),
                Eq,
                F64(1.5),
                And,
                Ident("age".to_string()),
                Eq,
                I64(18),
                And,
                Ident("is_peter".to_string()),
                Eq,
                Bool(true),
            ],
        );

        parse_and_check(
            r#"name.to_uppercase() = 'JOHN'"#,
            &[
                Ident("name".to_string()),
                Dot,
                Ident("to_uppercase".to_string()),
                LParen,
                RParen,
                Eq,
                Str("JOHN".to_string()),
            ],
        );

        parse_and_check(
            r#"name.contains('John')"#,
            &[
                Ident("name".to_string()),
                Dot,
                Ident("contains".to_string()),
                LParen,
                Str("John".to_string()),
                RParen,
            ],
        );

        parse_and_check(
            r#"type(maybe_i64_or_f64) IN ['i64', 'f64']"#,
            &[
                Ident("type".to_string()),
                LParen,
                Ident("maybe_i64_or_f64".to_string()),
                RParen,
                In,
                LBracket,
                Str("i64".to_string()),
                Comma,
                Str("f64".to_string()),
                RBracket,
            ],
        );

        parse_and_check(
            r#"type(foo.contains('bar')) = 'i64'"#,
            &[
                Ident("type".to_string()),
                LParen,
                Ident("foo".to_string()),
                Dot,
                Ident("contains".to_string()),
                LParen,
                Str("bar".to_string()),
                RParen,
                RParen,
                Eq,
                Str("i64".to_string()),
            ],
        );

        parse_and_check(
            r#"name = 'John\nDoe' OR name = "John\tDoe""#,
            &[
                Ident("name".to_string()),
                Eq,
                Str("John\nDoe".to_string()),
                Or,
                Ident("name".to_string()),
                Eq,
                Str("John\tDoe".to_string()),
            ],
        );

        parse_and_check(
            r#"name = 'John\\Doe'"#,
            &[Ident("name".to_string()), Eq, Str("John\\Doe".to_string())],
        );

        parse_and_check(
            r#"magic = "\"\n\r\t\\" AND magic = '\'\n\r\t\\'"#,
            &[
                Ident("magic".to_string()),
                Eq,
                Str("\"\n\r\t\\".to_string()),
                And,
                Ident("magic".to_string()),
                Eq,
                Str("'\n\r\t\\".to_string()),
            ],
        );

        parse_and_check(
            r#"a NOT IN ['foo', 'bar']"#,
            &[
                Ident("a".to_string()),
                Not,
                In,
                LBracket,
                Str("foo".to_string()),
                Comma,
                Str("bar".to_string()),
                RBracket,
            ],
        );

        parse_and_check(
            r#"!(a IN ['foo', 'bar'])"#,
            &[
                Bang,
                LParen,
                Ident("a".to_string()),
                In,
                LBracket,
                Str("foo".to_string()),
                Comma,
                Str("bar".to_string()),
                RBracket,
                RParen,
            ],
        );

        parse_and_check(
            r#"!(a IN /* the strings Peterlits like */ ['foo', 'bar'])"#,
            &[
                Bang,
                LParen,
                Ident("a".to_string()),
                In,
                LBracket,
                Str("foo".to_string()),
                Comma,
                Str("bar".to_string()),
                RBracket,
                RParen,
            ],
        );

        parse_and_check(
            r#"{"a":"b"}.a = "b""#,
            &[
                LBrace,
                Str("a".to_string()),
                Colon,
                Str("b".to_string()),
                RBrace,
                Dot,
                Ident("a".to_string()),
                Eq,
                Str("b".to_string()),
            ],
        );
    }
}
