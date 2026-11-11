use crate::{CompactString, Error, LanguageVersion};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Token {
    /// An identifier (variable name, field name, etc.).
    Ident(CompactString),
    /// A string literal (single quotes or double quotes).
    Str(CompactString),
    /// A float literal.
    F64(f64),
    /// A integer literal.
    I64(i64),
    BigInt(crate::BigInt),
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

pub(crate) struct Lexer<'a> {
    chars: CharCursor<'a>,
    at_start: bool,
    pub(crate) language_version: LanguageVersion,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Self {
            chars: CharCursor::new(input),
            at_start: true,
            language_version: LanguageVersion::V1,
        }
    }

    pub(crate) fn next_token(&mut self) -> Result<Option<Token>, Error> {
        let chars = &mut self.chars;

        while let Some(ch) = chars.peek() {
            if ch.is_whitespace() {
                chars.next();
                continue;
            }
            let declaration_allowed = self.at_start;
            self.at_start = false;
            match ch {
                // Division or block comments.
                '/' => {
                    chars.next();
                    if chars.peek() != Some('*') {
                        return Ok(Some(Token::Div));
                    }

                    let comment = chars.as_str();

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
                    if declaration_allowed && comment.starts_with("*!") {
                        let end = comment.len() - chars.as_str().len() - 2;
                        let version = comment[2..end].trim();
                        let valid = version.strip_prefix('v').is_some_and(|digits| {
                            !digits.is_empty()
                                && !digits.starts_with('0')
                                && digits.bytes().all(|b| b.is_ascii_digit())
                        });
                        if !valid {
                            continue;
                        }
                        self.language_version = match version {
                            "v1" => LanguageVersion::V1,
                            _ => return Err(Error::UnsupportedLanguageVersion(version.into())),
                        };
                    }
                }

                // String literal.
                '\'' | '"' => {
                    chars.next();
                    let remaining = chars.as_str();
                    let mut decoded: Option<CompactString> = None;
                    let value = loop {
                        let c = chars.next().ok_or(Error::UnexpectedEOF)?;
                        if c == ch {
                            break match decoded {
                                Some(value) => value,
                                None => remaining[..remaining.len() - chars.as_str().len() - 1].into(),
                            };
                        }
                        if c == '\\' {
                            let value = decoded.get_or_insert_with(|| {
                                remaining[..remaining.len() - chars.as_str().len() - 1].into()
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
                    return Ok(Some(Token::Str(value)));
                }

                // Operators.
                '+' => {
                    chars.next();
                    return Ok(Some(Token::Add));
                }
                '-' => {
                    chars.next();
                    return Ok(Some(Token::Sub));
                }
                '*' => {
                    chars.next();
                    return Ok(Some(Token::Mul));
                }
                '%' => {
                    chars.next();
                    return Ok(Some(Token::Rem));
                }

                '&' | '|' => {
                    chars.next();
                    if chars.peek() != Some(ch) {
                        return Err(Error::UnsupportedCharacter(ch));
                    }
                    chars.next();
                    return Ok(Some(if ch == '&' { Token::And } else { Token::Or }));
                }
                '=' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        chars.next();
                    }
                    return Ok(Some(Token::Eq));
                }
                '>' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        chars.next();
                        return Ok(Some(Token::Ge));
                    } else {
                        return Ok(Some(Token::Gt));
                    }
                }
                '<' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        chars.next();
                        return Ok(Some(Token::Le));
                    } else {
                        return Ok(Some(Token::Lt));
                    }
                }
                '!' => {
                    chars.next();
                    if let Some('=') = chars.peek() {
                        chars.next();
                        return Ok(Some(Token::Ne));
                    } else {
                        return Ok(Some(Token::Bang));
                    }
                }

                // Brackets.
                '[' => {
                    chars.next();
                    return Ok(Some(Token::LBracket));
                }
                ']' => {
                    chars.next();
                    return Ok(Some(Token::RBracket));
                }
                '(' => {
                    chars.next();
                    return Ok(Some(Token::LParen));
                }
                ')' => {
                    chars.next();
                    return Ok(Some(Token::RParen));
                }
                '{' => {
                    chars.next();
                    return Ok(Some(Token::LBrace));
                }
                '}' => {
                    chars.next();
                    return Ok(Some(Token::RBrace));
                }

                // Punctuations.
                ',' => {
                    chars.next();
                    return Ok(Some(Token::Comma));
                }
                ':' => {
                    chars.next();
                    if chars.peek() == Some(':') {
                        chars.next();
                        return Ok(Some(Token::DoubleColon));
                    } else if chars.peek() == Some('=') {
                        chars.next();
                        return Ok(Some(Token::Bind));
                    } else {
                        return Ok(Some(Token::Colon));
                    }
                }
                '.' => {
                    chars.next();
                    return Ok(Some(Token::Dot));
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
                        return Ok(Some(Token::F64(text.parse::<f64>().unwrap())));
                    } else {
                        return Ok(Some(match text.parse::<i64>() {
                            Ok(value) => Token::I64(value),
                            Err(_) => Token::BigInt(text.parse().map_err(|_| {
                                Error::Parse("invalid integer literal".into())
                            })?),
                        }));
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
                        "AND" | "and" => return Ok(Some(Token::And)),
                        "OR" | "or" => return Ok(Some(Token::Or)),
                        "NOT" | "not" => return Ok(Some(Token::Not)),
                        "let" => return Ok(Some(Token::Let)),
                        "then" => return Ok(Some(Token::Then)),
                        "IN" | "in" => return Ok(Some(Token::In)),

                        "true" => return Ok(Some(Token::Bool(true))),
                        "false" => return Ok(Some(Token::Bool(false))),
                        "null" => return Ok(Some(Token::Null)),

                        _ => return Ok(Some(Token::Ident(text.into()))),
                    }
                }

                // Unsupported characters.
                ch => {
                    return Err(Error::UnsupportedCharacter(ch));
                }
            }
        }

        Ok(None)
    }
}

#[cfg(test)]
fn parse_token(input: &str) -> Result<Vec<Token>, Error> {
    let mut lexer = Lexer::new(input);
    let mut tokens = Vec::new();
    while let Some(token) = lexer.next_token()? {
        tokens.push(token);
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
                Ident("name".into()),
                Eq,
                Str("John".into()),
                And,
                Ident("age".into()),
                Gt,
                I64(18),
            ],
        );

        parse_and_check(
            r#"name = "John" AND age IN [18, 19, 20, 21]"#,
            &[
                Ident("name".into()),
                Eq,
                Str("John".into()),
                And,
                Ident("age".into()),
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
                Ident("matches".into()),
                LParen,
                Ident("name".into()),
                Comma,
                Str("^J.*n$".into()),
                RParen,
            ],
        );

        parse_and_check(r#"name != null"#, &[Ident("name".into()), Ne, Null]);

        parse_and_check(
            r#"open = 1.5 AND age = 18 AND is_peter = true"#,
            &[
                Ident("open".into()),
                Eq,
                F64(1.5),
                And,
                Ident("age".into()),
                Eq,
                I64(18),
                And,
                Ident("is_peter".into()),
                Eq,
                Bool(true),
            ],
        );

        parse_and_check(
            r#"name.to_uppercase() = 'JOHN'"#,
            &[
                Ident("name".into()),
                Dot,
                Ident("to_uppercase".into()),
                LParen,
                RParen,
                Eq,
                Str("JOHN".into()),
            ],
        );

        parse_and_check(
            r#"name.contains('John')"#,
            &[
                Ident("name".into()),
                Dot,
                Ident("contains".into()),
                LParen,
                Str("John".into()),
                RParen,
            ],
        );

        parse_and_check(
            r#"type(maybe_i64_or_f64) IN ['i64', 'f64']"#,
            &[
                Ident("type".into()),
                LParen,
                Ident("maybe_i64_or_f64".into()),
                RParen,
                In,
                LBracket,
                Str("i64".into()),
                Comma,
                Str("f64".into()),
                RBracket,
            ],
        );

        parse_and_check(
            r#"type(foo.contains('bar')) = 'i64'"#,
            &[
                Ident("type".into()),
                LParen,
                Ident("foo".into()),
                Dot,
                Ident("contains".into()),
                LParen,
                Str("bar".into()),
                RParen,
                RParen,
                Eq,
                Str("i64".into()),
            ],
        );

        parse_and_check(
            r#"name = 'John\nDoe' OR name = "John\tDoe""#,
            &[
                Ident("name".into()),
                Eq,
                Str("John\nDoe".into()),
                Or,
                Ident("name".into()),
                Eq,
                Str("John\tDoe".into()),
            ],
        );

        parse_and_check(
            r#"name = 'John\\Doe'"#,
            &[Ident("name".into()), Eq, Str("John\\Doe".into())],
        );

        parse_and_check(
            r#"magic = "\"\n\r\t\\" AND magic = '\'\n\r\t\\'"#,
            &[
                Ident("magic".into()),
                Eq,
                Str("\"\n\r\t\\".into()),
                And,
                Ident("magic".into()),
                Eq,
                Str("'\n\r\t\\".into()),
            ],
        );

        parse_and_check(
            r#"a NOT IN ['foo', 'bar']"#,
            &[
                Ident("a".into()),
                Not,
                In,
                LBracket,
                Str("foo".into()),
                Comma,
                Str("bar".into()),
                RBracket,
            ],
        );

        parse_and_check(
            r#"!(a IN ['foo', 'bar'])"#,
            &[
                Bang,
                LParen,
                Ident("a".into()),
                In,
                LBracket,
                Str("foo".into()),
                Comma,
                Str("bar".into()),
                RBracket,
                RParen,
            ],
        );

        parse_and_check(
            r#"!(a IN /* the strings Peterlits like */ ['foo', 'bar'])"#,
            &[
                Bang,
                LParen,
                Ident("a".into()),
                In,
                LBracket,
                Str("foo".into()),
                Comma,
                Str("bar".into()),
                RBracket,
                RParen,
            ],
        );

        parse_and_check(
            r#"{"a":"b"}.a = "b""#,
            &[
                LBrace,
                Str("a".into()),
                Colon,
                Str("b".into()),
                RBrace,
                Dot,
                Ident("a".into()),
                Eq,
                Str("b".into()),
            ],
        );
    }
}
