use crate::{Error, expr::Expr, token::Token};

pub(crate) struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    /// ```text
    /// <expr> = <factor> ('AND' <factor>)*
    /// ```
    pub(crate) fn parse_expr(&mut self) -> Result<Expr, Error> {
        let mut exprs = vec![self.parse_factor()?];

        while self.peek() == Some(&Token::And) {
            self.advance(); // consume `AND` token.
            exprs.push(self.parse_factor()?);
        }

        if exprs.len() == 1 {
            Ok(exprs.into_iter().next().unwrap())
        } else {
            Ok(Expr::And(exprs))
        }
    }

    /// ```text
    /// <factor> = <term> ('OR' <term>)*
    /// ```
    fn parse_factor(&mut self) -> Result<Expr, Error> {
        let mut exprs = vec![self.parse_term()?];

        while self.peek() == Some(&Token::Or) {
            self.advance(); // consume `OR` token.
            exprs.push(self.parse_term()?);
        }

        if exprs.len() == 1 {
            Ok(exprs.into_iter().next().unwrap())
        } else {
            Ok(Expr::Or(exprs))
        }
    }

    /// ```text
    /// <term> = ['NOT'] <comparison>
    /// ```
    fn parse_term(&mut self) -> Result<Expr, Error> {
        // Handle NOT operator (unary)
        if self.peek() == Some(&Token::Not) {
            self.advance(); // consume NOT
            let expr = self.parse_term()?;
            return Ok(Expr::Not(Box::new(expr)));
        }

        self.parse_comparison()
    }

    /// ```text
    /// <comparison> = <primary> <operator> <comparison>
    ///              | <primary>
    /// ```
    fn parse_comparison(&mut self) -> Result<Expr, Error> {
        // Parse <primary>
        let primary = self.parse_primary()?;

        // Check if there's an operator
        let operator = self.peek().cloned();
        match operator {
            Some(Token::Eq) | Some(Token::Gt) | Some(Token::Lt) | Some(Token::Ge)
            | Some(Token::Le) | Some(Token::Ne) | Some(Token::In) => {
                // Parse <operator> <comparison> (recursive)
                self.advance(); // consume operator

                let right = self.parse_comparison()?; // Recursive call

                Ok(match operator.unwrap() {
                    Token::Eq => Expr::Eq(Box::new(primary), Box::new(right)),
                    Token::Gt => Expr::Gt(Box::new(primary), Box::new(right)),
                    Token::Lt => Expr::Lt(Box::new(primary), Box::new(right)),
                    Token::Ge => Expr::Ge(Box::new(primary), Box::new(right)),
                    Token::Le => Expr::Le(Box::new(primary), Box::new(right)),
                    Token::Ne => Expr::Ne(Box::new(primary), Box::new(right)),
                    Token::In => Expr::In(Box::new(primary), Box::new(right)),
                    _ => unreachable!(),
                })
            }
            _ => {
                // Just <primary>
                Ok(primary)
            }
        }
    }

    /// ```text
    /// <primary> = <func-call>
    ///           | <method-call>
    ///           | <value>
    /// ```
    fn parse_primary(&mut self) -> Result<Expr, Error> {
        // Try to parse as function call first (ident followed by '(')
        if let Some(&Token::Ident(ref name)) = self.peek() {
            let name = name.clone();
            self.advance();

            if self.peek() == Some(&Token::LParen) {
                // It's a function call
                return self.parse_func_call(name);
            } else {
                // It's a field name (value)
                // We need to check if it's followed by a method call
                let value = Expr::field_(name);
                return self.parse_method_call_chain(value);
            }
        }

        // Parse as value (which may include method calls)
        let value = self.parse_value()?;
        self.parse_method_call_chain(value)
    }

    /// ```text
    /// <value> = <str>
    ///         | <i64>
    ///         | <f64>
    ///         | <bool>
    ///         | <null>
    ///         | <ident>
    ///         | <array>
    ///
    /// <array> = '[' [<value> (',' <value>)* ','?] ']'
    /// ```
    fn parse_value(&mut self) -> Result<Expr, Error> {
        // Handle array literal [ ... ]
        if self.peek() == Some(&Token::LBracket) {
            return self.parse_array();
        }

        let token = self.peek().cloned();
        if let Some(token) = token {
            self.advance();
            Ok(match token {
                Token::Str(s) => Expr::str_(s),
                Token::I64(i) => Expr::i64_(i),
                Token::F64(f) => Expr::f64_(f),
                Token::Bool(b) => Expr::bool_(b),
                Token::Null => Expr::null_(),
                Token::Ident(name) => {
                    // Identifiers are treated as field names.
                    // Function calls are handled in parse_primary
                    Expr::field_(name)
                }
                _ => {
                    // Unexpected token.
                    let err_msg = format!("unexpected token: {:?}", token);
                    return Err(Error::Parse(err_msg));
                }
            })
        } else {
            // No more tokens.
            let err_msg = "no more tokens".to_string();
            return Err(Error::Parse(err_msg));
        }
    }

    fn parse_func_call(&mut self, func_name: String) -> Result<Expr, Error> {
        // Consume (
        if self.peek() != Some(&Token::LParen) {
            return Err(Error::Parse("expected (".to_string()));
        }
        self.advance();

        let mut args = Vec::new();

        // Handle empty argument list ()
        if self.peek() == Some(&Token::RParen) {
            self.advance();
            return Ok(Expr::FuncCall(func_name, args));
        }

        // Parse first argument
        args.push(self.parse_value()?);

        // Parse remaining arguments separated by commas
        while self.peek() == Some(&Token::Comma) {
            self.advance(); // consume comma
            args.push(self.parse_value()?);
        }

        // Consume )
        if self.peek() != Some(&Token::RParen) {
            return Err(Error::Parse("expected )".to_string()));
        }
        self.advance();

        Ok(Expr::FuncCall(func_name, args))
    }

    fn parse_method_call_chain(&mut self, mut value: Expr) -> Result<Expr, Error> {
        // Handle method calls: <value> '.' <ident> '(' ... ')'
        while self.peek() == Some(&Token::Dot) {
            self.advance(); // consume '.'

            let method_name = match self.peek() {
                Some(&Token::Ident(ref name)) => {
                    let name = name.clone();
                    self.advance();
                    name
                }
                _ => {
                    return Err(Error::Parse("expected method name after '.'".to_string()));
                }
            };

            // Parse method call arguments
            if self.peek() != Some(&Token::LParen) {
                return Err(Error::Parse("expected '(' after method name".to_string()));
            }
            self.advance(); // consume '('

            let mut args = Vec::new();

            // Handle empty argument list ()
            if self.peek() == Some(&Token::RParen) {
                self.advance();
                value = Expr::MethodCall(method_name, Box::new(value), args);
                continue;
            }

            // Parse first argument
            args.push(self.parse_value()?);

            // Parse remaining arguments separated by commas
            while self.peek() == Some(&Token::Comma) {
                self.advance(); // consume comma
                args.push(self.parse_value()?);
            }

            // Consume )
            if self.peek() != Some(&Token::RParen) {
                return Err(Error::Parse("expected )".to_string()));
            }
            self.advance();

            value = Expr::MethodCall(method_name, Box::new(value), args);
        }
        Ok(value)
    }

    fn parse_array(&mut self) -> Result<Expr, Error> {
        // Consume [
        if self.peek() != Some(&Token::LBracket) {
            return Err(Error::Parse("expected [".to_string()));
        }
        self.advance();

        let mut values = Vec::new();

        // Handle empty array []
        if self.peek() == Some(&Token::RBracket) {
            self.advance();
            return Ok(Expr::array_(vec![]));
        }

        // Parse first element
        values.push(self.parse_value()?);

        // Parse remaining elements separated by commas
        while self.peek() == Some(&Token::Comma) {
            self.advance(); // consume comma
            values.push(self.parse_value()?);
        }

        // Consume ]
        if self.peek() != Some(&Token::RBracket) {
            return Err(Error::Parse("expected ]".to_string()));
        }
        self.advance();

        Ok(Expr::array_(values))
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    fn advance(&mut self) {
        if self.pos < self.tokens.len() {
            self.pos += 1;
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::token::parse_token;

    use super::*;

    #[test]
    fn test_parse_expr() {
        let input = "name = 'John' AND age > 18 AND 1 > 0";
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::and_([
                Expr::eq_(Expr::field_("name"), Expr::str_("John")),
                Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
                Expr::gt_(Expr::i64_(1), Expr::i64_(0)),
            ])
        );

        let input = r#"name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0"#;
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::and_([
                Expr::eq_(Expr::field_("name"), Expr::str_("John")),
                Expr::in_(
                    Expr::field_("age"),
                    Expr::array_([
                        Expr::i64_(18),
                        Expr::i64_(19),
                        Expr::i64_(20),
                        Expr::i64_(22)
                    ])
                ),
                Expr::gt_(Expr::i64_(1), Expr::i64_(0)),
            ])
        );

        let input = r#"matches(name, "^J.*n$")"#;
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::FuncCall(
                "matches".to_string(),
                vec![Expr::field_("name"), Expr::str_("^J.*n$"),]
            )
        );

        let input = r#"name != null"#;
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, Expr::ne_(Expr::field_("name"), Expr::null_()));

        let input = r#"name.to_uppercase() = 'JOHN'"#;
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::eq_(
                Expr::method_call_(Expr::field_("name"), "to_uppercase".to_string(), vec![]),
                Expr::str_("JOHN")
            )
        );

        let input = r#"name.to_uppercase().to_lowercase() = 'john'"#;
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::eq_(
                Expr::method_call_(
                    Expr::method_call_(Expr::field_("name"), "to_uppercase".to_string(), vec![]),
                    "to_lowercase".to_string(),
                    vec![]
                ),
                Expr::str_("john")
            )
        );

        let input = r#"name.contains('John')"#;
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::method_call_(
                Expr::field_("name"),
                "contains".to_string(),
                vec![Expr::str_("John")]
            )
        );
    }
}
