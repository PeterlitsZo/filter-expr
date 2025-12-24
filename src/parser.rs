use crate::{
    Error,
    expr::Expr,
    token::Token,
};

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
    /// <term> = ['NOT'] <term>
    ///        | <value> [<operator> <value>]
    /// ```
    fn parse_term(&mut self) -> Result<Expr, Error> {
        // Handle NOT operator (unary)
        if self.peek() == Some(&Token::Not) {
            self.advance(); // consume NOT
            let expr = self.parse_term()?;
            return Ok(Expr::Not(Box::new(expr)));
        }

        let left = self.parse_value()?;

        Ok(match self.peek() {
            Some(&Token::Eq) => {
                self.advance();
                let right = self.parse_value()?;
                Expr::Eq(Box::new(left), Box::new(right))
            }
            Some(&Token::Gt) => {
                self.advance();
                let right = self.parse_value()?;
                Expr::Gt(Box::new(left), Box::new(right))
            }
            Some(&Token::Lt) => {
                self.advance();
                let right = self.parse_value()?;
                Expr::Lt(Box::new(left), Box::new(right))
            }
            Some(&Token::Ge) => {
                self.advance();
                let right = self.parse_value()?;
                Expr::Ge(Box::new(left), Box::new(right))
            }
            Some(&Token::Le) => {
                self.advance();
                let right = self.parse_value()?;
                Expr::Le(Box::new(left), Box::new(right))
            }
            Some(&Token::Ne) => {
                self.advance();
                let right = self.parse_value()?;
                Expr::Ne(Box::new(left), Box::new(right))
            }
            Some(&Token::In) => {
                self.advance();
                let right = self.parse_value()?;
                Expr::In(Box::new(left), Box::new(right))
            }
            _ => left,
        })
    }

    /// ```text
    /// <value> = <str>
    ///        | <i64>
    ///        | <f64>
    ///        | <bool>
    ///        | <null>
    ///        | <func-call>
    ///        | <ident>
    ///        | <array>
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
                    // Check if this is a function call
                    if self.peek() == Some(&Token::LParen) {
                        self.parse_func_call(name)?
                    } else {
                        // Identifiers are treated as field names.
                        Expr::field_(name)
                    }
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
        assert_eq!(
            expr,
            Expr::ne_(
                Expr::field_("name"),
                Expr::null_()
            )
        );
    }
}
