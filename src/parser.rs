use crate::{
    Error,
    expr::{Expr, ExprValue},
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

    pub(crate) fn parse_expr(&mut self) -> Result<Expr, Error> {
        Ok(self.parse_and_expr()?)
    }

    fn parse_and_expr(&mut self) -> Result<Expr, Error> {
        let mut exprs = vec![self.parse_or_expr()?];

        while self.peek() == Some(&Token::And) {
            self.advance(); // consume `AND` token.
            exprs.push(self.parse_or_expr()?);
        }

        if exprs.len() == 1 {
            Ok(exprs.into_iter().next().unwrap())
        } else {
            Ok(Expr::And(exprs))
        }
    }

    fn parse_or_expr(&mut self) -> Result<Expr, Error> {
        let mut exprs = vec![self.parse_comparison()?];

        while self.peek() == Some(&Token::Or) {
            self.advance(); // consume `OR` token.
            exprs.push(self.parse_comparison()?);
        }

        if exprs.len() == 1 {
            Ok(exprs.into_iter().next().unwrap())
        } else {
            Ok(Expr::Or(exprs))
        }
    }

    fn parse_comparison(&mut self) -> Result<Expr, Error> {
        // Handle NOT operator (unary)
        if self.peek() == Some(&Token::Not) {
            self.advance(); // consume NOT
            let expr = self.parse_comparison()?;
            return Ok(Expr::Not(Box::new(expr)));
        }

        let left = self.parse_value_or_field()?;

        Ok(match self.peek() {
            Some(&Token::Eq) => {
                self.advance();
                let right = self.parse_value_or_field()?;
                Expr::Eq(Box::new(left), Box::new(right))
            }
            Some(&Token::Gt) => {
                self.advance();
                let right = self.parse_value_or_field()?;
                Expr::Gt(Box::new(left), Box::new(right))
            }
            Some(&Token::Lt) => {
                self.advance();
                let right = self.parse_value_or_field()?;
                Expr::Lt(Box::new(left), Box::new(right))
            }
            Some(&Token::Ge) => {
                self.advance();
                let right = self.parse_value_or_field()?;
                Expr::Ge(Box::new(left), Box::new(right))
            }
            Some(&Token::Le) => {
                self.advance();
                let right = self.parse_value_or_field()?;
                Expr::Le(Box::new(left), Box::new(right))
            }
            Some(&Token::Ne) => {
                self.advance();
                let right = self.parse_value_or_field()?;
                Expr::Ne(Box::new(left), Box::new(right))
            }
            Some(&Token::In) => {
                self.advance();
                let right = self.parse_value_or_field()?;
                Expr::In(Box::new(left), Box::new(right))
            }
            _ => left,
        })
    }

    fn parse_value_or_field(&mut self) -> Result<Expr, Error> {
        // Handle array literal [ ... ]
        if self.peek() == Some(&Token::LBracket) {
            return self.parse_array();
        }

        let token = self.peek().cloned();
        if let Some(token) = token {
            self.advance();
            Ok(match token {
                Token::Str(s) => Expr::value(s),
                Token::I64(i) => Expr::value(i),
                Token::F64(f) => Expr::value(f),
                Token::Bool(b) => Expr::value(b),
                Token::Null => Expr::value(ExprValue::Null),
                Token::Ident(name) => {
                    // Check if this is a function call
                    if self.peek() == Some(&Token::LParen) {
                        self.parse_func_call(name)?
                    } else {
                        // Identifiers are treated as field names.
                        Expr::field(name)
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
        args.push(self.parse_value_or_field()?);

        // Parse remaining arguments separated by commas
        while self.peek() == Some(&Token::Comma) {
            self.advance(); // consume comma
            args.push(self.parse_value_or_field()?);
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
            return Ok(Expr::value(ExprValue::Array(vec![])));
        }

        // Parse first element
        values.push(self.parse_array_element()?);

        // Parse remaining elements separated by commas
        while self.peek() == Some(&Token::Comma) {
            self.advance(); // consume comma
            values.push(self.parse_array_element()?);
        }

        // Consume ]
        if self.peek() != Some(&Token::RBracket) {
            return Err(Error::Parse("expected ]".to_string()));
        }
        self.advance();

        Ok(Expr::value(ExprValue::Array(values)))
    }

    fn parse_array_element(&mut self) -> Result<ExprValue, Error> {
        let token = self.peek().cloned();
        if let Some(token) = token {
            self.advance();
            Ok(match token {
                Token::Str(s) => ExprValue::Str(s),
                Token::I64(i) => ExprValue::Int(i),
                Token::F64(f) => ExprValue::Float(f),
                Token::Bool(b) => ExprValue::Bool(b),
                Token::Null => ExprValue::Null,
                Token::Ident(name) => {
                    // In array context, identifiers are treated as strings
                    ExprValue::Str(name)
                }
                _ => {
                    let err_msg = format!("unexpected token in array: {:?}", token);
                    return Err(Error::Parse(err_msg));
                }
            })
        } else {
            Err(Error::Parse("unexpected end of input in array".to_string()))
        }
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
            Expr::And(vec![
                Expr::Eq(Expr::field_boxed("name"), Expr::value_boxed("John")),
                Expr::Gt(Expr::field_boxed("age"), Expr::value_boxed(18)),
                Expr::Gt(Expr::value_boxed(1), Expr::value_boxed(0)),
            ])
        );

        let input = r#"name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0"#;
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::And(vec![
                Expr::Eq(Expr::field_boxed("name"), Expr::value_boxed("John")),
                Expr::In(
                    Expr::field_boxed("age"),
                    Expr::value_boxed(vec![18, 19, 20, 22])
                ),
                Expr::Gt(Expr::value_boxed(1), Expr::value_boxed(0)),
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
                vec![Expr::field("name"), Expr::value("^J.*n$"),]
            )
        );

        let input = r#"name != null"#;
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(
            expr,
            Expr::Ne(
                Expr::field_boxed("name"),
                Expr::value_boxed(ExprValue::Null)
            )
        );
    }
}
