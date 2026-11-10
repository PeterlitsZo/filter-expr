use std::collections::BTreeMap;

use crate::{Error, expr::Expr, token::Token};

pub(crate) struct Parser {
    tokens: std::vec::IntoIter<Token>,
}

impl Parser {
    pub(crate) fn new(tokens: Vec<Token>) -> Self {
        Self { tokens: tokens.into_iter() }
    }

    /// Parse a complete expression, rejecting trailing tokens.
    pub(crate) fn parse(&mut self) -> Result<Expr, Error> {
        let expr = self.parse_expr()?;
        if let Some(token) = self.peek() {
            return Err(Error::Parse(format!(
                "unexpected trailing token: {token:?}"
            )));
        }
        Ok(expr)
    }

    /// Parse a binding or a complete logical expression.
    fn parse_expr(&mut self) -> Result<Expr, Error> {
        if self.peek() != Some(&Token::Let) {
            return self.parse_logical_expr();
        }
        self.advance();
        let Some(Token::Ident(name)) = self.tokens.next() else {
            return Err(Error::Parse("expected binding name after 'let'".into()));
        };
        if self.peek() != Some(&Token::Bind) {
            return Err(Error::Parse("expected ':=' after binding name".into()));
        }
        self.advance();
        let value = self.parse_logical_expr()?;
        if self.peek() != Some(&Token::Then) {
            return Err(Error::Parse("expected 'then' after binding value".into()));
        }
        self.advance();
        let body = self.parse_expr()?;
        Ok(Expr::let_(name, value, body))
    }

    /// ```text
    /// <logical_expr> = <or_test> (<or> <or_test>)*
    /// ```
    fn parse_logical_expr(&mut self) -> Result<Expr, Error> {
        let item = self.parse_or_test()?;
        if self.peek() != Some(&Token::Or) {
            return Ok(item);
        }
        let mut exprs = match item {
            Expr::Or(exprs) => exprs,
            _ => vec![item],
        };

        while self.peek() == Some(&Token::Or) {
            self.advance(); // consume `OR` token.
            let item = self.parse_or_test()?;
            match item {
                Expr::Or(items) => exprs.extend(items),
                _ => exprs.push(item),
            }
        }

        if exprs.len() == 1 {
            Ok(exprs.into_iter().next().unwrap())
        } else {
            Ok(Expr::Or(exprs))
        }
    }

    /// ```text
    /// <or_test> = <and_test> (<and> <and_test>)*
    /// ```
    fn parse_or_test(&mut self) -> Result<Expr, Error> {
        let item = self.parse_and_test()?;
        if self.peek() != Some(&Token::And) {
            return Ok(item);
        }
        let mut exprs = match item {
            Expr::And(exprs) => exprs,
            _ => vec![item],
        };

        while self.peek() == Some(&Token::And) {
            self.advance(); // consume `AND` token.
            let item = self.parse_and_test()?;
            match item {
                Expr::And(items) => exprs.extend(items),
                _ => exprs.push(item),
            }
        }

        if exprs.len() == 1 {
            Ok(exprs.into_iter().next().unwrap())
        } else {
            Ok(Expr::And(exprs))
        }
    }

    /// ```text
    /// <and_test> = <not> <and_test> | <comparison>
    /// ```
    fn parse_and_test(&mut self) -> Result<Expr, Error> {
        // Handle either spelling of unary negation.
        if matches!(self.peek(), Some(Token::Not | Token::Bang)) {
            self.advance(); // consume NOT or !
            let expr = self.parse_and_test()?;
            return Ok(Expr::Not(Box::new(expr)));
        }

        self.parse_comparison()
    }

    /// ```text
    /// <comparison> = <additive> <operator> <comparison>
    ///              | <additive>
    /// ```
    fn parse_comparison(&mut self) -> Result<Expr, Error> {
        // Parse the arithmetic operand.
        let primary = self.parse_additive()?;

        // Check if there's an operator
        let operator = self.peek().filter(|token| matches!(token,
            Token::Eq | Token::Gt | Token::Lt | Token::Ge | Token::Le
            | Token::Ne | Token::In | Token::Not
        )).cloned();
        match operator {
            Some(Token::Eq) | Some(Token::Gt) | Some(Token::Lt) | Some(Token::Ge)
            | Some(Token::Le) | Some(Token::Ne) | Some(Token::In) => {
                // Parse <operator> <comparison> (recursive).

                // Consume the operator.
                self.advance();

                // Recursive call.
                let right = self.parse_comparison()?;

                Ok(match operator {
                    Some(Token::Eq) => Expr::Eq(Box::new(primary), Box::new(right)),
                    Some(Token::Gt) => Expr::Gt(Box::new(primary), Box::new(right)),
                    Some(Token::Lt) => Expr::Lt(Box::new(primary), Box::new(right)),
                    Some(Token::Ge) => Expr::Ge(Box::new(primary), Box::new(right)),
                    Some(Token::Le) => Expr::Le(Box::new(primary), Box::new(right)),
                    Some(Token::Ne) => Expr::Ne(Box::new(primary), Box::new(right)),
                    Some(Token::In) => Expr::In(Box::new(primary), Box::new(right)),
                    _ => {
                        let err_msg = format!("unexpected operator: {operator:?}");
                        return Err(Error::Parse(err_msg));
                    }
                })
            }
            Some(Token::Not) => {
                // Parse <operator> <comparison> (recursive). But the operator
                // is 'NOT' 'IN'.

                // consume the 'NOT'.
                self.advance();

                // consume the 'IN'.
                let should_be_in = self.peek();
                if should_be_in != Some(&Token::In) {
                    let err_msg = format!("unexpected operator: {should_be_in:?} after NOT");
                    return Err(Error::Parse(err_msg));
                }
                self.advance();

                // Recursive call.
                let right = self.parse_comparison()?;

                // Return the result.
                Ok(Expr::Not(Box::new(Expr::In(
                    Box::new(primary),
                    Box::new(right),
                ))))
            }
            _ => {
                // No comparison operator follows.
                Ok(primary)
            }
        }
    }

    /// ```text
    /// <additive> = <multiplicative> (('+' | '-') <multiplicative>)*
    /// ```
    fn parse_additive(&mut self) -> Result<Expr, Error> {
        let mut left = self.parse_multiplicative()?;
        while matches!(self.peek(), Some(Token::Add | Token::Sub)) {
            let operator = self.tokens.next().unwrap();
            let right = self.parse_multiplicative()?;
            left = match operator {
                Token::Add => Expr::Add(Box::new(left), Box::new(right)),
                Token::Sub => Expr::Sub(Box::new(left), Box::new(right)),
                _ => unreachable!(),
            };
        }
        Ok(left)
    }

    /// ```text
    /// <multiplicative> = <unary> (('*' | '/' | '%') <unary>)*
    /// ```
    fn parse_multiplicative(&mut self) -> Result<Expr, Error> {
        let mut left = self.parse_unary()?;
        while matches!(self.peek(), Some(Token::Mul | Token::Div | Token::Rem)) {
            let operator = self.tokens.next().unwrap();
            let right = self.parse_unary()?;
            left = match operator {
                Token::Mul => Expr::Mul(Box::new(left), Box::new(right)),
                Token::Div => Expr::Div(Box::new(left), Box::new(right)),
                Token::Rem => Expr::Rem(Box::new(left), Box::new(right)),
                _ => unreachable!(),
            };
        }
        Ok(left)
    }

    /// ```text
    /// <unary> = ('+' | '-') <unary> | <primary>
    /// ```
    fn parse_unary(&mut self) -> Result<Expr, Error> {
        match self.peek() {
            Some(Token::Add) => {
                self.advance();
                Ok(Expr::Pos(Box::new(self.parse_unary()?)))
            }
            Some(Token::Sub) => {
                self.advance();
                Ok(Expr::Neg(Box::new(self.parse_unary()?)))
            }
            _ => self.parse_primary(),
        }
    }

    /// ```text
    /// <primary> = <func-call>
    ///           | <method-call>
    ///           | <field-access>
    ///           | <value>
    /// ```
    fn parse_primary(&mut self) -> Result<Expr, Error> {
        // Parse a field or a possibly namespaced function call.
        if matches!(self.peek(), Some(Token::Ident(_))) {
            let Some(Token::Ident(mut name)) = self.tokens.next() else { unreachable!() };
            let mut namespaced = false;
            while self.peek() == Some(&Token::DoubleColon) {
                namespaced = true;
                self.advance();
                let Some(Token::Ident(segment)) = self.tokens.next() else {
                    return Err(Error::Parse("expected identifier after '::'".into()));
                };
                name.push_str("::");
                name.push_str(&segment);
            }
            if namespaced && self.peek() != Some(&Token::LParen) {
                return Err(Error::Parse(
                    "expected '(' after namespaced function name".into(),
                ));
            }

            if self.peek() == Some(&Token::LParen) {
                // It's a function call
                let value = self.parse_func_call(name)?;
                return self.parse_method_call_chain_or_field_access(value);
            } else {
                // It's a field name (value)
                // We need to check if it's followed by a method call
                let value = Expr::field_(name);
                return self.parse_method_call_chain_or_field_access(value);
            }
        }

        // Parse as value (which may include method calls)
        let value = self.parse_value()?;
        self.parse_method_call_chain_or_field_access(value)
    }

    /// ```text
    /// <value> = <str>
    ///         | <i64>
    ///         | <f64>
    ///         | <bool>
    ///         | <null>
    ///         | <ident>
    ///         | <array>
    ///         | <object>
    ///         | '(' <expr> ')'
    ///
    /// <array> = '[' [<expr> (',' <expr>)* ','?] ']'
    /// <object> = '{' [<str> ':' <expr> (',' <str> ':' <expr>)* ','?] '}'
    /// ```
    fn parse_value(&mut self) -> Result<Expr, Error> {
        // Handle array.
        if self.peek() == Some(&Token::LBracket) {
            return self.parse_array();
        }

        // Handle object.
        if self.peek() == Some(&Token::LBrace) {
            return self.parse_object();
        }

        let token = self.tokens.next();
        if let Some(token) = token {
            Ok(match token {
                Token::Str(s) => Expr::str_(s),
                Token::I64(i) => Expr::i64_(i),
                Token::F64(f) => Expr::f64_(f),
                Token::Bool(b) => Expr::bool_(b),
                Token::Null => Expr::null_(),
                Token::Ident(name) => Expr::field_(name),
                Token::LParen => {
                    let expr = self.parse_expr()?;
                    if self.peek() != Some(&Token::RParen) {
                        return Err(Error::Parse("expected ')'".to_string()));
                    }
                    self.advance();
                    expr
                }
                _ => {
                    // Unexpected token.
                    let err_msg = format!("parse value: unexpected token: {token:?}");
                    return Err(Error::Parse(err_msg));
                }
            })
        } else {
            // No more tokens.
            let err_msg = "no more tokens".to_string();
            Err(Error::Parse(err_msg))
        }
    }

    fn parse_func_call(&mut self, func_name: String) -> Result<Expr, Error> {
        // Consume the '(' token.
        if self.peek() != Some(&Token::LParen) {
            return Err(Error::Parse("expected '('".to_string()));
        }
        self.advance();

        let mut args = Vec::new();

        // Handle empty argument list.
        if self.peek() == Some(&Token::RParen) {
            self.advance();
            return Ok(Expr::FuncCall(func_name, args));
        }

        // Parse first argument.
        args.push(self.parse_expr()?);

        // Parse remaining arguments separated by commas
        while self.peek() == Some(&Token::Comma) {
            self.advance(); // consume comma
            args.push(self.parse_expr()?);
        }

        // Consume the ')' token
        if self.peek() != Some(&Token::RParen) {
            return Err(Error::Parse("expected ')'".to_string()));
        }
        self.advance();

        Ok(Expr::FuncCall(func_name, args))
    }

    fn parse_method_call_chain_or_field_access(&mut self, mut value: Expr) -> Result<Expr, Error> {
        // Handle method calls: <value> '.' <ident> '(' ... ')'
        while self.peek() == Some(&Token::Dot) {
            self.advance(); // consume '.'

            let method_or_field_name = match self.tokens.next() {
                Some(Token::Ident(name)) => name,
                _ => {
                    return Err(Error::Parse("expected method name after '.'".to_string()));
                }
            };

            // Parse method call or field access arguments
            if self.peek() != Some(&Token::LParen) {
                value = Expr::FieldAccess(Box::new(value), method_or_field_name);
                continue;
            }
            self.advance(); // consume '('

            let mut args = Vec::new();

            // Handle empty argument list ()
            if self.peek() == Some(&Token::RParen) {
                self.advance();
                value = Expr::MethodCall(method_or_field_name, Box::new(value), args);
                continue;
            }

            // Parse first argument
            args.push(self.parse_expr()?);

            // Parse remaining arguments separated by commas
            while self.peek() == Some(&Token::Comma) {
                self.advance(); // consume comma
                args.push(self.parse_expr()?);
            }

            // Consume the ')' token
            if self.peek() != Some(&Token::RParen) {
                return Err(Error::Parse("expected ')'".to_string()));
            }
            self.advance();

            value = Expr::MethodCall(method_or_field_name, Box::new(value), args);
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
        values.push(self.parse_expr()?);

        // Parse remaining elements separated by commas
        while self.peek() == Some(&Token::Comma) {
            self.advance(); // consume comma
            values.push(self.parse_expr()?);
        }

        // Consume ]
        if self.peek() != Some(&Token::RBracket) {
            return Err(Error::Parse("expected ]".to_string()));
        }
        self.advance();

        Ok(Expr::array_(values))
    }

    /// ```text
    /// <object> = '{' [<str> ':' <expr> (',' <str> ':' <expr>)* ','?] '}'
    /// ```
    fn parse_object(&mut self) -> Result<Expr, Error> {
        if self.peek() != Some(&Token::LBrace) {
            return Err(Error::Parse("expected '{'".to_string()));
        }
        self.advance();

        let mut fields = BTreeMap::new();
        while self.peek() != Some(&Token::RBrace) {
            let key = match self.tokens.next() {
                Some(Token::Str(key)) => key,
                _ => return Err(Error::Parse("expected string key in object".to_string())),
            };

            if self.peek() != Some(&Token::Colon) {
                return Err(Error::Parse("expected ':' after object key".to_string()));
            }
            self.advance();
            fields.insert(key, self.parse_expr()?);

            if self.peek() != Some(&Token::Comma) {
                break;
            }
            self.advance();
        }

        if self.peek() != Some(&Token::RBrace) {
            return Err(Error::Parse("expected '}'".to_string()));
        }
        self.advance();

        Ok(Expr::object_(fields))
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.as_slice().first()
    }

    fn advance(&mut self) {
        self.tokens.next();
    }
}

#[cfg(test)]
mod tests {
    use crate::token::parse_token;

    use super::*;

    #[track_caller]
    fn parse_and_check(input: &str, expected: Expr) {
        let tokens = parse_token(input).unwrap();
        let mut parser = Parser::new(tokens);
        let expr = parser.parse_expr().unwrap();
        assert_eq!(expr, expected);
    }

    #[test]
    fn test_parse_expr() {
        parse_and_check(
            "name = 'John' AND age > 18 AND 1 > 0",
            Expr::and_([
                Expr::eq_(Expr::field_("name"), Expr::str_("John")),
                Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
                Expr::gt_(Expr::i64_(1), Expr::i64_(0)),
            ]),
        );

        parse_and_check(
            r#"name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0"#,
            Expr::and_([
                Expr::eq_(Expr::field_("name"), Expr::str_("John")),
                Expr::in_(
                    Expr::field_("age"),
                    Expr::array_([
                        Expr::i64_(18),
                        Expr::i64_(19),
                        Expr::i64_(20),
                        Expr::i64_(22),
                    ]),
                ),
                Expr::gt_(Expr::i64_(1), Expr::i64_(0)),
            ]),
        );

        parse_and_check(
            r#"matches(name, "^J.*n$")"#,
            Expr::FuncCall(
                "matches".to_string(),
                vec![Expr::field_("name"), Expr::str_("^J.*n$")],
            ),
        );

        parse_and_check(
            r#"name != null"#,
            Expr::ne_(Expr::field_("name"), Expr::null_()),
        );

        parse_and_check(
            r#"name.to_uppercase() = 'JOHN'"#,
            Expr::eq_(
                Expr::method_call_(Expr::field_("name"), "to_uppercase".to_string(), vec![]),
                Expr::str_("JOHN"),
            ),
        );

        parse_and_check(
            r#"name.to_uppercase().to_lowercase() = 'john'"#,
            Expr::eq_(
                Expr::method_call_(
                    Expr::method_call_(Expr::field_("name"), "to_uppercase".to_string(), vec![]),
                    "to_lowercase".to_string(),
                    vec![],
                ),
                Expr::str_("john"),
            ),
        );

        parse_and_check(
            r#"name.contains('John')"#,
            Expr::method_call_(
                Expr::field_("name"),
                "contains".to_string(),
                vec![Expr::str_("John")],
            ),
        );

        parse_and_check(
            r#"true OR false"#,
            Expr::or_(vec![Expr::bool_(true), Expr::bool_(false)]),
        );

        parse_and_check(
            r#"true AND false OR true"#,
            Expr::or_(vec![
                Expr::and_(vec![Expr::bool_(true), Expr::bool_(false)]),
                Expr::bool_(true),
            ]),
        );

        parse_and_check(
            r#"(true OR false) AND true"#,
            Expr::and_(vec![
                Expr::or_(vec![Expr::bool_(true), Expr::bool_(false)]),
                Expr::bool_(true),
            ]),
        );

        parse_and_check(
            r#"(name = 'John' AND age > 18) AND 1 > 0"#,
            Expr::and_(vec![
                Expr::eq_(Expr::field_("name"), Expr::str_("John")),
                Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
                Expr::gt_(Expr::i64_(1), Expr::i64_(0)),
            ]),
        );

        parse_and_check(
            r#"name = 'John' AND (age > 18 AND 1 > 0)"#,
            Expr::and_(vec![
                Expr::eq_(Expr::field_("name"), Expr::str_("John")),
                Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
                Expr::gt_(Expr::i64_(1), Expr::i64_(0)),
            ]),
        );

        parse_and_check(
            r#"(name = 'John' AND age > 18) OR (1 > 0)"#,
            Expr::or_(vec![
                Expr::and_(vec![
                    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
                    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
                ]),
                Expr::gt_(Expr::i64_(1), Expr::i64_(0)),
            ]),
        );

        parse_and_check(
            r#"type(maybe_i64_or_f64) IN ['i64', 'f64']"#,
            Expr::in_(
                Expr::FuncCall("type".to_string(), vec![Expr::field_("maybe_i64_or_f64")]),
                Expr::array_([Expr::str_("i64"), Expr::str_("f64")]),
            ),
        );

        parse_and_check(
            r#"type(foo.contains('bar')) = 'i64'"#,
            Expr::eq_(
                Expr::FuncCall(
                    "type".to_string(),
                    vec![Expr::method_call_(
                        Expr::field_("foo"),
                        "contains".to_string(),
                        vec![Expr::str_("bar")],
                    )],
                ),
                Expr::str_("i64"),
            ),
        );

        parse_and_check(
            r#"foo.bar.baz = 5"#,
            Expr::eq_(
                Expr::field_access_(Expr::field_access_(Expr::field_("foo"), "bar"), "baz"),
                Expr::i64_(5),
            ),
        );

        parse_and_check(
            r#"a NOT IN ['foo', 'bar']"#,
            Expr::not_(Expr::in_(
                Expr::field_("a"),
                Expr::array_([Expr::str_("foo"), Expr::str_("bar")]),
            )),
        );

        parse_and_check(
            r#"!(a IN ['foo', 'bar'])"#,
            Expr::not_(Expr::in_(
                Expr::field_("a"),
                Expr::array_([Expr::str_("foo"), Expr::str_("bar")]),
            )),
        );

        parse_and_check(
            r#"{"a":"b",}.a = "b""#,
            Expr::eq_(
                Expr::field_access_(Expr::object_([("a".to_string(), Expr::str_("b"))]), "a"),
                Expr::str_("b"),
            ),
        );
    }
}
