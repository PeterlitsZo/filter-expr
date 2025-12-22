mod error;
mod token;

use std::collections::HashMap;

use token::Token;

pub use error::Error;

/// The filter expression.
pub struct FilterExpr {
    /// The expression of the filter. Possibly empty.
    #[allow(unused)]
    expr: Option<Expr>,
}

impl FilterExpr {
    pub fn parse(expr: &str) -> Result<Self, Error> {
        let expr = parse_expr(expr)?;
        Ok(Self { expr: Some(expr) })
    }

    pub async fn eval(&self, ctx: &dyn Context) -> Result<bool, Error> {
        if let Some(expr) = &self.expr {
            let value = expr.eval(ctx).await?;
            match value {
                ExprValue::Bool(b) => Ok(b),
                _ => Err(Error::InvalidValue(format!("{:?}", value))),
            }
        } else {
            Ok(true)
        }
    }
}

/// The context of the filter for evaluation.
#[async_trait::async_trait]
pub trait Context: Send + Sync {
    async fn get_var(&self, name: &str) -> Result<ExprValue, Error>;
}

pub struct SimpleContext {
    vars: HashMap<String, ExprValue>,
}

impl SimpleContext {
    pub fn new(vars: HashMap<String, ExprValue>) -> Self {
        Self { vars }
    }
}

#[async_trait::async_trait]
impl Context for SimpleContext {
    async fn get_var(&self, name: &str) -> Result<ExprValue, Error> {
        self.vars
            .get(name)
            .cloned()
            .ok_or(Error::NoSuchField(name.to_string()))
    }
}

#[async_trait::async_trait]
impl Context for HashMap<String, ExprValue> {
    async fn get_var(&self, name: &str) -> Result<ExprValue, Error> {
        Ok(self.get(name).cloned().unwrap_or(ExprValue::Bool(false)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Field(String),
    Value(ExprValue),

    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    In(Box<Expr>, Box<Expr>),

    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),
}

impl Expr {
    pub fn field<T: Into<String>>(field: T) -> Self {
        Self::Field(field.into())
    }

    pub fn field_boxed<T: Into<String>>(field: T) -> Box<Self> {
        Box::new(Self::field(field))
    }

    pub fn value<T: Into<ExprValue>>(value: T) -> Self {
        Self::Value(value.into())
    }

    pub fn value_boxed<T: Into<ExprValue>>(value: T) -> Box<Self> {
        Box::new(Self::value(value))
    }

    pub async fn eval(&self, ctx: &dyn Context) -> Result<ExprValue, Error> {
        match self {
            Self::Field(field) => {
                let value = ctx.get_var(field).await?;
                Ok(value)
            }
            Self::Value(value) => Ok(value.clone()),

            Self::Gt(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(ordering == std::cmp::Ordering::Greater)),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Self::Lt(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(ordering == std::cmp::Ordering::Less)),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Self::Ge(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(
                        ordering == std::cmp::Ordering::Greater
                            || ordering == std::cmp::Ordering::Equal,
                    )),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Self::Le(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(
                        ordering == std::cmp::Ordering::Less
                            || ordering == std::cmp::Ordering::Equal,
                    )),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Self::Eq(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                Ok(ExprValue::Bool(left_value == right_value))
            }
            Self::Ne(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                Ok(ExprValue::Bool(left_value != right_value))
            }
            Self::In(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                match right_value {
                    ExprValue::Array(array) => {
                        Ok(ExprValue::Bool(array.contains(&left_value)))
                    }
                    _ => Err(Error::TypeMismatch(
                        format!("{:?}", right_value),
                        format!("{:?}", left_value),
                    )),
                }
            }

            Self::And(exprs) => {
                let mut result = true;
                for expr in exprs {
                    let value = Box::pin(expr.eval(ctx)).await?;
                    match value {
                        ExprValue::Bool(b) => result = result && b,
                        _ => {
                            return Err(Error::InvalidValue(format!(
                                "expected bool, got {:?}",
                                value
                            )));
                        }
                    }
                }
                Ok(ExprValue::Bool(result))
            }
            Self::Or(exprs) => {
                let mut result = false;
                for expr in exprs {
                    let value = Box::pin(expr.eval(ctx)).await?;
                    match value {
                        ExprValue::Bool(b) => result = result || b,
                        _ => {
                            return Err(Error::InvalidValue(format!(
                                "expected bool, got {:?}",
                                value
                            )));
                        }
                    }
                }
                Ok(ExprValue::Bool(result))
            }
            Self::Not(expr) => {
                let value = Box::pin(expr.eval(ctx)).await?;
                match value {
                    ExprValue::Bool(b) => Ok(ExprValue::Bool(!b)),
                    _ => Err(Error::TypeMismatch(
                        format!("{:?}", value),
                        format!("bool"),
                    )),
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ExprValue {
    Str(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Null,

    Array(Vec<ExprValue>),
}

impl PartialOrd for ExprValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (ExprValue::Str(a), ExprValue::Str(b)) => a.partial_cmp(b),
            (ExprValue::Int(a), ExprValue::Int(b)) => a.partial_cmp(b),
            (ExprValue::Float(a), ExprValue::Float(b)) => a.partial_cmp(b),
            (ExprValue::Bool(a), ExprValue::Bool(b)) => a.partial_cmp(b),
            _ => None, // Different types cannot be compared
        }
    }
}

impl Into<ExprValue> for String {
    fn into(self) -> ExprValue {
        ExprValue::Str(self)
    }
}

impl Into<ExprValue> for &str {
    fn into(self) -> ExprValue {
        ExprValue::Str(self.to_string())
    }
}

impl Into<ExprValue> for i64 {
    fn into(self) -> ExprValue {
        ExprValue::Int(self)
    }
}

impl Into<ExprValue> for f64 {
    fn into(self) -> ExprValue {
        ExprValue::Float(self)
    }
}

impl Into<ExprValue> for bool {
    fn into(self) -> ExprValue {
        ExprValue::Bool(self)
    }
}

impl<T: Into<ExprValue>> Into<ExprValue> for Vec<T> {
    fn into(self) -> ExprValue {
        ExprValue::Array(self.into_iter().map(|item| item.into()).collect())
    }
}

fn parse_expr(input: &str) -> Result<Expr, Error> {
    let tokens = token::parse_token(input)?;
    let mut parser = Parser::new(tokens);
    Ok(parser.parse_expr()?)
}

struct Parser {
    tokens: Vec<Token>,
    pos: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Self { tokens, pos: 0 }
    }

    fn parse_expr(&mut self) -> Result<Expr, Error> {
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
                Token::Null => {
                    Expr::value(ExprValue::Null)
                }
                Token::Ident(name) => {
                    // Identifiers are treated as field names.
                    Expr::field(name)
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
    use super::*;

    #[tokio::test]
    async fn test_parse_expr_and_then_eval() {
        let input = "name = 'John' AND age > 18 AND 1 > 0";
        let expr = parse_expr(input).unwrap();
        let expected = Expr::And(vec![
            Expr::Eq(Expr::field_boxed("name"), Expr::value_boxed("John")),
            Expr::Gt(Expr::field_boxed("age"), Expr::value_boxed(18)),
            Expr::Gt(Expr::value_boxed(1), Expr::value_boxed(0)),
        ]);
        assert_eq!(expr, expected);

        let ctx = SimpleContext::new(HashMap::from([
            ("name".to_string(), "John".into()),
            ("age".to_string(), 19.into()),
        ]));
        let result = expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true.into());

        let ctx = SimpleContext::new(HashMap::from([
            ("name".to_string(), "John".into()),
            ("age".to_string(), 18.into()),
        ]));
        let result = expr.eval(&ctx).await.unwrap();
        assert_eq!(result, false.into());

        let input = r#"name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0"#;
        let expr = parse_expr(input).unwrap();
        let expected = Expr::And(vec![
            Expr::Eq(Expr::field_boxed("name"), Expr::value_boxed("John")),
            Expr::In(Expr::field_boxed("age"), Expr::value_boxed(vec![18, 19, 20, 22])),
            Expr::Gt(Expr::value_boxed(1), Expr::value_boxed(0)),
        ]);
        assert_eq!(expr, expected);

        let ctx = SimpleContext::new(HashMap::from([
            ("name".to_string(), "John".into()),
            ("age".to_string(), 19.into()),
        ]));
        let result = expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true.into());

        let ctx = SimpleContext::new(HashMap::from([
            ("name".to_string(), "John".into()),
            ("age".to_string(), 23.into()),
        ]));
        let result = expr.eval(&ctx).await.unwrap();
        assert_eq!(result, false.into());
    }
}
