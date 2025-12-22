use crate::{Error, ctx::Context};

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum Expr {
    Field(String),
    Value(ExprValue),

    FuncCall(String, Vec<Expr>),

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
    pub(crate) fn field<T: Into<String>>(field: T) -> Self {
        Self::Field(field.into())
    }

    pub(crate) fn field_boxed<T: Into<String>>(field: T) -> Box<Self> {
        Box::new(Self::field(field))
    }

    pub(crate) fn value<T: Into<ExprValue>>(value: T) -> Self {
        Self::Value(value.into())
    }

    pub(crate) fn value_boxed<T: Into<ExprValue>>(value: T) -> Box<Self> {
        Box::new(Self::value(value))
    }

    pub(crate) async fn eval(&self, ctx: &dyn Context) -> Result<ExprValue, Error> {
        match self {
            Self::Field(field) => {
                let value = ctx.get_var(field).await?;
                Ok(value)
            }
            Self::Value(value) => Ok(value.clone()),

            Self::FuncCall(func, args) => {
                let mut args_values = vec![];
                for arg in args {
                    let value = Box::pin(arg.eval(ctx)).await?;
                    args_values.push(value);
                }

                match func.as_str() {
                    "matches" => {
                        if args_values.len() != 2 {
                            return Err(Error::InvalidArgumentCount {
                                expected: 2,
                                got: args_values.len(),
                            });
                        }
                        let text = match &args_values[0] {
                            ExprValue::Str(s) => s,
                            _ => {
                                return Err(Error::InvalidArgumentType {
                                    expected: "string".to_string(),
                                    got: format!("{:?}", args_values[0]),
                                });
                            }
                        };
                        let pattern = match &args_values[1] {
                            ExprValue::Str(s) => s,
                            _ => {
                                return Err(Error::InvalidArgumentType {
                                    expected: "string".to_string(),
                                    got: format!("{:?}", args_values[1]),
                                });
                            }
                        };
                        let pattern = regex::Regex::new(&pattern);
                        let pattern = match pattern {
                            Ok(pattern) => pattern,
                            Err(e) => {
                                return Err(Error::Internal(format!("failed to compile regex: {}", e)));
                            }
                        };

                        let matches = pattern.is_match(&text);
                        Ok(ExprValue::Bool(matches))
                    }
                    _ => Err(Error::InvalidFunction(func.clone())),
                }
            }

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
                    ExprValue::Array(array) => Ok(ExprValue::Bool(array.contains(&left_value))),
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
                    _ => Err(Error::TypeMismatch(format!("{:?}", value), format!("bool"))),
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
