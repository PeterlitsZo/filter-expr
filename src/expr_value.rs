use crate::Error;

#[derive(Debug, Clone, PartialEq)]
pub enum ExprValue {
    /// The string value.
    Str(String),
    /// The integer value.
    I64(i64),
    /// The float value.
    F64(f64),
    /// The boolean value.
    Bool(bool),
    /// The null value.
    Null,

    /// The array value.
    Array(Vec<ExprValue>),
}

impl PartialOrd for ExprValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (ExprValue::Str(a), ExprValue::Str(b)) => a.partial_cmp(b),
            (ExprValue::I64(a), ExprValue::I64(b)) => a.partial_cmp(b),
            (ExprValue::F64(a), ExprValue::F64(b)) => a.partial_cmp(b),
            (ExprValue::Bool(a), ExprValue::Bool(b)) => a.partial_cmp(b),
            (ExprValue::Null, ExprValue::Null) => Some(std::cmp::Ordering::Equal),

            (ExprValue::F64(a), ExprValue::I64(b)) => a.partial_cmp(&(*b as f64)),
            (ExprValue::I64(a), ExprValue::F64(b)) => (*a as f64).partial_cmp(b),

            (ExprValue::Array(a), ExprValue::Array(b)) => a.partial_cmp(b),

            (ExprValue::Null, _) => Some(std::cmp::Ordering::Greater),
            (_, ExprValue::Null) => Some(std::cmp::Ordering::Less),

            _ => None, // Different types cannot be compared...
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
        ExprValue::I64(self)
    }
}

impl Into<ExprValue> for f64 {
    fn into(self) -> ExprValue {
        ExprValue::F64(self)
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

impl ExprValue {
    pub fn typ(&self) -> ExprValueType {
        match self {
            ExprValue::Str(_) => ExprValueType::Str,
            ExprValue::I64(_) => ExprValueType::I64,
            ExprValue::F64(_) => ExprValueType::F64,
            ExprValue::Bool(_) => ExprValueType::Bool,
            ExprValue::Null => ExprValueType::Null,
            ExprValue::Array(_) => ExprValueType::Array,
        }
    }
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub enum ExprValueType {
    Str,
    I64,
    F64,
    Bool,
    Null,

    Array,
}

pub struct ExprFnContext {
    pub args: Vec<ExprValue>,
}

#[async_trait::async_trait]
pub trait ExprFn: Send + Sync {
    async fn call(&self, ctx: ExprFnContext) -> Result<ExprValue, Error>;
}

pub type BoxedExprFn = Box<dyn ExprFn>;
