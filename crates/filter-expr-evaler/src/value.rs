#[derive(Debug, Clone, PartialEq)]
pub enum Value {
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
    Array(Vec<Value>),
}

impl PartialOrd for Value {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => a.partial_cmp(b),
            (Value::I64(a), Value::I64(b)) => a.partial_cmp(b),
            (Value::F64(a), Value::F64(b)) => a.partial_cmp(b),
            (Value::Bool(a), Value::Bool(b)) => a.partial_cmp(b),
            (Value::Null, Value::Null) => Some(std::cmp::Ordering::Equal),

            (Value::F64(a), Value::I64(b)) => a.partial_cmp(&(*b as f64)),
            (Value::I64(a), Value::F64(b)) => (*a as f64).partial_cmp(b),

            (Value::Array(a), Value::Array(b)) => a.partial_cmp(b),

            (Value::Null, _) => Some(std::cmp::Ordering::Greater),
            (_, Value::Null) => Some(std::cmp::Ordering::Less),

            _ => None, // Different types cannot be compared...
        }
    }
}

impl Into<Value> for String {
    fn into(self) -> Value {
        Value::Str(self)
    }
}

impl Into<Value> for &str {
    fn into(self) -> Value {
        Value::Str(self.to_string())
    }
}

impl Into<Value> for i64 {
    fn into(self) -> Value {
        Value::I64(self)
    }
}

impl Into<Value> for f64 {
    fn into(self) -> Value {
        Value::F64(self)
    }
}

impl Into<Value> for bool {
    fn into(self) -> Value {
        Value::Bool(self)
    }
}

impl<T: Into<Value>> Into<Value> for Vec<T> {
    fn into(self) -> Value {
        Value::Array(self.into_iter().map(|item| item.into()).collect())
    }
}

impl Value {
    pub fn typ(&self) -> ValueType {
        match self {
            Value::Str(_) => ValueType::Str,
            Value::I64(_) => ValueType::I64,
            Value::F64(_) => ValueType::F64,
            Value::Bool(_) => ValueType::Bool,
            Value::Null => ValueType::Null,
            Value::Array(_) => ValueType::Array,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ValueType {
    Str,
    I64,
    F64,
    Bool,
    Null,

    Array,
}

#[cfg(test)]
mod tests {
    use crate::Value;

    #[test]
    fn test_expr_value_ordering() {
        // Test string ordering.
        assert!(Value::Str("a".to_string()) < Value::Str("b".to_string()));
        assert!(Value::Str("a".to_string()) <= Value::Str("a".to_string()));
        assert!(Value::Str("b".to_string()) > Value::Str("a".to_string()));

        // Test integer ordering.
        assert!(Value::I64(1) < Value::I64(2));
        assert!(Value::I64(1) <= Value::I64(1));
        assert!(Value::I64(2) > Value::I64(1));

        // Test float ordering.
        assert!(Value::F64(1.0) < Value::F64(2.0));
        assert!(Value::F64(1.0) <= Value::F64(1.0));
        assert!(Value::F64(2.0) > Value::F64(1.0));

        // Test boolean ordering.
        assert!(Value::Bool(false) < Value::Bool(true));
        assert!(Value::Bool(false) <= Value::Bool(false));
        assert!(Value::Bool(true) > Value::Bool(false));

        // Test Int and Float comparison.
        assert!(Value::I64(1) < Value::F64(2.0));
        assert!(Value::I64(2) > Value::F64(1.0));
        assert!(Value::F64(1.0) < Value::I64(2));
        assert!(Value::F64(2.0) > Value::I64(1));

        // Test Null ordering.
        assert!(Value::Null == Value::Null);
        assert!(Value::Null > Value::Str("a".to_string()));
        assert!(Value::Str("a".to_string()) < Value::Null);
        assert!(Value::Null > Value::I64(1));
        assert!(Value::I64(1) < Value::Null);

        // Test array ordering.
        let arr1 = Value::Array(vec![Value::I64(1), Value::I64(2)]);
        let arr2 = Value::Array(vec![Value::I64(1), Value::I64(3)]);
        assert!(arr1 < arr2);
        assert!(arr1 <= arr1);
        assert!(arr2 > arr1);

        // Test incompatible types (should return None).
        assert!(
            Value::Str("a".to_string())
                .partial_cmp(&Value::I64(1))
                .is_none()
        );
        assert!(Value::I64(1).partial_cmp(&Value::Bool(true)).is_none());
        assert!(
            Value::Str("a".to_string())
                .partial_cmp(&Value::Bool(false))
                .is_none()
        );
        assert!(Value::Array(vec![]).partial_cmp(&Value::I64(1)).is_none());
    }
}
