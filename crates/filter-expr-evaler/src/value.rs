use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    /// The string value.
    Str(Arc<String>),
    /// The integer value.
    I64(i64),
    /// The float value.
    F64(f64),
    /// The boolean value.
    Bool(bool),
    /// The null value.
    Null,

    /// The array value.
    Array(Arc<Vec<Value>>),
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

impl From<String> for Value {
    fn from(val: String) -> Self {
        Value::Str(Arc::new(val))
    }
}

impl From<&str> for Value {
    fn from(val: &str) -> Self {
        Value::Str(Arc::new(val.to_string()))
    }
}

impl From<i64> for Value {
    fn from(val: i64) -> Self {
        Value::I64(val)
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Value::F64(val)
    }
}

impl From<bool> for Value {
    fn from(val: bool) -> Self {
        Value::Bool(val)
    }
}

impl<T: Into<Value>> From<Vec<T>> for Value {
    fn from(val: Vec<T>) -> Self {
        Value::Array(Arc::new(val.into_iter().map(|item| item.into()).collect()))
    }
}

impl Value {
    pub fn str(s: impl Into<String>) -> Self {
        Value::Str(Arc::new(s.into()))
    }

    pub fn i64(i: i64) -> Self {
        Value::I64(i)
    }

    pub fn f64(f: f64) -> Self {
        Value::F64(f)
    }

    pub fn bool(b: bool) -> Self {
        Value::Bool(b)
    }

    pub fn null() -> Self {
        Value::Null
    }

    pub fn array(items: impl Into<Vec<Value>>) -> Self {
        Value::Array(Arc::new(items.into().into_iter().collect()))
    }

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
        assert!(Value::str("a") < Value::str("b"));
        assert!(Value::str("a") <= Value::str("a"));
        assert!(Value::str("b") > Value::str("a"));

        // Test integer ordering.
        assert!(Value::i64(1) < Value::i64(2));
        assert!(Value::i64(1) <= Value::i64(1));
        assert!(Value::i64(2) > Value::i64(1));

        // Test float ordering.
        assert!(Value::f64(1.0) < Value::f64(2.0));
        assert!(Value::f64(1.0) <= Value::f64(1.0));
        assert!(Value::f64(2.0) > Value::f64(1.0));

        // Test boolean ordering.
        assert!(Value::bool(false) < Value::bool(true));
        assert!(Value::bool(false) <= Value::bool(false));
        assert!(Value::bool(true) > Value::bool(false));

        // Test Int and Float comparison.
        assert!(Value::i64(1) < Value::f64(2.0));
        assert!(Value::i64(2) > Value::f64(1.0));
        assert!(Value::f64(1.0) < Value::i64(2));
        assert!(Value::f64(2.0) > Value::i64(1));

        // Test Null ordering.
        assert!(Value::null() == Value::null());
        assert!(Value::null() > Value::str("a"));
        assert!(Value::str("a") < Value::null());
        assert!(Value::null() > Value::i64(1));
        assert!(Value::i64(1) < Value::null());

        // Test array ordering.
        let arr1 = Value::array([Value::i64(1), Value::i64(2)]);
        let arr2 = Value::array([Value::i64(1), Value::i64(3)]);
        assert!(arr1 < arr2);
        assert!(arr1 <= arr1);
        assert!(arr2 > arr1);

        // Test incompatible types (should return None).
        assert!(Value::str("a").partial_cmp(&Value::i64(1)).is_none());
        assert!(Value::i64(1).partial_cmp(&Value::bool(true)).is_none());
        assert!(Value::str("a").partial_cmp(&Value::bool(false)).is_none());
        assert!(Value::array([]).partial_cmp(&Value::i64(1)).is_none());
    }
}
