use std::{
    any::Any,
    cmp::Ordering,
    fmt::{Debug, Display},
    sync::Arc,
};

use crate::{Error, ErrorKind, Result};

#[derive(Debug, Clone)]
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

    /// The userdata value.
    Userdata(Arc<dyn Userdata>),
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
            Value::Userdata(_) => ValueType::Userdata,
        }
    }

    pub fn partial_eq(&self, other: &Self) -> Result<bool> {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => Ok(a == b),
            (Value::I64(a), Value::I64(b)) => Ok(a == b),
            (Value::F64(a), Value::F64(b)) => Ok(a == b),
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            (Value::Null, Value::Null) => Ok(true),

            (Value::F64(a), Value::I64(b)) => Ok(*a == *b as f64),
            (Value::I64(a), Value::F64(b)) => Ok(*a as f64 == *b),

            (Value::Array(a), Value::Array(b)) => {
                if a.len() != b.len() {
                    return Ok(false);
                }

                for (a, b) in a.iter().zip(b.iter()) {
                    if !a.partial_eq(b)? {
                        return Ok(false);
                    }
                }

                Ok(true)
            }

            (Value::Userdata(a), Value::Userdata(b)) if Arc::ptr_eq(a, b) => Ok(true),
            (Value::Userdata(userdata), _) => userdata.partial_eq(other),
            (_, Value::Userdata(userdata)) => userdata.partial_eq(self),

            _ => Ok(false),
        }
    }

    pub fn partial_cmp(&self, other: &Self) -> Result<Option<Ordering>> {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => Ok(a.partial_cmp(b)),
            (Value::I64(a), Value::I64(b)) => Ok(a.partial_cmp(b)),
            (Value::F64(a), Value::F64(b)) => Ok(a.partial_cmp(b)),
            (Value::Bool(a), Value::Bool(b)) => Ok(a.partial_cmp(b)),
            (Value::Null, Value::Null) => Ok(Some(Ordering::Equal)),

            (Value::F64(a), Value::I64(b)) => Ok(a.partial_cmp(&(*b as f64))),
            (Value::I64(a), Value::F64(b)) => Ok((*a as f64).partial_cmp(b)),

            (Value::Array(a), Value::Array(b)) => Self::partial_cmp_arrays(a, b),

            (Value::Userdata(a), Value::Userdata(b)) if Arc::ptr_eq(a, b) => {
                Ok(Some(Ordering::Equal))
            }
            (Value::Userdata(userdata), _) => userdata.partial_ord(other),
            (_, Value::Userdata(userdata)) => Ok(userdata
                .partial_ord(self)?
                .map(|ordering| ordering.reverse())),

            (Value::Null, _) => Ok(Some(Ordering::Greater)),
            (_, Value::Null) => Ok(Some(Ordering::Less)),

            (a, b) => Err(
                Error::new(ErrorKind::TypeMismatch, "compare two uncomparable values")
                    .with_metadata("left", a.typ())
                    .with_metadata("right", b.typ()),
            ),
        }
    }

    fn partial_cmp_arrays(a: &[Value], b: &[Value]) -> Result<Option<Ordering>> {
        for (a, b) in a.iter().zip(b.iter()) {
            match a.partial_cmp(b)? {
                Some(Ordering::Equal) => {}
                non_equal => return Ok(non_equal),
            }
        }

        Ok(a.len().partial_cmp(&b.len()))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ValueType {
    Str,
    I64,
    F64,
    Bool,
    Null,

    Array,

    Userdata,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

pub trait Userdata: Debug + Send + Sync {
    /// Return the value as `Any` for implementer-side downcasting.
    fn as_any(&self) -> &dyn Any;

    /// Compare with another value.
    fn partial_eq(&self, other: &Value) -> Result<bool>;

    /// Compare with another value.
    fn partial_ord(&self, other: &Value) -> Result<Option<Ordering>>;
}

#[cfg(test)]
mod tests {
    use std::{any::Any, cmp::Ordering, sync::Arc};

    use crate::{Result, Userdata, Value};

    fn assert_cmp(left: Value, right: Value, expected: Ordering) {
        assert_eq!(left.partial_cmp(&right).unwrap(), Some(expected));
    }

    fn assert_value_eq(left: Value, right: Value, expected: bool) {
        assert_eq!(left.partial_eq(&right).unwrap(), expected);
    }

    #[test]
    fn test_expr_value_ordering() {
        // Test string ordering.
        assert_cmp(Value::str("a"), Value::str("b"), Ordering::Less);
        assert_cmp(Value::str("a"), Value::str("a"), Ordering::Equal);
        assert_cmp(Value::str("b"), Value::str("a"), Ordering::Greater);

        // Test integer ordering.
        assert_cmp(Value::i64(1), Value::i64(2), Ordering::Less);
        assert_cmp(Value::i64(1), Value::i64(1), Ordering::Equal);
        assert_cmp(Value::i64(2), Value::i64(1), Ordering::Greater);

        // Test float ordering.
        assert_cmp(Value::f64(1.0), Value::f64(2.0), Ordering::Less);
        assert_cmp(Value::f64(1.0), Value::f64(1.0), Ordering::Equal);
        assert_cmp(Value::f64(2.0), Value::f64(1.0), Ordering::Greater);

        // Test boolean ordering.
        assert_cmp(Value::bool(false), Value::bool(true), Ordering::Less);
        assert_cmp(Value::bool(false), Value::bool(false), Ordering::Equal);
        assert_cmp(Value::bool(true), Value::bool(false), Ordering::Greater);

        // Test Int and Float comparison.
        assert_cmp(Value::i64(1), Value::f64(2.0), Ordering::Less);
        assert_cmp(Value::i64(2), Value::f64(1.0), Ordering::Greater);
        assert_cmp(Value::f64(1.0), Value::i64(2), Ordering::Less);
        assert_cmp(Value::f64(2.0), Value::i64(1), Ordering::Greater);

        // Test Null ordering.
        assert_value_eq(Value::null(), Value::null(), true);
        assert_cmp(Value::null(), Value::str("a"), Ordering::Greater);
        assert_cmp(Value::str("a"), Value::null(), Ordering::Less);
        assert_cmp(Value::null(), Value::i64(1), Ordering::Greater);
        assert_cmp(Value::i64(1), Value::null(), Ordering::Less);

        // Test array ordering.
        let arr1 = Value::array([Value::i64(1), Value::i64(2)]);
        let arr2 = Value::array([Value::i64(1), Value::i64(3)]);
        assert_cmp(arr1.clone(), arr2.clone(), Ordering::Less);
        assert_cmp(arr1.clone(), arr1, Ordering::Equal);
        assert_cmp(
            arr2,
            Value::array([Value::i64(1), Value::i64(2)]),
            Ordering::Greater,
        );

        // Test incompatible types.
        assert!(Value::str("a").partial_cmp(&Value::i64(1)).is_err());
        assert!(Value::i64(1).partial_cmp(&Value::bool(true)).is_err());
        assert!(Value::str("a").partial_cmp(&Value::bool(false)).is_err());
        assert!(Value::array([]).partial_cmp(&Value::i64(1)).is_err());
    }

    #[test]
    fn test_expr_value_equality() {
        assert_value_eq(Value::str("a"), Value::str("a"), true);
        assert_value_eq(Value::str("a"), Value::str("b"), false);
        assert_value_eq(Value::i64(1), Value::f64(1.0), true);
        assert_value_eq(Value::i64(1), Value::f64(1.5), false);
        assert_value_eq(Value::null(), Value::str("a"), false);
        assert_value_eq(
            Value::array([Value::i64(1), Value::str("a")]),
            Value::array([Value::i64(1), Value::str("a")]),
            true,
        );
        assert_value_eq(
            Value::array([Value::i64(1), Value::str("a")]),
            Value::array([Value::i64(1), Value::str("b")]),
            false,
        );
    }

    #[derive(Debug)]
    struct IntUserdata(i64);

    impl Userdata for IntUserdata {
        fn as_any(&self) -> &dyn Any {
            self
        }

        fn partial_eq(&self, other: &Value) -> Result<bool> {
            match other {
                Value::I64(other) => Ok(self.0 == *other),
                Value::Userdata(other) => Ok(other
                    .as_any()
                    .downcast_ref::<IntUserdata>()
                    .is_some_and(|other| self.0 == other.0)),
                _ => Ok(false),
            }
        }

        fn partial_ord(&self, other: &Value) -> Result<Option<Ordering>> {
            match other {
                Value::I64(other) => Ok(self.0.partial_cmp(other)),
                Value::Userdata(other) => Ok(other
                    .as_any()
                    .downcast_ref::<IntUserdata>()
                    .and_then(|other| self.0.partial_cmp(&other.0))),
                _ => Ok(None),
            }
        }
    }

    #[test]
    fn test_expr_value_userdata_delegation() {
        let userdata_inner: Arc<dyn Userdata> = Arc::new(IntUserdata(2));
        let userdata = Value::Userdata(Arc::clone(&userdata_inner));
        let same_userdata = Value::Userdata(userdata_inner);
        let equal_userdata = Value::Userdata(Arc::new(IntUserdata(2)));
        let greater_userdata = Value::Userdata(Arc::new(IntUserdata(3)));

        assert_value_eq(userdata.clone(), Value::i64(2), true);
        assert_value_eq(Value::i64(2), userdata.clone(), true);
        assert_value_eq(userdata.clone(), same_userdata, true);
        assert_value_eq(userdata.clone(), equal_userdata.clone(), true);

        assert_cmp(userdata.clone(), Value::i64(1), Ordering::Greater);
        assert_cmp(Value::i64(1), userdata.clone(), Ordering::Less);
        assert_cmp(userdata.clone(), equal_userdata, Ordering::Equal);
        assert_cmp(userdata, greater_userdata, Ordering::Less);
    }
}
