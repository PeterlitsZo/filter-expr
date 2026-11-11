use std::{
    any::Any, cmp::Ordering, collections::BTreeMap, fmt::{Debug, Display}, sync::Arc,
};

use num_bigint::BigInt;
use num_traits::{FromPrimitive, ToPrimitive, Zero};

use crate::{Error, ErrorKind, Result};

#[derive(Clone, Copy)]
pub(crate) enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
}

#[derive(Debug, Clone)]
pub enum Value {
    /// The string value.
    Str(Arc<String>),
    /// The integer value.
    I64(i64),
    /// An integer outside the small-integer representation.
    BigInt(Arc<BigInt>),
    /// The float value.
    F64(f64),
    /// The boolean value.
    Bool(bool),
    /// An absolute instant in time.
    DateTime(jiff::Timestamp),
    /// The null value.
    Null,

    /// The array value.
    Array(Arc<Vec<Value>>),
    /// The object value.
    Object(Arc<BTreeMap<String, Value>>),

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

impl From<u64> for Value {
    fn from(value: u64) -> Self {
        match i64::try_from(value) {
            Ok(value) => Self::I64(value),
            Err(_) => Self::bigint(value),
        }
    }
}

impl From<i32> for Value {
    fn from(value: i32) -> Self {
        Self::I64(i64::from(value))
    }
}

impl From<BigInt> for Value {
    fn from(value: BigInt) -> Self {
        Self::bigint(value)
    }
}

impl From<f64> for Value {
    fn from(val: f64) -> Self {
        Value::F64(val)
    }
}

impl From<jiff::Timestamp> for Value {
    fn from(value: jiff::Timestamp) -> Self {
        Self::DateTime(value)
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

impl<T: Into<Value>> From<BTreeMap<String, T>> for Value {
    fn from(val: BTreeMap<String, T>) -> Self {
        Value::Object(Arc::new(val.into_iter().map(|(k, v)| (k, v.into())).collect()))
    }
}

impl Value {
    pub fn str(s: impl Into<String>) -> Self {
        Value::Str(Arc::new(s.into()))
    }

    pub fn i64(i: i64) -> Self {
        Value::I64(i)
    }

    pub fn bigint(value: impl Into<BigInt>) -> Self {
        let value = value.into();
        match value.to_i64() {
            Some(value) => Self::I64(value),
            None => Self::BigInt(Arc::new(value)),
        }
    }

    pub fn f64(f: f64) -> Self {
        Value::F64(f)
    }

    pub fn bool(b: bool) -> Self {
        Value::Bool(b)
    }

    pub fn datetime(value: jiff::Timestamp) -> Self {
        Self::DateTime(value)
    }

    pub fn null() -> Self {
        Value::Null
    }

    pub fn array(items: impl Into<Vec<Value>>) -> Self {
        Value::Array(Arc::new(items.into()))
    }

    pub fn object(items: impl Into<BTreeMap<String, Value>>) -> Self {
        Value::Object(Arc::new(items.into()))
    }
}

impl Value {
    pub fn typ(&self) -> ValueType {
        match self {
            Value::Str(_) => ValueType::Str,
            Value::I64(_) => ValueType::I64,
            Value::BigInt(_) => ValueType::BigInt,
            Value::F64(_) => ValueType::F64,
            Value::Bool(_) => ValueType::Bool,
            Value::DateTime(_) => ValueType::DateTime,
            Value::Null => ValueType::Null,
            Value::Array(_) => ValueType::Array,
            Value::Object(_) => ValueType::Object,
            Value::Userdata(_) => ValueType::Userdata,
        }
    }
}

impl Value {
    pub fn partial_eq(&self, other: &Self) -> Result<bool> {
        match (self, other) {
            (Value::Str(a), Value::Str(b)) => Ok(a == b),
            (Value::I64(a), Value::I64(b)) => Ok(a == b),
            (Value::BigInt(a), Value::BigInt(b)) => Ok(a == b),
            (Value::BigInt(a), Value::I64(b)) | (Value::I64(b), Value::BigInt(a)) => {
                Ok(a.as_ref() == &BigInt::from(*b))
            }
            (Value::BigInt(a), Value::F64(b)) | (Value::F64(b), Value::BigInt(a)) => {
                Ok(Self::compare_bigint_float(a, *b) == Some(Ordering::Equal))
            }
            (Value::F64(a), Value::F64(b)) => Ok(a == b),
            (Value::Bool(a), Value::Bool(b)) => Ok(a == b),
            (Value::DateTime(a), Value::DateTime(b)) => Ok(a == b),
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
            (Value::Object(a), Value::Object(b)) => {
                if a.len() != b.len() {
                    return Ok(false);
                }

                for ((ak, av), (bk, bv)) in a.iter().zip(b.iter()) {
                    if ak != bk {
                        return Ok(false);
                    }
                    if !av.partial_eq(bv)? {
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
            (Value::BigInt(a), Value::BigInt(b)) => Ok(a.partial_cmp(b)),
            (Value::BigInt(a), Value::I64(b)) => Ok(Some(a.as_ref().cmp(&BigInt::from(*b)))),
            (Value::I64(a), Value::BigInt(b)) => Ok(Some(BigInt::from(*a).cmp(b))),
            (Value::BigInt(a), Value::F64(b)) => Ok(Self::compare_bigint_float(a, *b)),
            (Value::F64(a), Value::BigInt(b)) => {
                Ok(Self::compare_bigint_float(b, *a).map(Ordering::reverse))
            }
            (Value::F64(a), Value::F64(b)) => Ok(a.partial_cmp(b)),
            (Value::Bool(a), Value::Bool(b)) => Ok(a.partial_cmp(b)),
            (Value::DateTime(a), Value::DateTime(b)) => Ok(a.partial_cmp(b)),
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


impl Value {
    fn compare_bigint_float(integer: &BigInt, float: f64) -> Option<Ordering> {
        if float.is_nan() {
            return None;
        }
        if float.is_infinite() {
            return Some(if float.is_sign_positive() {
                Ordering::Less
            } else {
                Ordering::Greater
            });
        }
        let truncated = BigInt::from_f64(float).unwrap();
        Some(match integer.cmp(&truncated) {
            Ordering::Equal if float.fract() > 0.0 => Ordering::Less,
            Ordering::Equal if float.fract() < 0.0 => Ordering::Greater,
            ordering => ordering,
        })
    }

    pub(crate) fn bigint_to_f64(value: &BigInt) -> Result<f64> {
        value.to_f64().filter(|value| value.is_finite()).ok_or_else(|| {
            Error::new(ErrorKind::InvalidValue, "integer is out of range for f64")
        })
    }

    pub(crate) fn numeric_sign(&self, negative: bool) -> Result<Self> {
        match self {
            Self::I64(value) if negative => Ok(match value.checked_neg() {
                Some(value) => Self::I64(value),
                None => Self::bigint(-BigInt::from(*value)),
            }),
            Self::BigInt(value) if negative => Ok(Self::bigint(-value.as_ref())),
            Self::F64(value) if negative => Ok(Self::F64(-value)),
            Self::I64(_) | Self::BigInt(_) | Self::F64(_) => Ok(self.clone()),
            _ => Err(Error::new(ErrorKind::TypeMismatch, "expected number")
                .with_metadata("got", self.typ())),
        }
    }

    fn bigint_arithmetic(left: &BigInt, right: &BigInt, op: ArithmeticOp) -> Result<Self> {
        use ArithmeticOp::*;

        if matches!(op, Div | Rem) && right.is_zero() {
            return Err(Error::new(ErrorKind::InvalidValue, "division by zero"));
        }
        Ok(Self::bigint(match op {
            Add => left + right,
            Sub => left - right,
            Mul => left * right,
            Div => left / right,
            Rem => left % right,
        }))
    }

    pub(crate) fn arithmetic(&self, other: &Self, op: ArithmeticOp) -> Result<Self> {
        use ArithmeticOp::*;

        if let (Add, Self::Str(left), Self::Str(right)) = (op, self, other) {
            let mut result = String::with_capacity(left.len() + right.len());
            result.push_str(left);
            result.push_str(right);
            return Ok(Self::str(result));
        }

        if let (Self::I64(left), Self::I64(right)) = (self, other) {
            if matches!(op, Div | Rem) && *right == 0 {
                return Err(Error::new(ErrorKind::InvalidValue, "division by zero"));
            }
            let result = match op {
                Add => left.checked_add(*right),
                Sub => left.checked_sub(*right),
                Mul => left.checked_mul(*right),
                Div => left.checked_div(*right),
                Rem => left.checked_rem(*right),
            };
            return match result {
                Some(value) => Ok(Self::I64(value)),
                None => Self::bigint_arithmetic(&BigInt::from(*left), &BigInt::from(*right), op),
            };
        }

        match (self, other) {
            (Self::BigInt(left), Self::BigInt(right)) => {
                return Self::bigint_arithmetic(left, right, op);
            }
            (Self::BigInt(left), Self::I64(right)) => {
                return Self::bigint_arithmetic(left, &BigInt::from(*right), op);
            }
            (Self::I64(left), Self::BigInt(right)) => {
                return Self::bigint_arithmetic(&BigInt::from(*left), right, op);
            }
            _ => {}
        }

        let (left, right) = match (self, other) {
            (Self::BigInt(left), Self::F64(right)) => (Self::bigint_to_f64(left)?, *right),
            (Self::F64(left), Self::BigInt(right)) => (*left, Self::bigint_to_f64(right)?),
            (Self::I64(left), Self::F64(right)) => (*left as f64, *right),
            (Self::F64(left), Self::I64(right)) => (*left, *right as f64),
            (Self::F64(left), Self::F64(right)) => (*left, *right),
            _ => {
                let message = if matches!(op, Add) {
                    "expected numeric operands or two strings"
                } else {
                    "expected numeric operands"
                };
                return Err(
                    Error::new(ErrorKind::TypeMismatch, message)
                        .with_metadata("left", self.typ())
                        .with_metadata("right", other.typ()),
                );
            }
        };
        if matches!(op, Div | Rem) && right == 0.0 {
            return Err(Error::new(ErrorKind::InvalidValue, "division by zero"));
        }
        Ok(Self::F64(match op {
            Add => left + right,
            Sub => left - right,
            Mul => left * right,
            Div => left / right,
            Rem => left % right,
        }))
    }
}

impl Value {
    pub(crate) fn get_field(&self, field: &str) -> Result<Value> {
        let value = match self {
            Value::Object(object) => object.get(field).cloned(),
            Value::Userdata(userdata) => userdata.get_field(field)?,
            _ => {
                return Err(Error::new(
                    ErrorKind::TypeMismatch,
                    "expected object or userdata for field access",
                )
                .with_metadata("field", field)
                .with_metadata("expected", "Object or Userdata")
                .with_metadata("got", self.typ()));
            }
        };
        value.ok_or_else(|| {
            Error::new(ErrorKind::FailedToGet, "no such field").with_metadata("field", field)
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum ValueType {
    Str,
    I64,
    BigInt,
    F64,
    Bool,
    DateTime,
    Null,

    Array,
    Object,

    Userdata,
}

impl Display for ValueType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

/// A custom value. Comparisons and field reads follow [`crate::Function`]'s
/// purity contract.
pub trait Userdata: Debug + Send + Sync {
    /// Return the value as `Any` for implementer-side downcasting.
    fn as_any(&self) -> &dyn Any;

    /// Compare for equality with another value.
    ///
    /// Return `Ok(true)` for equal values, `Ok(false)` for unequal values,
    /// or `Err` when comparison fails.
    fn partial_eq(&self, other: &Value) -> Result<bool>;

    /// Compare the ordering of this value relative to another value.
    ///
    /// Return `Ok(Some(ordering))` for an ordering, `Ok(None)` for unordered
    /// values, or `Err` when comparison fails.
    fn partial_ord(&self, other: &Value) -> Result<Option<Ordering>>;

    /// Read a field.
    ///
    /// Return `Ok(None)` for a missing field, `Ok(Some(Value::Null))` for an
    /// explicit null, or `Err` when reading fails.
    fn get_field(&self, _field: &str) -> Result<Option<Value>> {
        Ok(None)
    }
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
