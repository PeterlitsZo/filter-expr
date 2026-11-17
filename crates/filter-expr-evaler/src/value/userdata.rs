use std::{any::Any, cmp::Ordering, fmt::Debug};

use crate::Result;

use super::Value;

/// A custom value. Comparisons and field reads follow [`crate::Function`]'s
/// purity contract.
#[async_trait::async_trait]
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
    async fn get_field(&self, _field: &str) -> Result<Option<Value>> {
        Ok(None)
    }
}
