use std::{
    collections::BTreeMap,
    fmt::{self, Debug},
};

use crate::Value;

use crate::Error;

/// The context of the filter for evaluation.
#[async_trait::async_trait]
pub trait Context: Send + Sync {
    /// Get the value of a variable/field.
    ///
    /// It is a async function so that the callee can even request the database
    /// or network to get the value.
    ///
    /// If the variable is not found, return `None`.
    ///
    /// If some error occurs, return the error.
    async fn get_var(&self, name: &str) -> Result<Option<Value>, Error>;
}

/// A macro to create a `SimpleContext` from key-value pairs.
///
/// # Example
///
/// ```rust
/// use filter_expr::simple_context;
///
/// let ctx = simple_context! {
///     "name": "John",
///     "age": 19,
/// };
/// ```
#[macro_export]
macro_rules! simple_context {
    // Empty case.
    () => {
        $crate::SimpleContext::new(std::collections::BTreeMap::new())
    };

    // Single key-value pair.
    ($key:literal : $value:expr $(,)?) => {
        $crate::SimpleContext::new(std::collections::BTreeMap::from([
            ($key.to_string(), ($value).into()),
        ]))
    };

    // Multiple key-value pairs.
    ($key:literal : $value:expr, $($rest_key:literal : $rest_value:expr),* $(,)?) => {
        $crate::SimpleContext::new(std::collections::BTreeMap::from([
            ($key.to_string(), ($value).into()),
            $(($rest_key.to_string(), ($rest_value).into())),*
        ]))
    };
}

/// A simple context that stores the variables in a hash map.
///
/// For those who don't want to implement the `Context` trait, this is a simple
/// implementation that can be used.
pub struct SimpleContext {
    vars: BTreeMap<String, Value>,
}

impl Debug for SimpleContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "SimpleContext {{ vars: {:?} }}", self.vars)?;
        Ok(())
    }
}

impl SimpleContext {
    pub fn new(vars: BTreeMap<String, Value>) -> Self {
        Self { vars }
    }
}

#[async_trait::async_trait]
impl Context for SimpleContext {
    async fn get_var(&self, name: &str) -> Result<Option<Value>, Error> {
        Ok(self.vars.get(name).cloned())
    }
}
