use std::collections::HashMap;

use crate::{Error, expr::ExprValue};

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
        $crate::SimpleContext::new(std::collections::HashMap::new())
    };
    
    // Single key-value pair.
    ($key:literal : $value:expr $(,)?) => {
        $crate::SimpleContext::new(std::collections::HashMap::from([
            ($key.to_string(), ($value).into()),
        ]))
    };
    
    // Multiple key-value pairs.
    ($key:literal : $value:expr, $($rest_key:literal : $rest_value:expr),* $(,)?) => {
        $crate::SimpleContext::new(std::collections::HashMap::from([
            ($key.to_string(), ($value).into()),
            $(($rest_key.to_string(), ($rest_value).into())),*
        ]))
    };
}

/// The context of the filter for evaluation.
#[async_trait::async_trait]
pub trait Context: Send + Sync {
    async fn get_var(&self, name: &str) -> Result<ExprValue, Error>;
}

/// A simple context that stores the variables in a hash map.
/// 
/// For those who don't want to implement the `Context` trait, this is a simple
/// implementation that can be used.
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
