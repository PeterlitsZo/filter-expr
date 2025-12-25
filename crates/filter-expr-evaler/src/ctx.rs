use std::{collections::HashMap, fmt::{self, Debug}};

use filter_expr::ExprValue;

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
    async fn get_var(&self, name: &str) -> Result<Option<ExprValue>, Error>;

    /// Get the function by name.
    /// 
    /// If the function is not found, return `None`.
    fn get_fn(&self, name: &str) -> Option<&BoxedExprFn> {
        let _name = name;

        None
    }
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

/// A simple context that stores the variables in a hash map.
/// 
/// For those who don't want to implement the `Context` trait, this is a simple
/// implementation that can be used.
pub struct SimpleContext {
    vars: HashMap<String, ExprValue>,
    fns: HashMap<String, BoxedExprFn>,
}

impl Debug for SimpleContext {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let fns_len = self.fns.len();
        write!(f, "SimpleContext {{ vars: {:?}, fns: [{:?} fns] }}", self.vars, fns_len)?;
        Ok(())
    }
}

impl SimpleContext {
    pub fn new(vars: HashMap<String, ExprValue>) -> Self {
        Self {
            vars,
            fns: HashMap::new(),
        }
    }

    pub fn add_fn(&mut self, name: String, func: BoxedExprFn) {
        self.fns.insert(name, func);
    }
}

#[async_trait::async_trait]
impl Context for SimpleContext {
    async fn get_var(&self, name: &str) -> Result<Option<ExprValue>, Error> {
        Ok(self.vars.get(name).cloned())
    }

    fn get_fn(&self, name: &str) -> Option<&BoxedExprFn> {
        self.fns.get(name)
    }
}

pub struct ExprFnContext {
    pub args: Vec<ExprValue>,
}

#[async_trait::async_trait]
pub trait ExprFn: Send + Sync {
    async fn call(&self, ctx: ExprFnContext) -> Result<ExprValue, Error>;
}

pub type BoxedExprFn = Box<dyn ExprFn>;
