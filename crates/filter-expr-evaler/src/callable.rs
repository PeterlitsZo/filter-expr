use std::sync::Arc;

use crate::{Error, FilterExprEvalerEnv, Value};

/// The context for a function call.
pub struct FunctionContext<'env, 'a> {
    pub env: &'env FilterExprEvalerEnv,

    pub args: &'a [Value],
}

/// The trait for a function.
#[async_trait::async_trait]
pub trait Function: Send + Sync {
    /// Call the function.
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error>;
}

/// The boxed function.
pub type ArcFunction = Arc<dyn Function>;

/// The context for a method call.
pub struct MethodContext<'env, 'a> {
    pub env: &'env FilterExprEvalerEnv,

    pub obj: &'a Value,
    pub args: &'a [Value],
}

/// The trait for a method.
#[async_trait::async_trait]
pub trait Method: Send + Sync {
    /// Call the method.
    async fn call(&self, ctx: MethodContext<'_, '_>) -> Result<Value, Error>;
}

/// The boxed method.
pub type ArcMethod = Arc<dyn Method>;
