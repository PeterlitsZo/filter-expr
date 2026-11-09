use std::sync::Arc;

use crate::{Error, EvaluationState, FilterExprEvalerEnv, Value};

/// The context for a function call.
pub struct FunctionContext<'env, 'a> {
    pub env: &'env FilterExprEvalerEnv,

    pub state: &'a EvaluationState,

    pub args: &'a [Value],
}

/// A pure function of its arguments and the evaluation's world.
///
/// The same inputs MUST give the same value or error, without side effects.
/// Lazy loading and caching are allowed within the evaluation. Optimizations
/// may skip calls and their errors.
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

    pub state: &'a EvaluationState,

    pub obj: &'a Value,
    pub args: &'a [Value],
}

/// A pure method. Follows [`Function`]'s contract with the receiver as an
/// input.
#[async_trait::async_trait]
pub trait Method: Send + Sync {
    /// Call the method.
    async fn call(&self, ctx: MethodContext<'_, '_>) -> Result<Value, Error>;
}

/// The boxed method.
pub type ArcMethod = Arc<dyn Method>;
