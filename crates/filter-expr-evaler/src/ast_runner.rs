use filter_expr::Expr;

use crate::{Context, Error, FilterExprEvalerEnv, FunctionContext, MethodContext, Value};

/// A runner for executing AST.
pub(crate) struct AstRunner<'a> {
    /// The expr (AST) to run.
    expr: &'a Expr,
    /// The evaler environment.
    env: FilterExprEvalerEnv,
}

impl<'a> AstRunner<'a> {
    /// Create a new AST runner from expr.
    pub(crate) fn new(expr: &'a Expr, env: FilterExprEvalerEnv) -> Self {
        Self { expr, env }
    }

    /// Run the expr and return the result.
    pub(crate) async fn run(&self, ctx: &dyn Context) -> Result<Value, Error> {
        return self.eval_expr(self.expr, ctx).await;
    }

    async fn eval_expr(&self, expr: &Expr, ctx: &dyn Context) -> Result<Value, Error> {
        match expr {
            Expr::Field(field) => {
                let value = ctx
                    .get_var(field)
                    .await
                    .map_err(|e| Error::FailedToGetVar {
                        var: field.to_string(),
                        error: e.to_string(),
                    })?;
                match value {
                    Some(value) => Ok(value),
                    None => Err(Error::NoSuchVar {
                        var: field.to_string(),
                    }),
                }
            }
            Expr::FieldAccess(obj, field) => {
                todo!()
            }

            Expr::Str(value) => Ok(Value::str(value.clone())),
            Expr::I64(value) => Ok(Value::i64(*value)),
            Expr::F64(value) => Ok(Value::f64(*value)),
            Expr::Bool(value) => Ok(Value::bool(*value)),
            Expr::Null => Ok(Value::null()),

            Expr::Array(value) => self.eval_array(value, ctx).await,

            Expr::FuncCall(func, args) => self.eval_func_call(func, args, ctx).await,
            Expr::MethodCall(method, obj, args) => {
                self.eval_method_call(obj, method, args, ctx).await
            }

            Expr::Gt(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(Value::Bool(ordering == std::cmp::Ordering::Greater)),
                    None => Err(Error::TypeMismatch(
                        format!("{left_value:?}"),
                        format!("{right_value:?}"),
                    )),
                }
            }
            Expr::Lt(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(Value::Bool(ordering == std::cmp::Ordering::Less)),
                    None => Err(Error::TypeMismatch(
                        format!("{left_value:?}"),
                        format!("{right_value:?}"),
                    )),
                }
            }
            Expr::Ge(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(Value::Bool(
                        ordering == std::cmp::Ordering::Greater
                            || ordering == std::cmp::Ordering::Equal,
                    )),
                    None => Err(Error::TypeMismatch(
                        format!("{left_value:?}"),
                        format!("{right_value:?}"),
                    )),
                }
            }
            Expr::Le(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(Value::Bool(
                        ordering == std::cmp::Ordering::Less
                            || ordering == std::cmp::Ordering::Equal,
                    )),
                    None => Err(Error::TypeMismatch(
                        format!("{left_value:?}"),
                        format!("{right_value:?}"),
                    )),
                }
            }
            Expr::Eq(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                Ok(Value::Bool(left_value == right_value))
            }
            Expr::Ne(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                Ok(Value::Bool(left_value != right_value))
            }
            Expr::In(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match right_value {
                    Value::Array(array) => Ok(Value::Bool(array.contains(&left_value))),
                    _ => Err(Error::TypeMismatch(
                        format!("{right_value:?}"),
                        format!("{left_value:?}"),
                    )),
                }
            }

            Expr::And(exprs) => {
                let mut result = true;
                for expr in exprs {
                    let value = Box::pin(self.eval_expr(expr, ctx)).await?;
                    match value {
                        Value::Bool(b) => result = result && b,
                        _ => {
                            return Err(Error::InvalidValue(format!(
                                "expected bool, got {value:?}"
                            )));
                        }
                    }
                }
                Ok(Value::Bool(result))
            }
            Expr::Or(exprs) => {
                let mut result = false;
                for expr in exprs {
                    let value = Box::pin(self.eval_expr(expr, ctx)).await?;
                    match value {
                        Value::Bool(b) => result = result || b,
                        _ => {
                            return Err(Error::InvalidValue(format!(
                                "expected bool, got {value:?}"
                            )));
                        }
                    }
                }
                Ok(Value::Bool(result))
            }
            Expr::Not(expr) => {
                let value = Box::pin(self.eval_expr(expr, ctx)).await?;
                match value {
                    Value::Bool(b) => Ok(Value::Bool(!b)),
                    _ => Err(Error::TypeMismatch(
                        format!("{value:?}"),
                        "bool".to_string(),
                    )),
                }
            }
        }
    }

    async fn eval_func_call(
        &self,
        func: &str,
        args: &[Expr],
        ctx: &dyn Context,
    ) -> Result<Value, Error> {
        // Evaluate the arguments.
        let mut args_values = vec![];
        for arg in args {
            let value = Box::pin(self.eval_expr(arg, ctx)).await?;
            args_values.push(value);
        }

        // Get the function to call.
        let func_name = func;
        let func = self.env.get_function(func_name)?;

        // Call the function or call the builtin function.
        return func
            .call(FunctionContext {
                env: &self.env,
                args: &args_values,
            })
            .await;
    }

    async fn eval_method_call(
        &self,
        obj: &Expr,
        method: &str,
        args: &[Expr],
        ctx: &dyn Context,
    ) -> Result<Value, Error> {
        let obj_value = Box::pin(self.eval_expr(obj, ctx)).await?;
        let mut args_values = vec![];
        for arg in args {
            let value = Box::pin(self.eval_expr(arg, ctx)).await?;
            args_values.push(value);
        }

        let method = self.env.get_method(method, obj_value.typ())?;

        let result = method
            .call(MethodContext {
                env: &self.env,
                obj: &obj_value,
                args: &args_values,
            })
            .await?;
        Ok(result)
    }

    async fn eval_array(&self, array: &[Expr], ctx: &dyn Context) -> Result<Value, Error> {
        let mut values = vec![];
        for expr in array {
            let value = Box::pin(self.eval_expr(expr, ctx)).await?;
            values.push(value);
        }
        Ok(Value::array(values))
    }
}
