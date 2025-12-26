use std::sync::{Arc, Mutex};

use filter_expr::Expr;

use crate::{Context, Error, ExprFnContext, FilterExprEvalerCache, Value, ValueType};

/// A runner for executing AST.
pub(crate) struct AstRunner<'a> {
    /// The expr (AST) to run.
    expr: &'a Expr,
    /// The evaler cache.
    evaler_cache: Arc<Mutex<FilterExprEvalerCache>>,
}

impl<'a> AstRunner<'a> {
    /// Create a new AST runner from expr.
    pub(crate) fn new(expr: &'a Expr, evaler_cache: Arc<Mutex<FilterExprEvalerCache>>) -> Self {
        Self { expr, evaler_cache }
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
                    _ => Err(Error::TypeMismatch(format!("{value:?}"), "bool".to_string())),
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
        let func = ctx.get_fn(func);

        // Call the function or call the builtin function.
        if let Some(func) = func {
            return func.call(ExprFnContext { args: args_values }).await;
        } else {
            match func_name {
                "matches" => self.eval_builtin_func_call_matches(&args_values).await,
                "type" => self.eval_builtin_func_call_type(&args_values).await,
                _ => Err(Error::NoSuchFunction {
                    function: func_name.to_string(),
                }),
            }
        }
    }

    async fn eval_method_call(
        &self,
        obj: &Expr,
        method: &str,
        args: &[Expr],
        ctx: &dyn Context,
    ) -> Result<Value, Error> {
        let obj_value = Box::pin(self.eval_expr(obj, ctx)).await?;
        match obj_value {
            Value::Str(s) => match method {
                "to_uppercase" => {
                    if !args.is_empty() {
                        return Err(Error::InvalidArgumentCountForMethod {
                            method: method.to_string(),
                            expected: 0,
                            got: args.len(),
                        });
                    }
                    Ok(Value::str(s.to_uppercase()))
                }
                "to_lowercase" => {
                    if !args.is_empty() {
                        return Err(Error::InvalidArgumentCountForMethod {
                            method: method.to_string(),
                            expected: 0,
                            got: args.len(),
                        });
                    }
                    Ok(Value::str(s.to_lowercase()))
                }
                "contains" => {
                    if args.len() != 1 {
                        return Err(Error::InvalidArgumentCountForMethod {
                            method: method.to_string(),
                            expected: 1,
                            got: args.len(),
                        });
                    }
                    let arg = Box::pin(self.eval_expr(&args[0], ctx)).await?;
                    let arg = match arg {
                        Value::Str(s) => s,
                        _ => {
                            return Err(Error::InvalidArgumentTypeForMethod {
                                method: method.to_string(),
                                index: 0,
                                expected: ValueType::Str,
                                got: arg.typ(),
                            });
                        }
                    };

                    Ok(Value::bool(s.contains(arg.as_str())))
                }
                "starts_with" => {
                    if args.len() != 1 {
                        return Err(Error::InvalidArgumentCountForMethod {
                            method: method.to_string(),
                            expected: 1,
                            got: args.len(),
                        });
                    }

                    let arg = Box::pin(self.eval_expr(&args[0], ctx)).await?;
                    let arg = match arg {
                        Value::Str(s) => s,
                        _ => {
                            return Err(Error::InvalidArgumentTypeForMethod {
                                method: method.to_string(),
                                index: 0,
                                expected: ValueType::Str,
                                got: arg.typ(),
                            });
                        }
                    };

                    Ok(Value::bool(s.starts_with(arg.as_str())))
                }
                "ends_with" => {
                    if args.len() != 1 {
                        return Err(Error::InvalidArgumentCountForMethod {
                            method: method.to_string(),
                            expected: 1,
                            got: args.len(),
                        });
                    }

                    let arg = Box::pin(self.eval_expr(&args[0], ctx)).await?;
                    let arg = match arg {
                        Value::Str(s) => s,
                        _ => {
                            return Err(Error::InvalidArgumentTypeForMethod {
                                method: method.to_string(),
                                index: 0,
                                expected: ValueType::Str,
                                got: arg.typ(),
                            });
                        }
                    };

                    Ok(Value::bool(s.ends_with(arg.as_str())))
                }
                _ => Err(Error::NoSuchMethod {
                    method: method.to_string(),
                    obj_type: ValueType::Str,
                }),
            },
            _ => Err(Error::NoSuchMethod {
                method: method.to_string(),
                obj_type: obj_value.typ(),
            }),
        }
    }

    async fn eval_array(&self, array: &[Expr], ctx: &dyn Context) -> Result<Value, Error> {
        let mut values = vec![];
        for expr in array {
            let value = Box::pin(self.eval_expr(expr, ctx)).await?;
            values.push(value);
        }
        Ok(Value::array(values))
    }

    async fn eval_builtin_func_call_matches(&self, args: &[Value]) -> Result<Value, Error> {
        if args.len() != 2 {
            return Err(Error::InvalidArgumentCountForFunction {
                function: "matches".to_string(),
                expected: 2,
                got: args.len(),
            });
        }
        let text = match &args[0] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::InvalidArgumentTypeForFunction {
                    function: "matches".to_string(),
                    index: 0,
                    expected: ValueType::Str,
                    got: args[0].typ(),
                });
            }
        };
        let pattern = match &args[1] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::InvalidArgumentTypeForFunction {
                    function: "matches".to_string(),
                    index: 1,
                    expected: ValueType::Str,
                    got: args[1].typ(),
                });
            }
        };
        let cached_regex = self
            .evaler_cache
            .lock()
            .map_err(|e| Error::Internal(format!("failed to lock evaler cache: {e}")))?;
        let pattern_regex = cached_regex.cached_regex.get(pattern.as_str()).cloned();
        drop(cached_regex);

        let matches = match pattern_regex {
            Some(pattern) => pattern.is_match(text),
            None => {
                let pattern = regex::Regex::new(pattern.as_str())
                    .map_err(|e| Error::Internal(format!("failed to compile regex: {e}")))?;
                let mut cached_regex = self
                    .evaler_cache
                    .lock()
                    .map_err(|e| Error::Internal(format!("failed to lock evaler cache: {e}")))?;
                cached_regex
                    .cached_regex
                    .insert(pattern.to_string(), Arc::new(pattern.clone()));
                pattern.is_match(text)
            }
        };

        Ok(Value::Bool(matches))
    }

    async fn eval_builtin_func_call_type(&self, args: &[Value]) -> Result<Value, Error> {
        if args.len() != 1 {
            return Err(Error::InvalidArgumentCountForFunction {
                function: "type".to_string(),
                expected: 1,
                got: args.len(),
            });
        }

        Ok(Value::str(match args[0].typ() {
            ValueType::Str => "str",
            ValueType::I64 => "i64",
            ValueType::F64 => "f64",
            ValueType::Bool => "bool",
            ValueType::Null => "null",
            ValueType::Array => "array",
        }))
    }
}
