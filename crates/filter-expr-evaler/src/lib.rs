//! Evaluator for filter expressions.

mod ctx;
mod error;

use filter_expr::{Expr, ExprValue, ExprValueType, FilterExpr};

pub use crate::ctx::{Context, ExprFn, ExprFnContext, SimpleContext};
pub use crate::error::Error;

pub struct FilterExprEvaler {
    filter_expr: FilterExpr,
}

impl FilterExprEvaler {
    /// Create a new filter expression evaluator.
    pub fn new(filter_expr: FilterExpr) -> Self {
        Self { filter_expr }
    }

    /// Evaluate the filter expression in the given context.
    pub async fn eval(&self, ctx: &dyn Context) -> Result<bool, Error> {
        if let Some(expr) = self.filter_expr.expr() {
            let value = FilterExprEvalerInner::new(self, expr).eval(ctx).await?;
            match value {
                ExprValue::Bool(b) => Ok(b),
                _ => Err(Error::InvalidValue(format!("{:?}", value))),
            }
        } else {
            Ok(true)
        }
    }
}

struct FilterExprEvalerInner<'a> {
    #[allow(unused)]
    evaler: &'a FilterExprEvaler,
    expr: &'a Expr,
}

impl<'a> FilterExprEvalerInner<'a> {
    pub fn new(evaler: &'a FilterExprEvaler, expr: &'a Expr) -> Self {
        Self { evaler, expr }
    }

    pub async fn eval(&self, ctx: &dyn Context) -> Result<ExprValue, Error> {
        return self.eval_expr(self.expr, ctx).await;
    }

    async fn eval_expr(&self, expr: &Expr, ctx: &dyn Context) -> Result<ExprValue, Error> {
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
            Expr::Str(value) => Ok(ExprValue::Str(value.clone())),
            Expr::I64(value) => Ok(ExprValue::I64(value.clone())),
            Expr::F64(value) => Ok(ExprValue::F64(value.clone())),
            Expr::Bool(value) => Ok(ExprValue::Bool(value.clone())),
            Expr::Null => Ok(ExprValue::Null),
            Expr::Array(value) => self.eval_array(value, ctx).await,

            Expr::FuncCall(func, args) => self.eval_func_call(func, args, ctx).await,
            Expr::MethodCall(method, obj, args) => {
                self.eval_method_call(obj, method, args, ctx).await
            }

            Expr::Gt(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(ordering == std::cmp::Ordering::Greater)),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Expr::Lt(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(ordering == std::cmp::Ordering::Less)),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Expr::Ge(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(
                        ordering == std::cmp::Ordering::Greater
                            || ordering == std::cmp::Ordering::Equal,
                    )),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Expr::Le(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(
                        ordering == std::cmp::Ordering::Less
                            || ordering == std::cmp::Ordering::Equal,
                    )),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Expr::Eq(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                Ok(ExprValue::Bool(left_value == right_value))
            }
            Expr::Ne(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                Ok(ExprValue::Bool(left_value != right_value))
            }
            Expr::In(left, right) => {
                let left_value = Box::pin(self.eval_expr(left, ctx)).await?;
                let right_value = Box::pin(self.eval_expr(right, ctx)).await?;
                match right_value {
                    ExprValue::Array(array) => Ok(ExprValue::Bool(array.contains(&left_value))),
                    _ => Err(Error::TypeMismatch(
                        format!("{:?}", right_value),
                        format!("{:?}", left_value),
                    )),
                }
            }

            Expr::And(exprs) => {
                let mut result = true;
                for expr in exprs {
                    let value = Box::pin(self.eval_expr(expr, ctx)).await?;
                    match value {
                        ExprValue::Bool(b) => result = result && b,
                        _ => {
                            return Err(Error::InvalidValue(format!(
                                "expected bool, got {:?}",
                                value
                            )));
                        }
                    }
                }
                Ok(ExprValue::Bool(result))
            }
            Expr::Or(exprs) => {
                let mut result = false;
                for expr in exprs {
                    let value = Box::pin(self.eval_expr(expr, ctx)).await?;
                    match value {
                        ExprValue::Bool(b) => result = result || b,
                        _ => {
                            return Err(Error::InvalidValue(format!(
                                "expected bool, got {:?}",
                                value
                            )));
                        }
                    }
                }
                Ok(ExprValue::Bool(result))
            }
            Expr::Not(expr) => {
                let value = Box::pin(self.eval_expr(expr, ctx)).await?;
                match value {
                    ExprValue::Bool(b) => Ok(ExprValue::Bool(!b)),
                    _ => Err(Error::TypeMismatch(format!("{:?}", value), format!("bool"))),
                }
            }
        }
    }

    pub(crate) async fn eval_func_call(
        &self,
        func: &str,
        args: &[Expr],
        ctx: &dyn Context,
    ) -> Result<ExprValue, Error> {
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

    pub(crate) async fn eval_method_call(
        &self,
        obj: &Expr,
        method: &str,
        args: &[Expr],
        ctx: &dyn Context,
    ) -> Result<ExprValue, Error> {
        let obj_value = Box::pin(self.eval_expr(obj, ctx)).await?;
        match obj_value {
            ExprValue::Str(s) => match method {
                "to_uppercase" => {
                    if args.len() != 0 {
                        return Err(Error::InvalidArgumentCountForMethod {
                            method: method.to_string(),
                            expected: 0,
                            got: args.len(),
                        });
                    }
                    Ok(ExprValue::Str(s.to_uppercase()))
                }
                "to_lowercase" => {
                    if args.len() != 0 {
                        return Err(Error::InvalidArgumentCountForMethod {
                            method: method.to_string(),
                            expected: 0,
                            got: args.len(),
                        });
                    }
                    Ok(ExprValue::Str(s.to_lowercase()))
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
                        ExprValue::Str(s) => s,
                        _ => {
                            return Err(Error::InvalidArgumentTypeForMethod {
                                method: method.to_string(),
                                index: 0,
                                expected: ExprValueType::Str,
                                got: arg.typ(),
                            });
                        }
                    };

                    Ok(ExprValue::Bool(s.contains(&arg)))
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
                        ExprValue::Str(s) => s,
                        _ => {
                            return Err(Error::InvalidArgumentTypeForMethod {
                                method: method.to_string(),
                                index: 0,
                                expected: ExprValueType::Str,
                                got: arg.typ(),
                            });
                        }
                    };

                    Ok(ExprValue::Bool(s.starts_with(&arg)))
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
                        ExprValue::Str(s) => s,
                        _ => {
                            return Err(Error::InvalidArgumentTypeForMethod {
                                method: method.to_string(),
                                index: 0,
                                expected: ExprValueType::Str,
                                got: arg.typ(),
                            });
                        }
                    };

                    Ok(ExprValue::Bool(s.ends_with(&arg)))
                }
                _ => Err(Error::NoSuchMethod {
                    method: method.to_string(),
                    obj_type: ExprValueType::Str,
                }),
            },
            _ => Err(Error::NoSuchMethod {
                method: method.to_string(),
                obj_type: obj_value.typ(),
            }),
        }
    }

    pub(crate) async fn eval_array(
        &self,
        array: &[Expr],
        ctx: &dyn Context,
    ) -> Result<ExprValue, Error> {
        let mut values = vec![];
        for expr in array {
            let value = Box::pin(self.eval_expr(expr, ctx)).await?;
            values.push(value);
        }
        Ok(ExprValue::Array(values))
    }

    pub(crate) async fn eval_builtin_func_call_matches(
        &self,
        args: &[ExprValue],
    ) -> Result<ExprValue, Error> {
        if args.len() != 2 {
            return Err(Error::InvalidArgumentCountForFunction {
                function: "matches".to_string(),
                expected: 2,
                got: args.len(),
            });
        }
        let text = match &args[0] {
            ExprValue::Str(s) => s,
            _ => {
                return Err(Error::InvalidArgumentTypeForFunction {
                    function: "matches".to_string(),
                    index: 0,
                    expected: ExprValueType::Str,
                    got: args[0].typ(),
                });
            }
        };
        let pattern = match &args[1] {
            ExprValue::Str(s) => s,
            _ => {
                return Err(Error::InvalidArgumentTypeForFunction {
                    function: "matches".to_string(),
                    index: 1,
                    expected: ExprValueType::Str,
                    got: args[1].typ(),
                });
            }
        };
        let pattern = regex::Regex::new(&pattern);
        let pattern = match pattern {
            Ok(pattern) => pattern,
            Err(e) => {
                return Err(Error::Internal(format!("failed to compile regex: {}", e)));
            }
        };

        let matches = pattern.is_match(&text);
        Ok(ExprValue::Bool(matches))
    }

    pub(crate) async fn eval_builtin_func_call_type(
        &self,
        args: &[ExprValue],
    ) -> Result<ExprValue, Error> {
        if args.len() != 1 {
            return Err(Error::InvalidArgumentCountForFunction {
                function: "type".to_string(),
                expected: 1,
                got: args.len(),
            });
        }

        Ok(ExprValue::Str(
            match args[0].typ() {
                ExprValueType::Str => "str",
                ExprValueType::I64 => "i64",
                ExprValueType::F64 => "f64",
                ExprValueType::Bool => "bool",
                ExprValueType::Null => "null",
                ExprValueType::Array => "array",
            }
            .to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use crate::ctx::ExprFn;

    use super::*;

    #[tokio::test]
    async fn test_parse_and_then_eval() {
        // Parse the filter-expr:
        //
        //     name = 'John' AND age > 18 AND 1 > 0
        // =====================================================================
        parse_and_do_test_cases(
            "name = 'John' AND age > 18 AND 1 > 0",
            vec![
                (simple_context! { "name": "John", "age": 19 }, true),
                (simple_context! { "name": "John", "age": 18 }, false),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0
        // =====================================================================
        parse_and_do_test_cases(
            r#"name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0"#,
            vec![
                (simple_context! { "name": "John", "age": 19 }, true),
                (simple_context! { "name": "John", "age": 23 }, false),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     matches(name, "^J.*n$")
        // =====================================================================
        parse_and_do_test_cases(
            r#"matches(name, "^J.*n$")"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Jane" }, false),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     custom_add(1, 2) = 3
        // =====================================================================
        fn with_custom_add_fn(mut ctx: SimpleContext) -> SimpleContext {
            ctx.add_fn("custom_add".to_string(), Box::new(CustomAddFn));
            ctx
        }
        parse_and_do_test_cases(
            r#"custom_add(a, b) = 3"#,
            vec![
                (with_custom_add_fn(simple_context! { "a": 1, "b": 2 }), true),
                (
                    with_custom_add_fn(simple_context! { "a": 1, "b": 3 }),
                    false,
                ),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name != null
        // =====================================================================
        parse_and_do_test_cases(
            r#"name != null"#,
            vec![
                (simple_context! { "name": ExprValue::Null }, false),
                (simple_context! { "name": "John" }, true),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     open > 1.5 AND age > 17.5 AND age < 18.5 AND is_peter = true
        // =====================================================================
        parse_and_do_test_cases(
            r#"open > 1.5 AND age > 17.5 AND age < 18.5 AND is_peter = true"#,
            vec![(
                simple_context! { "open": 1.6, "age": 18, "is_peter": true },
                true,
            )],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name.to_uppercase() = 'JOHN'
        // =====================================================================
        parse_and_do_test_cases(
            r#"name.to_uppercase() = 'JOHN'"#,
            vec![
                (simple_context! { "name": "john" }, true),
                (simple_context! { "name": "Jane" }, false),
                (simple_context! { "name": "John" }, true),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name.contains('John')
        // =====================================================================
        parse_and_do_test_cases(
            r#"name.contains('John')"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Jane" }, false),
                (simple_context! { "name": "The John is a good boy." }, true),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     type(name) = 'str'
        //     type(name) = 'null'
        //     type(foo.contains('bar')) = 'bool'
        //     type(age) = 'i64'
        //     type(open) = 'f64'
        //     type(maybe_i64_or_f64) IN ['i64', 'f64']
        // =====================================================================
        parse_and_do_test_cases(
            r#"type(name) = 'str'"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": 18 }, false),
                (simple_context! { "name": ExprValue::Null }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(name) = 'null'"#,
            vec![
                (simple_context! { "name": "John" }, false),
                (simple_context! { "name": ExprValue::Null }, true),
                (simple_context! { "name": 18 }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(foo.contains('bar')) = 'bool'"#,
            vec![
                (simple_context! { "foo": "foobar" }, true),
                (simple_context! { "foo": "bar and foo" }, true),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(age) = 'i64'"#,
            vec![
                (simple_context! { "age": 18 }, true),
                (simple_context! { "age": 18.5 }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(open) = 'f64'"#,
            vec![
                (simple_context! { "open": 18 }, false),
                (simple_context! { "open": 18.5 }, true),
                (simple_context! { "open": "18" }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(maybe_i64_or_f64) IN ['i64', 'f64']"#,
            vec![
                (simple_context! { "maybe_i64_or_f64": 18 }, true),
                (simple_context! { "maybe_i64_or_f64": 18.5 }, true),
                (simple_context! { "maybe_i64_or_f64": "18" }, false),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name.starts_with('J')
        //     name.ends_with('n')
        // =====================================================================
        parse_and_do_test_cases(
            r#"name.starts_with('J')"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Peterlits" }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"name.ends_with('n')"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Jane" }, false),
            ],
        )
        .await;
    }

    async fn parse_and_do_test_cases(input: &str, test_cases: Vec<(SimpleContext, bool)>) {
        let filter_expr = FilterExpr::parse(input).expect(&format!("failed to parse: {}", input));
        let evaler = FilterExprEvaler::new(filter_expr);

        for (ctx, expected) in test_cases {
            let result = evaler.eval(&ctx).await.unwrap();
            assert_eq!(result, expected, "{} failed with context {:?}", input, ctx);
        }
    }

    struct CustomAddFn;

    #[async_trait::async_trait]
    impl ExprFn for CustomAddFn {
        async fn call(&self, ctx: ExprFnContext) -> Result<ExprValue, Error> {
            if ctx.args.len() != 2 {
                return Err(Error::InvalidArgumentCountForFunction {
                    function: "custom_add".to_string(),
                    expected: 2,
                    got: ctx.args.len(),
                });
            }
            let a = match ctx.args[0] {
                ExprValue::I64(a) => a,
                _ => {
                    return Err(Error::InvalidArgumentTypeForFunction {
                        function: "custom_add".to_string(),
                        index: 0,
                        expected: ExprValueType::I64,
                        got: ctx.args[0].typ(),
                    });
                }
            };
            let b = match ctx.args[1] {
                ExprValue::I64(b) => b,
                _ => {
                    return Err(Error::InvalidArgumentTypeForFunction {
                        function: "custom_add".to_string(),
                        index: 1,
                        expected: ExprValueType::I64,
                        got: ctx.args[1].typ(),
                    });
                }
            };
            Ok((a + b).into())
        }
    }
}
