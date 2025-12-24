use crate::{Error, ExprFnContext, ExprValue, ExprValueType, ctx::Context};

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    Field(String),
    Str(String),
    I64(i64),
    F64(f64),
    Bool(bool),
    Null,
    Array(Vec<Expr>),

    FuncCall(String, Vec<Expr>),
    MethodCall(String, Box<Expr>, Vec<Expr>),

    Gt(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),
    Ge(Box<Expr>, Box<Expr>),
    Le(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Ne(Box<Expr>, Box<Expr>),
    In(Box<Expr>, Box<Expr>),

    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(Box<Expr>),
}

#[allow(unused)]
impl Expr {
    pub(crate) fn field_<T: Into<String>>(field: T) -> Self {
        Self::Field(field.into())
    }

    pub(crate) fn str_<T: Into<String>>(value: T) -> Self {
        Self::Str(value.into())
    }

    pub(crate) fn i64_<T: Into<i64>>(value: T) -> Self {
        Self::I64(value.into())
    }

    pub(crate) fn f64_<T: Into<f64>>(value: T) -> Self {
        Self::F64(value.into())
    }

    pub(crate) fn bool_<T: Into<bool>>(value: T) -> Self {
        Self::Bool(value.into())
    }

    pub(crate) fn null_() -> Self {
        Self::Null
    }

    pub(crate) fn array_<T: Into<Vec<Expr>>>(value: T) -> Self {
        Self::Array(value.into())
    }

    pub(crate) fn func_call_(func: String, args: Vec<Expr>) -> Self {
        Self::FuncCall(func, args)
    }

    pub(crate) fn method_call_(obj: Expr, method: String, args: Vec<Expr>) -> Self {
        Self::MethodCall(method, Box::new(obj), args)
    }

    pub(crate) fn gt_(left: Expr, right: Expr) -> Self {
        Self::Gt(Box::new(left), Box::new(right))
    }

    pub(crate) fn lt_(left: Expr, right: Expr) -> Self {
        Self::Lt(Box::new(left), Box::new(right))
    }

    pub(crate) fn ge_(left: Expr, right: Expr) -> Self {
        Self::Ge(Box::new(left), Box::new(right))
    }

    pub(crate) fn le_(left: Expr, right: Expr) -> Self {
        Self::Le(Box::new(left), Box::new(right))
    }

    pub(crate) fn eq_(left: Expr, right: Expr) -> Self {
        Self::Eq(Box::new(left), Box::new(right))
    }

    pub(crate) fn ne_(left: Expr, right: Expr) -> Self {
        Self::Ne(Box::new(left), Box::new(right))
    }

    pub(crate) fn in_(left: Expr, right: Expr) -> Self {
        Self::In(Box::new(left), Box::new(right))
    }

    pub(crate) fn and_<T: Into<Vec<Expr>>>(value: T) -> Self {
        Self::And(value.into())
    }

    pub(crate) fn or_<T: Into<Vec<Expr>>>(value: T) -> Self {
        Self::Or(value.into())
    }

    pub(crate) fn not_(self) -> Self {
        Self::Not(Box::new(self))
    }
}

impl Expr {
    pub(crate) async fn eval(&self, ctx: &dyn Context) -> Result<ExprValue, Error> {
        match self {
            Self::Field(field) => {
                let value = ctx.get_var(field).await?;
                match value {
                    Some(value) => Ok(value),
                    None => Err(Error::NoSuchVar {
                        var: field.to_string(),
                    }),
                }
            }
            Self::Str(value) => Ok(ExprValue::Str(value.clone())),
            Self::I64(value) => Ok(ExprValue::I64(value.clone())),
            Self::F64(value) => Ok(ExprValue::F64(value.clone())),
            Self::Bool(value) => Ok(ExprValue::Bool(value.clone())),
            Self::Null => Ok(ExprValue::Null),
            Self::Array(value) => self.eval_array(value, ctx).await,

            Self::FuncCall(func, args) => self.eval_func_call(func, args, ctx).await,
            Self::MethodCall(method, obj, args) => {
                self.eval_method_call(obj, method, args, ctx).await
            }

            Self::Gt(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(ordering == std::cmp::Ordering::Greater)),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Self::Lt(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                match left_value.partial_cmp(&right_value) {
                    Some(ordering) => Ok(ExprValue::Bool(ordering == std::cmp::Ordering::Less)),
                    None => Err(Error::TypeMismatch(
                        format!("{:?}", left_value),
                        format!("{:?}", right_value),
                    )),
                }
            }
            Self::Ge(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
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
            Self::Le(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
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
            Self::Eq(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                Ok(ExprValue::Bool(left_value == right_value))
            }
            Self::Ne(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                Ok(ExprValue::Bool(left_value != right_value))
            }
            Self::In(left, right) => {
                let left_value = Box::pin(left.eval(ctx)).await?;
                let right_value = Box::pin(right.eval(ctx)).await?;
                match right_value {
                    ExprValue::Array(array) => Ok(ExprValue::Bool(array.contains(&left_value))),
                    _ => Err(Error::TypeMismatch(
                        format!("{:?}", right_value),
                        format!("{:?}", left_value),
                    )),
                }
            }

            Self::And(exprs) => {
                let mut result = true;
                for expr in exprs {
                    let value = Box::pin(expr.eval(ctx)).await?;
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
            Self::Or(exprs) => {
                let mut result = false;
                for expr in exprs {
                    let value = Box::pin(expr.eval(ctx)).await?;
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
            Self::Not(expr) => {
                let value = Box::pin(expr.eval(ctx)).await?;
                match value {
                    ExprValue::Bool(b) => Ok(ExprValue::Bool(!b)),
                    _ => Err(Error::TypeMismatch(format!("{:?}", value), format!("bool"))),
                }
            }
        }
    }

    pub(crate) async fn eval_array(
        &self,
        array: &[Expr],
        ctx: &dyn Context,
    ) -> Result<ExprValue, Error> {
        let mut values = vec![];
        for expr in array {
            let value = Box::pin(expr.eval(ctx)).await?;
            values.push(value);
        }
        Ok(ExprValue::Array(values))
    }

    pub(crate) async fn eval_method_call(
        &self,
        obj: &Expr,
        method: &str,
        args: &[Expr],
        ctx: &dyn Context,
    ) -> Result<ExprValue, Error> {
        let obj_value = Box::pin(obj.eval(ctx)).await?;
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
                    let arg = Box::pin(args[0].eval(ctx)).await?;
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

    pub(crate) async fn eval_func_call(
        &self,
        func: &str,
        args: &[Expr],
        ctx: &dyn Context,
    ) -> Result<ExprValue, Error> {
        // Evaluate the arguments.
        let mut args_values = vec![];
        for arg in args {
            let value = Box::pin(arg.eval(ctx)).await?;
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

/// A trait for transforming AST expressions.
///
/// This trait allows you to recursively transform expressions by visiting
/// all sub-expressions. The `transform` method is called recursively on all
/// sub-expressions, allowing you to transform the AST in a composable way.
///
/// # Example
///
/// ```rust
/// use filter_expr::{Expr, Transform};
///
/// struct MyTransformer;
///
/// impl Transform for MyTransformer {
///     fn transform(&mut self, expr: Expr) -> Expr {
///         // Transform the expression before recursing
///         match expr {
///             Expr::Field(name) if name == "old_name" => {
///                 Expr::Field("new_name".to_string())
///             }
///             other => other,
///         }
///     }
/// }
/// ```
pub trait Transform {
    /// Transform an expression by recursively transforming all sub-expressions.
    fn transform(&mut self, expr: Expr) -> Expr
    where
        Self: Sized;
}

impl Expr {
    /// Recursively transform an expression using the provided transformer.
    ///
    /// ```rust
    /// use filter_expr::{Expr, Transform};
    ///
    /// struct MyTransformer;
    ///
    /// impl Transform for MyTransformer {
    ///     fn transform(&mut self, expr: Expr) -> Expr {
    ///         // Transform the expression before recursing
    ///         match expr {
    ///             Expr::Field(name) if name == "old_name" => {
    ///                 Expr::Field("new_name".to_string())
    ///             }
    ///             other => other,
    ///         }
    ///     }
    /// }
    ///
    /// let expr = Expr::Field("old_name".to_string());
    /// let mut transformer = MyTransformer;
    /// let result = expr.transform(&mut transformer);
    /// assert_eq!(result, Expr::Field("new_name".to_string()));
    /// ```
    pub fn transform<F: Transform>(self, transformer: &mut F) -> Expr {
        let this = transformer.transform(self);

        match this {
            Expr::Field(name) => Expr::Field(name),
            Expr::Str(value) => Expr::Str(value),
            Expr::I64(value) => Expr::I64(value),
            Expr::F64(value) => Expr::F64(value),
            Expr::Bool(value) => Expr::Bool(value),
            Expr::Null => Expr::Null,
            Expr::Array(value) => Expr::Array(value),

            Expr::FuncCall(func, args) => {
                let args = args
                    .into_iter()
                    .map(|arg| transformer.transform(arg))
                    .collect();
                Expr::FuncCall(func, args)
            }
            Expr::MethodCall(method, obj, args) => {
                let obj = Box::new(transformer.transform(*obj));
                let args = args
                    .into_iter()
                    .map(|arg| transformer.transform(arg))
                    .collect();
                Expr::MethodCall(method, obj, args)
            }

            Expr::Gt(left, right) => {
                let left = Box::new(transformer.transform(*left));
                let right = Box::new(transformer.transform(*right));
                Expr::Gt(left, right)
            }
            Expr::Lt(left, right) => {
                let left = Box::new(transformer.transform(*left));
                let right = Box::new(transformer.transform(*right));
                Expr::Lt(left, right)
            }
            Expr::Ge(left, right) => {
                let left = Box::new(transformer.transform(*left));
                let right = Box::new(transformer.transform(*right));
                Expr::Ge(left, right)
            }
            Expr::Le(left, right) => {
                let left = Box::new(transformer.transform(*left));
                let right = Box::new(transformer.transform(*right));
                Expr::Le(left, right)
            }
            Expr::Eq(left, right) => {
                let left = Box::new(transformer.transform(*left));
                let right = Box::new(transformer.transform(*right));
                Expr::Eq(left, right)
            }
            Expr::Ne(left, right) => {
                let left = Box::new(transformer.transform(*left));
                let right = Box::new(transformer.transform(*right));
                Expr::Ne(left, right)
            }
            Expr::In(left, right) => {
                let left = Box::new(transformer.transform(*left));
                let right = Box::new(transformer.transform(*right));
                Expr::In(left, right)
            }
            Expr::And(exprs) => {
                let exprs = exprs
                    .into_iter()
                    .map(|e| transformer.transform(e))
                    .collect();
                Expr::And(exprs)
            }
            Expr::Or(exprs) => {
                let exprs = exprs
                    .into_iter()
                    .map(|e| transformer.transform(e))
                    .collect();
                Expr::Or(exprs)
            }
            Expr::Not(expr) => {
                let expr = Box::new(transformer.transform(*expr));
                Expr::Not(expr)
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_expr_value_ordering() {
        // Test string ordering.
        assert!(ExprValue::Str("a".to_string()) < ExprValue::Str("b".to_string()));
        assert!(ExprValue::Str("a".to_string()) <= ExprValue::Str("a".to_string()));
        assert!(ExprValue::Str("b".to_string()) > ExprValue::Str("a".to_string()));

        // Test integer ordering.
        assert!(ExprValue::I64(1) < ExprValue::I64(2));
        assert!(ExprValue::I64(1) <= ExprValue::I64(1));
        assert!(ExprValue::I64(2) > ExprValue::I64(1));

        // Test float ordering.
        assert!(ExprValue::F64(1.0) < ExprValue::F64(2.0));
        assert!(ExprValue::F64(1.0) <= ExprValue::F64(1.0));
        assert!(ExprValue::F64(2.0) > ExprValue::F64(1.0));

        // Test boolean ordering.
        assert!(ExprValue::Bool(false) < ExprValue::Bool(true));
        assert!(ExprValue::Bool(false) <= ExprValue::Bool(false));
        assert!(ExprValue::Bool(true) > ExprValue::Bool(false));

        // Test Int and Float comparison.
        assert!(ExprValue::I64(1) < ExprValue::F64(2.0));
        assert!(ExprValue::I64(2) > ExprValue::F64(1.0));
        assert!(ExprValue::F64(1.0) < ExprValue::I64(2));
        assert!(ExprValue::F64(2.0) > ExprValue::I64(1));

        // Test Null ordering.
        assert!(ExprValue::Null == ExprValue::Null);
        assert!(ExprValue::Null > ExprValue::Str("a".to_string()));
        assert!(ExprValue::Str("a".to_string()) < ExprValue::Null);
        assert!(ExprValue::Null > ExprValue::I64(1));
        assert!(ExprValue::I64(1) < ExprValue::Null);

        // Test array ordering.
        let arr1 = ExprValue::Array(vec![ExprValue::I64(1), ExprValue::I64(2)]);
        let arr2 = ExprValue::Array(vec![ExprValue::I64(1), ExprValue::I64(3)]);
        assert!(arr1 < arr2);
        assert!(arr1 <= arr1);
        assert!(arr2 > arr1);

        // Test incompatible types (should return None).
        assert!(
            ExprValue::Str("a".to_string())
                .partial_cmp(&ExprValue::I64(1))
                .is_none()
        );
        assert!(
            ExprValue::I64(1)
                .partial_cmp(&ExprValue::Bool(true))
                .is_none()
        );
        assert!(
            ExprValue::Str("a".to_string())
                .partial_cmp(&ExprValue::Bool(false))
                .is_none()
        );
        assert!(
            ExprValue::Array(vec![])
                .partial_cmp(&ExprValue::I64(1))
                .is_none()
        );
    }

    #[test]
    fn test_transform_expr() {
        // Example: Rename field "old_name" to "new_name"
        struct RenameField {
            old_name: String,
            new_name: String,
        }

        impl Transform for RenameField {
            fn transform(&mut self, expr: Expr) -> Expr {
                match expr {
                    Expr::Field(name) if name == self.old_name => {
                        Expr::Field(self.new_name.clone())
                    }
                    _ => expr,
                }
            }
        }

        let expr = Expr::Eq(
            Box::new(Expr::field_("old_name")),
            Box::new(Expr::str_("value")),
        );

        let mut transformer = RenameField {
            old_name: "old_name".to_string(),
            new_name: "new_name".to_string(),
        };

        let result = expr.transform(&mut transformer);
        assert_eq!(
            result,
            Expr::Eq(
                Box::new(Expr::field_("new_name")),
                Box::new(Expr::str_("value"))
            )
        );
    }
}
