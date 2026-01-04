use async_trait::async_trait;

use crate::Error;

/// The expression.
///
/// It is an AST of the filter expression.
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

    pub(crate) fn func_call_(func: impl Into<String>, args: Vec<Expr>) -> Self {
        Self::FuncCall(func.into(), args)
    }

    pub(crate) fn method_call_(obj: Expr, method: impl Into<String>, args: Vec<Expr>) -> Self {
        Self::MethodCall(method.into(), Box::new(obj), args)
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
/// use async_trait::async_trait;
///
/// struct MyTransformer;
///
/// #[async_trait]
/// impl Transform for MyTransformer {
///     async fn transform(&mut self, expr: Expr) -> Result<Expr, filter_expr::Error> {
///         // Transform the expression before recursing
///         Ok(match expr {
///             Expr::Field(name) if name == "old_name" => {
///                 Expr::Field("new_name".to_string())
///             }
///             other => other,
///         })
///     }
/// }
/// ```
#[async_trait]
pub trait Transform {
    /// Transform an expression by recursively transforming all sub-expressions.
    async fn transform(&mut self, expr: Expr) -> Result<Expr, Error>
    where
        Self: Sized;
}

impl Expr {
    /// Recursively transform an expression using the provided transformer.
    ///
    /// ```rust
    /// use filter_expr::{Expr, Transform};
    /// use async_trait::async_trait;
    ///
    /// struct MyTransformer;
    ///
    /// #[async_trait]
    /// impl Transform for MyTransformer {
    ///     async fn transform(&mut self, expr: Expr) -> Result<Expr, filter_expr::Error> {
    ///         // Transform the expression before recursing
    ///         Ok(match expr {
    ///             Expr::Field(name) if name == "old_name" => {
    ///                 Expr::Field("new_name".to_string())
    ///             }
    ///             other => other,
    ///         })
    ///     }
    /// }
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let expr = Expr::Field("old_name".to_string());
    /// let mut transformer = MyTransformer;
    /// let result = expr.transform(&mut transformer).await.unwrap();
    /// assert_eq!(result, Expr::Field("new_name".to_string()));
    /// # }
    /// ```
    pub async fn transform<F: Transform>(self, transformer: &mut F) -> Result<Expr, Error> {
        let this = transformer.transform(self).await?;

        Ok(match this {
            Expr::Field(name) => Expr::Field(name),
            Expr::Str(value) => Expr::Str(value),
            Expr::I64(value) => Expr::I64(value),
            Expr::F64(value) => Expr::F64(value),
            Expr::Bool(value) => Expr::Bool(value),
            Expr::Null => Expr::Null,
            Expr::Array(value) => Expr::Array(value),

            Expr::FuncCall(func, args) => {
                let mut transformed_args = Vec::new();
                for arg in args {
                    transformed_args.push(transformer.transform(arg).await?);
                }
                Expr::FuncCall(func, transformed_args)
            }
            Expr::MethodCall(method, obj, args) => {
                let obj = Box::new(transformer.transform(*obj).await?);
                let mut transformed_args = Vec::new();
                for arg in args {
                    transformed_args.push(transformer.transform(arg).await?);
                }
                Expr::MethodCall(method, obj, transformed_args)
            }

            Expr::Gt(left, right) => {
                let left = Box::new(transformer.transform(*left).await?);
                let right = Box::new(transformer.transform(*right).await?);
                Expr::Gt(left, right)
            }
            Expr::Lt(left, right) => {
                let left = Box::new(transformer.transform(*left).await?);
                let right = Box::new(transformer.transform(*right).await?);
                Expr::Lt(left, right)
            }
            Expr::Ge(left, right) => {
                let left = Box::new(transformer.transform(*left).await?);
                let right = Box::new(transformer.transform(*right).await?);
                Expr::Ge(left, right)
            }
            Expr::Le(left, right) => {
                let left = Box::new(transformer.transform(*left).await?);
                let right = Box::new(transformer.transform(*right).await?);
                Expr::Le(left, right)
            }
            Expr::Eq(left, right) => {
                let left = Box::new(transformer.transform(*left).await?);
                let right = Box::new(transformer.transform(*right).await?);
                Expr::Eq(left, right)
            }
            Expr::Ne(left, right) => {
                let left = Box::new(transformer.transform(*left).await?);
                let right = Box::new(transformer.transform(*right).await?);
                Expr::Ne(left, right)
            }
            Expr::In(left, right) => {
                let left = Box::new(transformer.transform(*left).await?);
                let right = Box::new(transformer.transform(*right).await?);
                Expr::In(left, right)
            }
            Expr::And(exprs) => {
                let mut transformed_exprs = Vec::new();
                for e in exprs {
                    transformed_exprs.push(transformer.transform(e).await?);
                }
                Expr::And(transformed_exprs)
            }
            Expr::Or(exprs) => {
                let mut transformed_exprs = Vec::new();
                for e in exprs {
                    transformed_exprs.push(transformer.transform(e).await?);
                }
                Expr::Or(transformed_exprs)
            }
            Expr::Not(expr) => {
                let expr = Box::new(transformer.transform(*expr).await?);
                Expr::Not(expr)
            }
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_transform_expr_to_rename_field() {
        mod rename_field {
            use super::*;

            pub struct RenameFieldTransformer {
                pub old_name: String,
                pub new_name: String,
            }

            #[async_trait::async_trait]
            impl Transform for RenameFieldTransformer {
                async fn transform(&mut self, expr: Expr) -> Result<Expr, Error> {
                    Ok(match expr {
                        Expr::Field(name) if name == self.old_name => {
                            Expr::Field(self.new_name.clone())
                        }
                        _ => expr,
                    })
                }
            }
        }

        let expr = Expr::eq_(
            Expr::field_("old_name"),
            Expr::str_("value"),
        );

        let mut transformer = rename_field::RenameFieldTransformer {
            old_name: "old_name".to_string(),
            new_name: "new_name".to_string(),
        };

        let result = expr.transform(&mut transformer).await.unwrap();
        assert_eq!(
            result,
            Expr::Eq(
                Box::new(Expr::field_("new_name")),
                Box::new(Expr::str_("value"))
            )
        );
    }

    #[tokio::test]
    async fn test_transform_expr_to_load_datas_from_external_datasource() {
        mod foo {
            use std::time::Duration;

            use super::*;

            pub struct FooTransformer;

            #[async_trait::async_trait]
            impl Transform for FooTransformer {
                async fn transform(&mut self, expr: Expr) -> Result<Expr, Error> {
                    Ok(match expr {
                        Expr::FuncCall(fn_name, args) if fn_name == "is_not_bad" => {
                            if args.len() != 1 {
                                return Err(Error::Transform("expected 1 argument".to_string()));
                            }
                            let datas = load_datas().await;
                            Expr::In(Box::new(args[0].clone()), Box::new(Expr::Array(datas)))
                        }
                        _ => expr,
                    })
                }
            }

            async fn load_datas() -> Vec<Expr> {
                tokio::time::sleep(Duration::from_millis(100)).await;

                vec![
                    Expr::Str("foo".to_string()),
                    Expr::Str("bar".to_string()),
                ]
            }
        }

        let expr = Expr::and_([
            Expr::func_call_("is_not_bad", vec![Expr::field_("magic")]),
            Expr::func_call_("is_not_bad", vec![Expr::field_("foobar")]),
        ]);

        let mut transformer = foo::FooTransformer;
        let result = expr.transform(&mut transformer).await.unwrap();
        assert_eq!(result, Expr::and_([
            Expr::in_(Expr::field_("magic"), Expr::array_([Expr::str_("foo"), Expr::str_("bar")])),
            Expr::in_(Expr::field_("foobar"), Expr::array_([Expr::str_("foo"), Expr::str_("bar")])),
        ]));
    }
}
