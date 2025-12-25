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
