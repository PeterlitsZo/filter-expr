use async_trait::async_trait;

use crate::Expr;

type BoxedError = Box<dyn std::error::Error>;

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
    /// Transform an expression.
    async fn transform(&mut self, expr: Expr, ctx: TransformContext) -> TransformResult 
    where
        Self: Sized;
}

/// The result of transforming an expression.
pub enum TransformResult {
    /// Continue transforming the expression.  The children of the expression
    /// will be transformed.
    Continue(Expr),
    /// Stop transforming the expression.  It means we will not transform the
    /// children of the expression.
    Stop(Expr),
    /// An error occurred while transforming the expression.
    Err(BoxedError),
}

/// The context of transforming an expression.
#[derive(Debug, Clone)]
pub struct TransformContext {
    /// The depth of the expression.  The root expression has a depth of 0.
    pub depth: usize,
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
                async fn transform(&mut self, expr: Expr, _ctx: TransformContext) -> TransformResult {
                    TransformResult::Continue(match expr {
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

            #[derive(Debug, thiserror::Error)]
            pub enum Error {
                #[error("unexpected arguments length, expected {expected}, actual {actual}")]
                UnexpectedArgumentsLength {
                    expected: usize,
                    actual: usize,
                },
            }

            #[async_trait::async_trait]
            impl Transform for FooTransformer {
                async fn transform(&mut self, expr: Expr, _ctx: TransformContext) -> TransformResult {
                    TransformResult::Continue(match expr {
                        Expr::FuncCall(fn_name, args) if fn_name == "is_not_bad" => {
                            if args.len() != 1 {
                                return TransformResult::Err(Box::new(Error::UnexpectedArgumentsLength {
                                    expected: 1,
                                    actual: args.len(),
                                }));
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

    #[tokio::test]
    async fn test_early_stop() {
        mod early_stop {
            use super::*;

            pub struct EarlyStopTransformer;

            #[async_trait::async_trait]
            impl Transform for EarlyStopTransformer {
                async fn transform(&mut self, expr: Expr, ctx: TransformContext) -> TransformResult {
                    fn transform_magic_eq_expr(expr: Expr) -> Expr {
                        if let Expr::Eq(ref left, ref right) = expr {
                            let left_is_field_magic = matches!(left.as_ref(), Expr::Field(field) if field == "magic");
                            let right_is_field_foobar = matches!(right.as_ref(), Expr::Str(s) if s == "foobar");
                            if left_is_field_magic && right_is_field_foobar {
                                // Ignore the `magic = "foobar"` condition.
                                return Expr::bool_(true);
                            }
                        }
                        expr
                    }

                    // Only transform the root expression.
                    if ctx.depth == 0 {
                        match expr {
                            Expr::Eq(..) => {
                                return TransformResult::Stop(transform_magic_eq_expr(expr))
                            }
                            Expr::And(exprs) => {
                                let transformed_exprs: Vec<Expr> = exprs.into_iter().map(transform_magic_eq_expr).collect();
                                let result = Expr::and_(transformed_exprs);
                                return TransformResult::Stop(result)
                            }
                            _ => return TransformResult::Stop(expr),
                        }
                    }

                    // Not root -- just ignore.
                    TransformResult::Stop(expr)
                }
            }
        }

        let expr = Expr::and_([
            Expr::eq_(Expr::field_("magic"), Expr::str_("foobar")),
            Expr::eq_(Expr::field_("magic"), Expr::str_("baz")),
        ]);

        let mut transformer = early_stop::EarlyStopTransformer;
        let result = expr.transform(&mut transformer).await.unwrap();
        assert_eq!(result, Expr::and_([
            Expr::bool_(true),
            Expr::eq_(Expr::field_("magic"), Expr::str_("baz")),
        ]));

        let expr = Expr::eq_(Expr::field_("magic"), Expr::str_("foobar"));

        let mut transformer = early_stop::EarlyStopTransformer;
        let result = expr.transform(&mut transformer).await.unwrap();
        assert_eq!(result, Expr::bool_(true));

        let expr = Expr::or_([
            Expr::eq_(Expr::field_("magic"), Expr::str_("foobar")),
            Expr::bool_(true),
        ]);

        let mut transformer = early_stop::EarlyStopTransformer;
        let result = expr.transform(&mut transformer).await.unwrap();

        assert_eq!(result, Expr::or_([
            Expr::eq_(Expr::field_("magic"), Expr::str_("foobar")),
            Expr::bool_(true),
        ]));
    }
}
