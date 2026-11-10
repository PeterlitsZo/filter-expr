use async_trait::async_trait;

use crate::{Expr, ExprArena};

type BoxedError = Box<dyn std::error::Error + Send + Sync>;

/// A trait for transforming AST expressions.
///
/// This trait allows you to recursively transform expressions by visiting
/// all sub-expressions. The `transform` method is called recursively on all
/// sub-expressions, allowing you to transform the AST in a composable way.
///
/// # Example
///
/// ```rust
/// use filter_expr::{Expr, ExprArena, Transform, TransformContext, TransformResult};
/// use async_trait::async_trait;
///
/// struct MyTransformer;
///
/// #[async_trait]
/// impl Transform for MyTransformer {
///     async fn transform(&mut self, expr: Expr, _arena: &mut ExprArena, _ctx: TransformContext) -> TransformResult {
///         // Transform the expression before recursing
///         TransformResult::Continue(match expr {
///             Expr::Field(name) if name == "old_name" => {
///                 Expr::Field("new_name".into())
///             }
///             other => other,
///         })
///     }
/// }
/// ```
#[async_trait]
pub trait Transform {
    /// Transform an expression.
    async fn transform(&mut self, expr: Expr, arena: &mut ExprArena, ctx: TransformContext) -> TransformResult
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
    macro_rules! alloc {
        ($arena:expr, $expr:expr) => {{
            let expr = $expr;
            $arena.alloc(expr)
        }};
    }

    use super::*;

    #[tokio::test]
    async fn test_transform_expr_to_rename_field() {
        let mut arena = ExprArena::default();
        mod rename_field {
            use super::*;

            pub struct RenameFieldTransformer {
                pub old_name: String,
                pub new_name: String,
            }

            #[async_trait::async_trait]
            impl Transform for RenameFieldTransformer {
                async fn transform(
                    &mut self,
                    expr: Expr,
                    _arena: &mut ExprArena,
                    _ctx: TransformContext,
                ) -> TransformResult {
                    TransformResult::Continue(match expr {
                        Expr::Field(name) if name == self.old_name => {
                            Expr::Field(self.new_name.as_str().into())
                        }
                        _ => expr,
                    })
                }
            }
        }

        let expr = Expr::eq_(
            alloc!(arena, Expr::field_("old_name")),
            alloc!(arena, Expr::str_("value")),
        );

        let mut transformer = rename_field::RenameFieldTransformer {
            old_name: "old_name".to_string(),
            new_name: "new_name".to_string(),
        };

        let result = expr.transform(&mut arena, &mut transformer).await.unwrap();
        {
            let expected = Expr::Eq(
                alloc!(arena, Expr::field_("new_name")),
                alloc!(arena, Expr::str_("value")),
            );
            assert!(result.eq_in(&expected, &arena, &arena));
        };
    }

    #[tokio::test]
    async fn test_transform_expr_to_load_datas_from_external_datasource() {
        let mut arena = ExprArena::default();
        mod foo {
            use std::time::Duration;

            use super::*;

            pub struct FooTransformer;

            #[derive(Debug, thiserror::Error)]
            pub enum Error {
                #[error("unexpected arguments length, expected {expected}, actual {actual}")]
                UnexpectedArgumentsLength { expected: usize, actual: usize },
            }

            #[async_trait::async_trait]
            impl Transform for FooTransformer {
                async fn transform(
                    &mut self,
                    expr: Expr,
                    arena: &mut ExprArena,
                    _ctx: TransformContext,
                ) -> TransformResult {
                    TransformResult::Continue(match expr {
                        Expr::FuncCall(fn_name, args) if fn_name == "is_not_bad" => {
                            if args.len() != 1 {
                                return TransformResult::Err(Box::new(
                                    Error::UnexpectedArgumentsLength {
                                        expected: 1,
                                        actual: args.len(),
                                    },
                                ));
                            }
                            let datas = load_datas().await;
                            Expr::In(
                                alloc!(arena, args[0].clone()),
                                alloc!(arena, Expr::Array(datas)),
                            )
                        }
                        _ => expr,
                    })
                }
            }

            async fn load_datas() -> Vec<Expr> {
                tokio::time::sleep(Duration::from_millis(100)).await;

                vec![Expr::Str("foo".into()), Expr::Str("bar".into())]
            }
        }

        let expr = Expr::and_([
            Expr::func_call_("is_not_bad", vec![Expr::field_("magic")]),
            Expr::func_call_("is_not_bad", vec![Expr::field_("foobar")]),
        ]);

        let mut transformer = foo::FooTransformer;
        let result = expr.transform(&mut arena, &mut transformer).await.unwrap();
        {
            let expected = Expr::and_([
                Expr::in_(
                    alloc!(arena, Expr::field_("magic")),
                    alloc!(arena, Expr::array_([Expr::str_("foo"), Expr::str_("bar")])),
                ),
                Expr::in_(
                    alloc!(arena, Expr::field_("foobar")),
                    alloc!(arena, Expr::array_([Expr::str_("foo"), Expr::str_("bar")])),
                ),
            ]);
            assert!(result.eq_in(&expected, &arena, &arena));
        };
    }

    #[tokio::test]
    async fn test_early_stop() {
        let mut arena = ExprArena::default();
        mod early_stop {
            use super::*;

            pub struct EarlyStopTransformer;

            #[async_trait::async_trait]
            impl Transform for EarlyStopTransformer {
                async fn transform(
                    &mut self,
                    expr: Expr,
                    arena: &mut ExprArena,
                    ctx: TransformContext,
                ) -> TransformResult {
                    fn transform_magic_eq_expr(expr: Expr, arena: &ExprArena) -> Expr {
                        if let Expr::Eq(ref left, ref right) = expr {
                            let left_is_field_magic = matches!(arena.get(*left).unwrap(), Expr::Field(field) if field == "magic");
                            let right_is_field_foobar =
                                matches!(arena.get(*right).unwrap(), Expr::Str(s) if s == "foobar");
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
                                return TransformResult::Stop(transform_magic_eq_expr(expr, arena));
                            }
                            Expr::And(exprs) => {
                                let transformed_exprs: Vec<Expr> = exprs
                                    .into_iter()
                                    .map(|expr| transform_magic_eq_expr(expr, arena))
                                    .collect();
                                let result = Expr::and_(transformed_exprs);
                                return TransformResult::Stop(result);
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
            Expr::eq_(
                alloc!(arena, Expr::field_("magic")),
                alloc!(arena, Expr::str_("foobar")),
            ),
            Expr::eq_(
                alloc!(arena, Expr::field_("magic")),
                alloc!(arena, Expr::str_("baz")),
            ),
        ]);

        let mut transformer = early_stop::EarlyStopTransformer;
        let result = expr.transform(&mut arena, &mut transformer).await.unwrap();
        {
            let expected = Expr::and_([
                Expr::bool_(true),
                Expr::eq_(
                    alloc!(arena, Expr::field_("magic")),
                    alloc!(arena, Expr::str_("baz")),
                ),
            ]);
            assert!(result.eq_in(&expected, &arena, &arena));
        };

        let expr = Expr::eq_(
            alloc!(arena, Expr::field_("magic")),
            alloc!(arena, Expr::str_("foobar")),
        );

        let mut transformer = early_stop::EarlyStopTransformer;
        let result = expr.transform(&mut arena, &mut transformer).await.unwrap();
        {
            let expected = Expr::bool_(true);
            assert!(result.eq_in(&expected, &arena, &arena));
        };

        let expr = Expr::or_([
            Expr::eq_(
                alloc!(arena, Expr::field_("magic")),
                alloc!(arena, Expr::str_("foobar")),
            ),
            Expr::bool_(true),
        ]);

        let mut transformer = early_stop::EarlyStopTransformer;
        let result = expr.transform(&mut arena, &mut transformer).await.unwrap();

        {
            let expected = Expr::or_([
                Expr::eq_(
                    alloc!(arena, Expr::field_("magic")),
                    alloc!(arena, Expr::str_("foobar")),
                ),
                Expr::bool_(true),
            ]);
            assert!(result.eq_in(&expected, &arena, &arena));
        };
    }
}
