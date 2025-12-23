//! A library for parsing the filter expression.

mod ctx;
mod error;
mod expr;
mod parser;
mod token;

pub use ctx::{SimpleContext, Context};
pub use error::Error;
pub use expr::{ExprValue, BoxedExprFn, ExprFn};

use crate::expr::Expr;

/// The filter expression.
pub struct FilterExpr {
    /// The expression of the filter. Possibly empty.
    #[allow(unused)]
    expr: Option<Expr>,
}

impl FilterExpr {
    pub fn parse(expr: &str) -> Result<Self, Error> {
        let expr = parse_expr(expr)?;
        Ok(Self { expr: Some(expr) })
    }

    pub async fn eval(&self, ctx: &dyn Context) -> Result<bool, Error> {
        if let Some(expr) = &self.expr {
            let value = expr.eval(ctx).await?;
            match value {
                ExprValue::Bool(b) => Ok(b),
                _ => Err(Error::InvalidValue(format!("{:?}", value))),
            }
        } else {
            Ok(true)
        }
    }
}

fn parse_expr(input: &str) -> Result<Expr, Error> {
    let tokens = token::parse_token(input)?;
    let mut parser = parser::Parser::new(tokens);
    Ok(parser.parse_expr()?)
}

#[cfg(test)]
mod tests {
    use crate::expr::{ExprFn, ExprFnContext};

    use super::*;

    #[tokio::test]
    async fn test_parse_expr_and_then_eval() {
        // Parse the filter-expr:
        //
        //     name = 'John' AND age > 18 AND 1 > 0
        // =====================================================================

        let input = "name = 'John' AND age > 18 AND 1 > 0";
        let filter_expr = FilterExpr::parse(input).unwrap();

        let ctx = simple_context! {
            "name": "John",
            "age": 19,
        };
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true);

        let ctx = simple_context! {
            "name": "John",
            "age": 18,
        };
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, false);

        // Parse the filter-expr:
        //
        //     name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0
        // =====================================================================

        let input = r#"name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0"#;
        let filter_expr = FilterExpr::parse(input).unwrap();

        let ctx = simple_context! {
            "name": "John",
            "age": 19,
        };
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true.into());

        let ctx = simple_context! {
            "name": "John",
            "age": 23,
        };
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, false.into());

        // Parse the filter-expr:
        //
        //     matches(name, "^J.*n$")
        // =====================================================================

        let input = r#"matches(name, "^J.*n$")"#;
        let filter_expr = FilterExpr::parse(input).unwrap();

        let ctx = simple_context! {
            "name": "John",
        };
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true);

        let ctx = simple_context! {
            "name": "Jane",
        };
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, false);

        // Parse the filter-expr:
        //
        //     custom_add(1, 2) = 3
        // =====================================================================

        let input = r#"custom_add(1, 2) = a"#;
        let filter_expr = FilterExpr::parse(input).unwrap();

        struct CustomAddFn;
        #[async_trait::async_trait]
        impl ExprFn for CustomAddFn {
            async fn call(&self, ctx: ExprFnContext) -> Result<ExprValue, Error> {
                if ctx.args.len() != 2 {
                    return Err(Error::InvalidArgumentCount {
                        expected: 2,
                        got: ctx.args.len(),
                    });
                }
                let a = match ctx.args[0] {
                    ExprValue::Int(a) => a,
                    _ => {
                        return Err(Error::InvalidArgumentType {
                            expected: "integer".to_string(),
                            got: format!("{:?}", ctx.args[0]),
                        });
                    }
                };
                let b = match ctx.args[1] {
                    ExprValue::Int(b) => b,
                    _ => {
                        return Err(Error::InvalidArgumentType {
                            expected: "integer".to_string(),
                            got: format!("{:?}", ctx.args[1]),
                        });
                    }
                };
                Ok((a + b).into())
            }
        }

        let mut ctx = simple_context! {
            "a": 3,
        };
        ctx.add_fn("custom_add".to_string(), Box::new(CustomAddFn));
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true);

        let mut ctx = simple_context! {
            "a": 4,
        };
        ctx.add_fn("custom_add".to_string(), Box::new(CustomAddFn));
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, false);

        // Parse the filter-expr:
        //
        //     name != null
        // =====================================================================

        let input = r#"name != null"#;
        let filter_expr = FilterExpr::parse(input).unwrap();

        let ctx = simple_context! {
            "name": ExprValue::Null,
        };
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, false);

        let ctx = simple_context! {
            "name": "John",
        };
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true);

        // Parse the filter-expr:
        //
        //     open > 1.5 AND age > 17.5 AND age < 18.5 AND is_peter = true
        // =====================================================================

        let input = r#"open > 1.5 AND age > 17.5 AND age < 18.5 AND is_peter = true"#;
        let filter_expr = FilterExpr::parse(input).unwrap();

        let ctx = simple_context! {
            "open": 1.6,
            "age": 18,
            "is_peter": true,
        };
        let result = filter_expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true);
    }
}
