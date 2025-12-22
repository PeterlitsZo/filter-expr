//! A library for parsing the filter expression.

mod ctx;
mod error;
mod expr;
mod parser;
mod token;

pub use error::Error;
pub use ctx::SimpleContext;

use crate::{ctx::Context, expr::{Expr, ExprValue}};

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
        assert_eq!(result, false.into());

        // Parse the filter-expr:
        //
        //     name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0
        // =====================================================================

        let input = r#"name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0"#;
        let expr = parse_expr(input).unwrap();
        let expected = Expr::And(vec![
            Expr::Eq(Expr::field_boxed("name"), Expr::value_boxed("John")),
            Expr::In(Expr::field_boxed("age"), Expr::value_boxed(vec![18, 19, 20, 22])),
            Expr::Gt(Expr::value_boxed(1), Expr::value_boxed(0)),
        ]);
        assert_eq!(expr, expected);

        let ctx = simple_context! {
            "name": "John",
            "age": 19,
        };
        let result = expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true.into());

        let ctx = simple_context! {
            "name": "John",
            "age": 23,
        };
        let result = expr.eval(&ctx).await.unwrap();
        assert_eq!(result, false.into());

        // Parse the filter-expr:
        //
        //     matches(name, "^J.*n$")
        // =====================================================================

        let input = r#"matches(name, "^J.*n$")"#;
        let expr = parse_expr(input).unwrap();
        let expected = Expr::FuncCall("matches".to_string(), vec![
            Expr::field("name"),
            Expr::value("^J.*n$"),
        ]);
        assert_eq!(expr, expected);

        let ctx = simple_context! {
            "name": "John",
        };
        let result = expr.eval(&ctx).await.unwrap();
        assert_eq!(result, true.into());

        let ctx = simple_context! {
            "name": "Jane",
        };
        let result = expr.eval(&ctx).await.unwrap();
        assert_eq!(result, false.into());
    }
}
