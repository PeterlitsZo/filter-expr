//! A library for parsing the filter expression.

mod error;
mod expr;
mod parser;
mod token;
mod transform;

use std::fmt::Debug;

pub use error::Error;
pub use expr::Expr;
pub use transform::{Transform, TransformContext, TransformResult};

/// The filter expression.
#[derive(Clone, PartialEq)]
pub struct FilterExpr {
    /// The expression of the filter. Possibly empty.
    expr: Option<Expr>,
}

impl Debug for FilterExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expr {
            Some(expr) => write!(f, "{:?}", expr),
            None => write!(f, "Empty"),
        }
    }
}

impl FilterExpr {
    /// Parse the filter expression.
    ///
    /// ```rust
    /// use filter_expr::FilterExpr;
    ///
    /// let filter_expr = FilterExpr::parse("name = 'John' AND age > 18").unwrap();
    /// ```
    pub fn parse(expr: &str) -> Result<Self, Error> {
        if expr.trim().is_empty() {
            return Ok(Self { expr: None });
        }

        let expr = parse_expr(expr)?;
        Ok(Self { expr: Some(expr) })
    }

    /// Check if the filter expression is empty.
    pub fn is_empty(&self) -> bool {
        self.expr.is_none()
    }

    /// Create a new filter expression with the given expression.
    pub fn new(expr: Option<Expr>) -> Self {
        Self { expr }
    }

    /// Get the expression of the filter.
    pub fn expr(&self) -> Option<&Expr> {
        self.expr.as_ref()
    }

    /// Transform the filter expression.
    pub async fn transform<F: Transform>(self, transformer: &mut F) -> Result<Self, Error> {
        Ok(Self {
            expr: match self.expr {
                Some(expr) => Some(expr.transform(transformer).await?),
                None => None,
            },
        })
    }
}

fn parse_expr(input: &str) -> Result<Expr, Error> {
    let tokens = token::parse_token(input)?;
    let mut parser = parser::Parser::new(tokens);
    parser.parse_expr()
}
