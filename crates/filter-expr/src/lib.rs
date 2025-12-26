//! A library for parsing the filter expression.

mod error;
mod expr;
mod parser;
mod token;

pub use error::Error;
pub use expr::{Expr, Transform};

/// The filter expression.
pub struct FilterExpr {
    /// The expression of the filter. Possibly empty.
    expr: Option<Expr>,
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

    /// Create a new filter expression with the given expression.
    pub fn new(expr: Option<Expr>) -> Self {
        Self { expr }
    }

    /// Get the expression of the filter.
    pub fn expr(&self) -> Option<&Expr> {
        self.expr.as_ref()
    }

    /// Transform the filter expression.
    pub fn transform<F: Transform>(self, transformer: &mut F) -> Self {
        Self {
            expr: self.expr.map(|expr| expr.transform(transformer)),
        }
    }
}

fn parse_expr(input: &str) -> Result<Expr, Error> {
    let tokens = token::parse_token(input)?;
    let mut parser = parser::Parser::new(tokens);
    parser.parse_expr()
}
