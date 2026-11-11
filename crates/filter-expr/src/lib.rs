//! A library for parsing the filter expression.

pub use num_bigint::BigInt;

mod error;
mod expr;
mod parser;
mod token;
mod transform;

use std::fmt::Debug;

use expr::ExprDebug;

pub use compact_str::CompactString;
pub use error::Error;
pub use expr::{Expr, ExprArena, ExprId};
pub use transform::{Transform, TransformContext, TransformResult};

/// The language semantics used to interpret an expression.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub enum LanguageVersion {
    #[default]
    V1,
}

/// The filter expression.
#[derive(Clone)]
pub struct FilterExpr {
    /// The expression of the filter. Possibly empty.
    expr: Option<Expr>,
    arena: ExprArena,
    language_version: LanguageVersion,
}

impl Debug for FilterExpr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self.expr {
            Some(expr) => write!(f, "{:?}", ExprDebug(expr, &self.arena)),
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
            return Ok(Self::new(None, ExprArena::default()));
        }

        let mut parser = parser::Parser::new(expr)?;
        let expr = if parser.is_empty() {
            None
        } else {
            Some(parser.parse()?)
        };
        let language_version = parser.language_version();
        Ok(Self { expr, arena: parser.arena, language_version })
    }

    /// Check if the filter expression is empty.
    pub fn is_empty(&self) -> bool {
        self.expr.is_none()
    }

    /// Create a new filter expression with the given expression.
    pub fn new(expr: Option<Expr>, arena: ExprArena) -> Self {
        Self { expr, arena, language_version: LanguageVersion::V1 }
    }

    pub fn language_version(&self) -> LanguageVersion {
        self.language_version
    }

    /// Get the expression of the filter.
    pub fn expr(&self) -> Option<&Expr> {
        self.expr.as_ref()
    }

    /// Transform the filter expression.
    pub async fn transform<F: Transform>(mut self, transformer: &mut F) -> Result<Self, Error> {
        if let Some(expr) = self.expr.take() {
            self.expr = Some(expr.transform(&mut self.arena, transformer).await?);
        }
        Ok(self)
    }

    pub fn node(&self, id: ExprId) -> Option<&Expr> {
        self.arena.get(id)
    }
}

impl PartialEq for FilterExpr {
    fn eq(&self, other: &Self) -> bool {
        if self.language_version != other.language_version {
            return false;
        }
        match (&self.expr, &other.expr) {
            (Some(a), Some(b)) => a.eq_in(b, &self.arena, &other.arena),
            (None, None) => true,
            _ => false,
        }
    }
}
