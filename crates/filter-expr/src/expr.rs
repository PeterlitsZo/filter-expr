use std::cmp::Ordering;
use std::hash::{Hash, Hasher};

use crate::{Error, Transform, TransformContext, TransformResult};

/// The expression.
///
/// It is an AST of the filter expression.
#[derive(Debug, Clone)]
pub enum Expr {
    Field(String),
    FieldAccess(Box<Expr>, String),

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

impl PartialOrd for Expr {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Expr {
    fn cmp(&self, other: &Self) -> Ordering {
        use Expr::*;
        use ordered_float::OrderedFloat;

        // Helper function to get variant order.
        fn variant_order(expr: &Expr) -> u16 {
            use Expr::*;
            match expr {
                Field(..) => 100,
                FieldAccess(..) => 101,

                Str(..) => 200,
                I64(..) => 201,
                F64(..) => 202,
                Bool(..) => 203,
                Null => 204,

                Array(..) => 300,

                FuncCall(..) => 400,
                MethodCall(..) => 401,

                Gt(..) => 500,
                Lt(..) => 501,
                Ge(..) => 502,
                Le(..) => 503,
                Eq(..) => 504,
                Ne(..) => 505,
                In(..) => 506,

                And(..) => 600,
                Or(..) => 601,
                Not(..) => 602,
            }
        }

        // First compare by variant order.
        match variant_order(self).cmp(&variant_order(other)) {
            Ordering::Equal => {
                // Same variant, compare contents.
                match (self, other) {
                    (Field(a), Field(b)) => a.cmp(b),
                    (FieldAccess(o1, f1), FieldAccess(o2, f2)) => match o1.cmp(o2) {
                        Ordering::Equal => f1.cmp(f2),
                        other => other,
                    },

                    (Str(a), Str(b)) => a.cmp(b),
                    (I64(a), I64(b)) => a.cmp(b),
                    (F64(a), F64(b)) => OrderedFloat(*a).cmp(&OrderedFloat(*b)),
                    (Bool(a), Bool(b)) => a.cmp(b),
                    (Null, Null) => Ordering::Equal,

                    (Array(a), Array(b)) => a.cmp(b),

                    (FuncCall(f1, a1), FuncCall(f2, a2)) => match f1.cmp(f2) {
                        Ordering::Equal => a1.cmp(a2),
                        other => other,
                    },
                    (MethodCall(m1, o1, a1), MethodCall(m2, o2, a2)) => match m1.cmp(m2) {
                        Ordering::Equal => match o1.cmp(o2) {
                            Ordering::Equal => a1.cmp(a2),
                            other => other,
                        },
                        other => other,
                    },

                    (Gt(l1, r1), Gt(l2, r2)) => match l1.cmp(l2) {
                        Ordering::Equal => r1.cmp(r2),
                        other => other,
                    },
                    (Lt(l1, r1), Lt(l2, r2)) => match l1.cmp(l2) {
                        Ordering::Equal => r1.cmp(r2),
                        other => other,
                    },
                    (Ge(l1, r1), Ge(l2, r2)) => match l1.cmp(l2) {
                        Ordering::Equal => r1.cmp(r2),
                        other => other,
                    },
                    (Le(l1, r1), Le(l2, r2)) => match l1.cmp(l2) {
                        Ordering::Equal => r1.cmp(r2),
                        other => other,
                    },
                    (Eq(l1, r1), Eq(l2, r2)) => match l1.cmp(l2) {
                        Ordering::Equal => r1.cmp(r2),
                        other => other,
                    },
                    (Ne(l1, r1), Ne(l2, r2)) => match l1.cmp(l2) {
                        Ordering::Equal => r1.cmp(r2),
                        other => other,
                    },
                    (In(l1, r1), In(l2, r2)) => match l1.cmp(l2) {
                        Ordering::Equal => r1.cmp(r2),
                        other => other,
                    },

                    (And(a), And(b)) => a.cmp(b),
                    (Or(a), Or(b)) => a.cmp(b),
                    (Not(a), Not(b)) => a.cmp(b),

                    _ => unreachable!(),
                }
            }
            other => other,
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Self) -> bool {
        self.cmp(other) == Ordering::Equal
    }
}

impl Eq for Expr {}

impl Hash for Expr {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Expr::*;
        use ordered_float::OrderedFloat;

        // Hash the discriminant first.
        std::mem::discriminant(self).hash(state);

        // Then hash the contents.
        match self {
            Field(s) => s.hash(state),
            FieldAccess(obj, field) => {
                obj.hash(state);
                field.hash(state);
            }

            Str(s) => s.hash(state),
            I64(i) => i.hash(state),
            F64(f) => OrderedFloat(*f).hash(state),
            Bool(b) => b.hash(state),
            Null => {},
            Array(v) => v.hash(state),
            FuncCall(name, args) => {
                name.hash(state);
                args.hash(state);
            }
            MethodCall(method, obj, args) => {
                method.hash(state);
                obj.hash(state);
                args.hash(state);
            }
            Gt(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            Lt(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            Ge(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            Le(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            Eq(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            Ne(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            In(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            And(v) => v.hash(state),
            Or(v) => v.hash(state),
            Not(e) => e.hash(state),
        }
    }
}

impl Expr {
    pub fn field_<T: Into<String>>(field: T) -> Self {
        Self::Field(field.into())
    }

    pub fn field_access_(obj: Expr, field: impl Into<String>) -> Self {
        Self::FieldAccess(Box::new(obj), field.into())
    }

    pub fn str_<T: Into<String>>(value: T) -> Self {
        Self::Str(value.into())
    }

    pub fn i64_<T: Into<i64>>(value: T) -> Self {
        Self::I64(value.into())
    }

    pub fn f64_<T: Into<f64>>(value: T) -> Self {
        Self::F64(value.into())
    }

    pub fn bool_<T: Into<bool>>(value: T) -> Self {
        Self::Bool(value.into())
    }

    pub fn null_() -> Self {
        Self::Null
    }

    pub fn array_<T: Into<Vec<Expr>>>(value: T) -> Self {
        Self::Array(value.into())
    }

    pub fn func_call_(func: impl Into<String>, args: Vec<Expr>) -> Self {
        Self::FuncCall(func.into(), args)
    }

    pub fn method_call_(obj: Expr, method: impl Into<String>, args: Vec<Expr>) -> Self {
        Self::MethodCall(method.into(), Box::new(obj), args)
    }

    pub fn gt_(left: Expr, right: Expr) -> Self {
        Self::Gt(Box::new(left), Box::new(right))
    }

    pub fn lt_(left: Expr, right: Expr) -> Self {
        Self::Lt(Box::new(left), Box::new(right))
    }

    pub fn ge_(left: Expr, right: Expr) -> Self {
        Self::Ge(Box::new(left), Box::new(right))
    }

    pub fn le_(left: Expr, right: Expr) -> Self {
        Self::Le(Box::new(left), Box::new(right))
    }

    pub fn eq_(left: Expr, right: Expr) -> Self {
        Self::Eq(Box::new(left), Box::new(right))
    }

    pub fn ne_(left: Expr, right: Expr) -> Self {
        Self::Ne(Box::new(left), Box::new(right))
    }

    pub fn in_(left: Expr, right: Expr) -> Self {
        Self::In(Box::new(left), Box::new(right))
    }

    pub fn and_<T: Into<Vec<Expr>>>(value: T) -> Self {
        Self::And(value.into())
    }

    pub fn or_<T: Into<Vec<Expr>>>(value: T) -> Self {
        Self::Or(value.into())
    }

    pub fn not_(self) -> Self {
        Self::Not(Box::new(self))
    }
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
        let ctx = TransformContext { depth: 0 };

        return Self::transform_expr(transformer, self, ctx).await;
    }

    async fn transform_expr<F: Transform>(transformer: &mut F, expr: Expr, ctx: TransformContext) -> Result<Expr, Error> {
        let this = transformer.transform(expr, ctx.clone()).await;

        match this {
            TransformResult::Continue(expr) => {
                return Box::pin(Self::transform_children(transformer, expr, ctx)).await;
            }
            TransformResult::Stop(expr) => {
                return Ok(expr);
            }
            TransformResult::Err(err) => {
                return Err(Error::Transform(err));
            }
        }
    }

    async fn transform_children<F: Transform>(transformer: &mut F, expr: Expr, mut ctx: TransformContext) -> Result<Expr, Error> {
        ctx.depth += 1;

        Ok(match expr {
            // Do nothing if the expression have no children.
            Expr::Field(name) => Expr::Field(name),
            Expr::FieldAccess(obj, field) => {
                let obj = Box::new(Self::transform_expr(transformer, *obj, ctx.clone()).await?);
                Expr::FieldAccess(obj, field)
            }

            Expr::Str(value) => Expr::Str(value),
            Expr::I64(value) => Expr::I64(value),
            Expr::F64(value) => Expr::F64(value),
            Expr::Bool(value) => Expr::Bool(value),
            Expr::Null => Expr::Null,
            Expr::Array(value) => Expr::Array(value),

            Expr::FuncCall(func, args) => {
                let mut transformed_args = Vec::new();
                for arg in args {
                    transformed_args.push(Self::transform_expr(transformer, arg, ctx.clone()).await?);
                }
                Expr::FuncCall(func, transformed_args)
            }
            Expr::MethodCall(method, obj, args) => {
                let obj = Box::new(Self::transform_expr(transformer, *obj, ctx.clone()).await?);
                let mut transformed_args = Vec::new();
                for arg in args {
                    transformed_args.push(Self::transform_expr(transformer, arg, ctx.clone()).await?);
                }
                Expr::MethodCall(method, obj, transformed_args)
            }

            Expr::Gt(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Gt(left, right)
            }
            Expr::Lt(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Lt(left, right)
            }
            Expr::Ge(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Ge(left, right)
            }
            Expr::Le(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Le(left, right)
            }
            Expr::Eq(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Eq(left, right)
            }
            Expr::Ne(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Ne(left, right)
            }
            Expr::In(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::In(left, right)
            }
            Expr::And(exprs) => {
                let mut transformed_exprs = Vec::new();
                for e in exprs {
                    transformed_exprs.push(Self::transform_expr(transformer, e, ctx.clone()).await?);
                }
                Expr::And(transformed_exprs)
            }
            Expr::Or(exprs) => {
                let mut transformed_exprs = Vec::new();
                for e in exprs {
                    transformed_exprs.push(Self::transform_expr(transformer, e, ctx.clone()).await?);
                }
                Expr::Or(transformed_exprs)
            }
            Expr::Not(expr) => {
                let expr = Box::new(Self::transform_expr(transformer, *expr, ctx).await?);
                Expr::Not(expr)
            }
        })
    }
}
