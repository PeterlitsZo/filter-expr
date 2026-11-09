use std::cmp::Ordering;
use std::collections::BTreeMap;
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
    Object(BTreeMap<String, Expr>),

    FuncCall(String, Vec<Expr>),
    MethodCall(String, Box<Expr>, Vec<Expr>),

    Add(Box<Expr>, Box<Expr>),
    Sub(Box<Expr>, Box<Expr>),
    Mul(Box<Expr>, Box<Expr>),
    Div(Box<Expr>, Box<Expr>),
    Rem(Box<Expr>, Box<Expr>),
    Pos(Box<Expr>),
    Neg(Box<Expr>),

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

    Let(String, Box<Expr>, Box<Expr>),
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
                Object(..) => 301,

                FuncCall(..) => 400,
                MethodCall(..) => 401,

                Add(..) => 500,
                Sub(..) => 501,
                Mul(..) => 502,
                Div(..) => 503,
                Rem(..) => 504,
                Pos(..) => 505,
                Neg(..) => 506,

                Gt(..) => 600,
                Lt(..) => 601,
                Ge(..) => 602,
                Le(..) => 603,
                Eq(..) => 604,
                Ne(..) => 605,
                In(..) => 606,

                And(..) => 700,
                Or(..) => 701,
                Not(..) => 702,

                Let(..) => 800,
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
                    (Object(a), Object(b)) => a.cmp(b),

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

                    (Add(l1, r1), Add(l2, r2)) => (l1, r1).cmp(&(l2, r2)),
                    (Sub(l1, r1), Sub(l2, r2)) => (l1, r1).cmp(&(l2, r2)),
                    (Mul(l1, r1), Mul(l2, r2)) => (l1, r1).cmp(&(l2, r2)),
                    (Div(l1, r1), Div(l2, r2)) => (l1, r1).cmp(&(l2, r2)),
                    (Rem(l1, r1), Rem(l2, r2)) => (l1, r1).cmp(&(l2, r2)),
                    (Pos(a), Pos(b)) => a.cmp(b),
                    (Neg(a), Neg(b)) => a.cmp(b),

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

                    (Let(n1, v1, b1), Let(n2, v2, b2)) => match n1.cmp(n2) {
                        Ordering::Equal => match v1.cmp(v2) {
                            Ordering::Equal => b1.cmp(b2),
                            other => other,
                        },
                        other => other,
                    },

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
            Object(v) => v.hash(state),
            FuncCall(name, args) => {
                name.hash(state);
                args.hash(state);
            }
            MethodCall(method, obj, args) => {
                method.hash(state);
                obj.hash(state);
                args.hash(state);
            }
            Add(l, r) | Sub(l, r) | Mul(l, r) | Div(l, r) | Rem(l, r) => {
                l.hash(state);
                r.hash(state);
            }
            Pos(e) | Neg(e) => e.hash(state),
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

            Let(name, value, body) => {
                name.hash(state);
                value.hash(state);
                body.hash(state);
            }
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

    pub fn object_<T: Into<BTreeMap<String, Expr>>>(value: T) -> Self {
        Self::Object(value.into())
    }

    pub fn func_call_(func: impl Into<String>, args: Vec<Expr>) -> Self {
        Self::FuncCall(func.into(), args)
    }

    pub fn method_call_(obj: Expr, method: impl Into<String>, args: Vec<Expr>) -> Self {
        Self::MethodCall(method.into(), Box::new(obj), args)
    }

    pub fn add_(left: Expr, right: Expr) -> Self {
        Self::Add(Box::new(left), Box::new(right))
    }

    pub fn sub_(left: Expr, right: Expr) -> Self {
        Self::Sub(Box::new(left), Box::new(right))
    }

    pub fn mul_(left: Expr, right: Expr) -> Self {
        Self::Mul(Box::new(left), Box::new(right))
    }

    pub fn div_(left: Expr, right: Expr) -> Self {
        Self::Div(Box::new(left), Box::new(right))
    }

    pub fn rem_(left: Expr, right: Expr) -> Self {
        Self::Rem(Box::new(left), Box::new(right))
    }

    pub fn pos_(expr: Expr) -> Self {
        Self::Pos(Box::new(expr))
    }

    pub fn neg_(expr: Expr) -> Self {
        Self::Neg(Box::new(expr))
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

    pub fn let_(name: impl Into<String>, value: Expr, body: Expr) -> Self {
        Self::Let(name.into(), Box::new(value), Box::new(body))
    }
}

impl Expr {
    /// Recursively transform an expression using the provided transformer.
    ///
    /// ```rust
    /// use filter_expr::{Expr, Transform, TransformContext, TransformResult};
    /// use async_trait::async_trait;
    ///
    /// struct MyTransformer;
    ///
    /// #[async_trait]
    /// impl Transform for MyTransformer {
    ///     async fn transform(&mut self, expr: Expr, _ctx: TransformContext) -> TransformResult {
    ///         // Transform the expression before recursing
    ///         TransformResult::Continue(match expr {
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

            Expr::Array(elements) => {
                let mut transformed_elements = Vec::new();
                for element in elements {
                    transformed_elements.push(Self::transform_expr(transformer, element, ctx.clone()).await?);
                }
                Expr::Array(transformed_elements)
            }
            Expr::Object(fields) => {
                let mut transformed_fields = BTreeMap::new();
                for (key, value) in fields {
                    transformed_fields.insert(key, Self::transform_expr(transformer, value, ctx.clone()).await?);
                }
                Expr::Object(transformed_fields)
            }

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

            Expr::Add(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Add(left, right)
            }
            Expr::Sub(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Sub(left, right)
            }
            Expr::Mul(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Mul(left, right)
            }
            Expr::Div(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Div(left, right)
            }
            Expr::Rem(left, right) => {
                let left = Box::new(Self::transform_expr(transformer, *left, ctx.clone()).await?);
                let right = Box::new(Self::transform_expr(transformer, *right, ctx).await?);
                Expr::Rem(left, right)
            }
            Expr::Pos(expr) => Expr::Pos(Box::new(
                Self::transform_expr(transformer, *expr, ctx).await?,
            )),
            Expr::Neg(expr) => Expr::Neg(Box::new(
                Self::transform_expr(transformer, *expr, ctx).await?,
            )),
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

            Expr::Let(name, value, body) => {
                let value = Box::new(Self::transform_expr(transformer, *value, ctx.clone()).await?);
                let body = Box::new(Self::transform_expr(transformer, *body, ctx.clone()).await?);
                Expr::Let(name, value, body)
            }
        })
    }
}

impl Expr {
    /// Optimize the expression by applying constant folding and simplification
    /// rules.
    ///
    /// Assumes expressions are pure within an evaluation's world. Removed
    /// expressions are not evaluated, including their errors.
    ///
    /// Examples:
    ///
    /// - `true AND true` → `true`
    /// - `true AND false` → `false`
    /// - `NOT NOT expr` → `expr`
    /// - `1 > 2` → `false`
    pub fn optimize(self) -> Self {
        use Expr::*;

        match self {
            // Leaf nodes - no optimization needed.
            Field(_) | Str(_) | I64(_) | F64(_) | Bool(_) | Null => self,

            // Field access - optimize the object.
            FieldAccess(obj, field) => {
                FieldAccess(Box::new(obj.optimize()), field)
            }

            // Array - optimize all elements.
            Array(elements) => {
                Array(elements.into_iter().map(|e| e.optimize()).collect())
            }

            // Object - optimize all field values.
            Object(fields) => {
                Object(fields.into_iter().map(|(key, value)| (key, value.optimize())).collect())
            }

            // Function call - optimize all arguments.
            FuncCall(func, args) => {
                FuncCall(func, args.into_iter().map(|a| a.optimize()).collect())
            }

            // Method call - optimize object and arguments.
            MethodCall(method, obj, args) => {
                MethodCall(method, Box::new(obj.optimize()), args.into_iter().map(|a| a.optimize()).collect())
            }

            Add(left, right) => Add(Box::new(left.optimize()), Box::new(right.optimize())),
            Sub(left, right) => Sub(Box::new(left.optimize()), Box::new(right.optimize())),
            Mul(left, right) => Mul(Box::new(left.optimize()), Box::new(right.optimize())),
            Div(left, right) => Div(Box::new(left.optimize()), Box::new(right.optimize())),
            Rem(left, right) => Rem(Box::new(left.optimize()), Box::new(right.optimize())),
            Pos(expr) => Pos(Box::new(expr.optimize())),
            Neg(expr) => Neg(Box::new(expr.optimize())),

            // Comparison operators - optimize and fold constants.
            Gt(left, right) => {
                let left = left.optimize();
                let right = right.optimize();
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a > *b),
                    (F64(a), F64(b)) => Bool(*a > *b),
                    (Str(a), Str(b)) => Bool(a > b),
                    _ => Gt(Box::new(left), Box::new(right)),
                }
            }
            Lt(left, right) => {
                let left = left.optimize();
                let right = right.optimize();
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a < *b),
                    (F64(a), F64(b)) => Bool(*a < *b),
                    (Str(a), Str(b)) => Bool(a < b),
                    _ => Lt(Box::new(left), Box::new(right)),
                }
            }
            Ge(left, right) => {
                let left = left.optimize();
                let right = right.optimize();
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a >= *b),
                    (F64(a), F64(b)) => Bool(*a >= *b),
                    (Str(a), Str(b)) => Bool(a >= b),
                    _ => Ge(Box::new(left), Box::new(right)),
                }
            }
            Le(left, right) => {
                let left = left.optimize();
                let right = right.optimize();
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a <= *b),
                    (F64(a), F64(b)) => Bool(*a <= *b),
                    (Str(a), Str(b)) => Bool(a <= b),
                    _ => Le(Box::new(left), Box::new(right)),
                }
            }
            Eq(left, right) => {
                let left = left.optimize();
                let right = right.optimize();
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a == *b),
                    (F64(a), F64(b)) => Bool(*a == *b),
                    (Str(a), Str(b)) => Bool(a == b),
                    (Bool(a), Bool(b)) => Bool(*a == *b),
                    (Null, Null) => Bool(true),
                    _ => Eq(Box::new(left), Box::new(right)),
                }
            }
            Ne(left, right) => {
                let left = left.optimize();
                let right = right.optimize();
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a != *b),
                    (F64(a), F64(b)) => Bool(*a != *b),
                    (Str(a), Str(b)) => Bool(a != b),
                    (Bool(a), Bool(b)) => Bool(*a != *b),
                    (Null, Null) => Bool(false),
                    _ => Ne(Box::new(left), Box::new(right)),
                }
            }
            In(left, right) => {
                let left = left.optimize();
                let right = right.optimize();
                In(Box::new(left), Box::new(right))
            }

            // AND optimization.
            And(exprs) => {
                let mut optimized: Vec<Expr> = Vec::new();
                let mut has_false = false;

                for expr in exprs {
                    let opt_expr = expr.optimize();
                    match &opt_expr {
                        Bool(true) => {
                            // Skip true values in AND.
                            continue;
                        }
                        Bool(false) => {
                            // If any operand is false, the whole AND is false.
                            has_false = true;
                            break;
                        }
                        _ => {
                            optimized.push(opt_expr);
                        }
                    }
                }

                if has_false {
                    Bool(false)
                } else if optimized.is_empty() {
                    // Empty AND is true (identity element)
                    Bool(true)
                } else if optimized.len() == 1 {
                    // Single element AND is just that element
                    optimized.into_iter().next().unwrap()
                } else {
                    And(optimized)
                }
            }

            // OR optimization.
            Or(exprs) => {
                let mut optimized: Vec<Expr> = Vec::new();
                let mut has_true = false;

                for expr in exprs {
                    let opt_expr = expr.optimize();
                    match &opt_expr {
                        Bool(false) => {
                            // Skip false values in OR
                            continue;
                        }
                        Bool(true) => {
                            // If any operand is true, the whole OR is true
                            has_true = true;
                            break;
                        }
                        _ => {
                            optimized.push(opt_expr);
                        }
                    }
                }

                if has_true {
                    Bool(true)
                } else if optimized.is_empty() {
                    // Empty OR is false (identity element)
                    Bool(false)
                } else if optimized.len() == 1 {
                    // Single element OR is just that element
                    optimized.into_iter().next().unwrap()
                } else {
                    Or(optimized)
                }
            }

            // NOT optimization.
            Not(expr) => {
                let opt_expr = expr.optimize();
                match opt_expr {
                    Bool(true) => Bool(false),
                    Bool(false) => Bool(true),
                    Not(inner) => {
                        // Double negation: NOT NOT expr -> expr
                        *inner
                    }
                    other => Not(Box::new(other)),
                }
            }

            // Optimize the binding value and body.
            Let(name, value, body) => {
                Let(name, Box::new(value.optimize()), Box::new(body.optimize()))
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_optimize() {
        let expr = Expr::And(vec![Expr::Bool(true), Expr::Bool(false)]);
        let optimized = expr.optimize();
        assert_eq!(optimized, Expr::Bool(false));

        let expr = Expr::Or(vec![Expr::Bool(false), Expr::Bool(true)]);
        let optimized = expr.optimize();
        assert_eq!(optimized, Expr::Bool(true));

        let expr = Expr::Not(Box::new(Expr::Bool(true)));
        let optimized = expr.optimize();
        assert_eq!(optimized, Expr::Bool(false));
    }
}
