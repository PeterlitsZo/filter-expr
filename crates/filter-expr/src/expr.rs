use std::collections::BTreeMap;
use std::fmt;

use crate::{CompactString, Error, Transform, TransformContext, TransformResult};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ExprId(usize);

#[derive(Clone, Default)]
pub struct ExprArena {
    nodes: Vec<Expr>,
}

impl ExprArena {
    pub fn alloc(&mut self, expr: Expr) -> ExprId {
        if self.nodes.capacity() == 0 {
            self.nodes.reserve(16);
        }
        let id = ExprId(self.nodes.len());
        self.nodes.push(expr);
        id
    }

    pub fn get(&self, id: ExprId) -> Option<&Expr> {
        self.nodes.get(id.0)
    }

    fn optimize_node(&mut self, id: ExprId) -> ExprId {
        let expr = self.get(id).unwrap().clone().optimize(self);
        self.alloc(expr)
    }

    async fn transform_node<F: Transform>(
        &mut self,
        id: ExprId,
        transformer: &mut F,
        ctx: TransformContext,
    ) -> Result<ExprId, Error> {
        let expr = self.get(id).unwrap().clone();
        let expr = Expr::transform_expr(self, transformer, expr, ctx).await?;
        Ok(self.alloc(expr))
    }
}

/// The expression.
///
/// It is an AST of the filter expression.
#[derive(Debug, Clone)]
pub enum Expr {
    Field(CompactString),
    FieldAccess(ExprId, CompactString),

    Str(CompactString),
    I64(i64),
    F64(f64),
    Bool(bool),
    Null,

    Array(Vec<Expr>),
    Object(BTreeMap<CompactString, Expr>),

    FuncCall(CompactString, Vec<Expr>),
    MethodCall(CompactString, ExprId, Vec<Expr>),

    Add(ExprId, ExprId),
    Sub(ExprId, ExprId),
    Mul(ExprId, ExprId),
    Div(ExprId, ExprId),
    Rem(ExprId, ExprId),
    Pos(ExprId),
    Neg(ExprId),

    Gt(ExprId, ExprId),
    Lt(ExprId, ExprId),
    Ge(ExprId, ExprId),
    Le(ExprId, ExprId),
    Eq(ExprId, ExprId),
    Ne(ExprId, ExprId),
    In(ExprId, ExprId),

    And(Vec<Expr>),
    Or(Vec<Expr>),
    Not(ExprId),

    Let(CompactString, ExprId, ExprId),
}

impl Expr {
    pub fn field_<T: Into<CompactString>>(field: T) -> Self {
        Self::Field(field.into())
    }

    pub fn field_access_(obj: ExprId, field: impl Into<CompactString>) -> Self {
        Self::FieldAccess(obj, field.into())
    }

    pub fn str_<T: Into<CompactString>>(value: T) -> Self {
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

    pub fn object_<T: Into<BTreeMap<CompactString, Expr>>>(value: T) -> Self {
        Self::Object(value.into())
    }

    pub fn func_call_(func: impl Into<CompactString>, args: Vec<Expr>) -> Self {
        Self::FuncCall(func.into(), args)
    }

    pub fn method_call_(obj: ExprId, method: impl Into<CompactString>, args: Vec<Expr>) -> Self {
        Self::MethodCall(method.into(), obj, args)
    }

    pub fn add_(left: ExprId, right: ExprId) -> Self {
        Self::Add(left, right)
    }

    pub fn sub_(left: ExprId, right: ExprId) -> Self {
        Self::Sub(left, right)
    }

    pub fn mul_(left: ExprId, right: ExprId) -> Self {
        Self::Mul(left, right)
    }

    pub fn div_(left: ExprId, right: ExprId) -> Self {
        Self::Div(left, right)
    }

    pub fn rem_(left: ExprId, right: ExprId) -> Self {
        Self::Rem(left, right)
    }

    pub fn pos_(expr: ExprId) -> Self {
        Self::Pos(expr)
    }

    pub fn neg_(expr: ExprId) -> Self {
        Self::Neg(expr)
    }

    pub fn gt_(left: ExprId, right: ExprId) -> Self {
        Self::Gt(left, right)
    }

    pub fn lt_(left: ExprId, right: ExprId) -> Self {
        Self::Lt(left, right)
    }

    pub fn ge_(left: ExprId, right: ExprId) -> Self {
        Self::Ge(left, right)
    }

    pub fn le_(left: ExprId, right: ExprId) -> Self {
        Self::Le(left, right)
    }

    pub fn eq_(left: ExprId, right: ExprId) -> Self {
        Self::Eq(left, right)
    }

    pub fn ne_(left: ExprId, right: ExprId) -> Self {
        Self::Ne(left, right)
    }

    pub fn in_(left: ExprId, right: ExprId) -> Self {
        Self::In(left, right)
    }

    pub fn and_<T: Into<Vec<Expr>>>(value: T) -> Self {
        Self::And(value.into())
    }

    pub fn or_<T: Into<Vec<Expr>>>(value: T) -> Self {
        Self::Or(value.into())
    }

    pub fn not_(expr: ExprId) -> Self {
        Self::Not(expr)
    }

    pub fn let_(name: impl Into<CompactString>, value: ExprId, body: ExprId) -> Self {
        Self::Let(name.into(), value, body)
    }
}

impl Expr {
    /// Recursively transform an expression using the provided transformer.
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
    ///
    /// # #[tokio::main]
    /// # async fn main() {
    /// let expr = Expr::Field("old_name".into());
    /// let mut transformer = MyTransformer;
    /// let result = expr.transform(&mut ExprArena::default(), &mut transformer).await.unwrap();
    /// assert!(matches!(result, Expr::Field(name) if name == "new_name"));
    /// # }
    /// ```
    pub async fn transform<F: Transform>(
        self,
        arena: &mut ExprArena,
        transformer: &mut F,
    ) -> Result<Expr, Error> {
        let ctx = TransformContext { depth: 0 };

        return Self::transform_expr(arena, transformer, self, ctx).await;
    }

    async fn transform_expr<F: Transform>(
        arena: &mut ExprArena,
        transformer: &mut F,
        expr: Expr,
        ctx: TransformContext,
    ) -> Result<Expr, Error> {
        let this = transformer.transform(expr, arena, ctx.clone()).await;

        match this {
            TransformResult::Continue(expr) => {
                return Box::pin(Self::transform_children(arena, transformer, expr, ctx)).await;
            }
            TransformResult::Stop(expr) => {
                return Ok(expr);
            }
            TransformResult::Err(err) => {
                return Err(Error::Transform(err));
            }
        }
    }

    async fn transform_children<F: Transform>(
        arena: &mut ExprArena,
        transformer: &mut F,
        expr: Expr,
        mut ctx: TransformContext,
    ) -> Result<Expr, Error> {
        ctx.depth += 1;

        Ok(match expr {
            // Do nothing if the expression have no children.
            Expr::Field(name) => Expr::Field(name),
            Expr::FieldAccess(obj, field) => {
                let obj = arena.transform_node(obj, transformer, ctx.clone()).await?;
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
                    transformed_elements.push(
                        Self::transform_expr(arena, transformer, element, ctx.clone()).await?,
                    );
                }
                Expr::Array(transformed_elements)
            }
            Expr::Object(fields) => {
                let mut transformed_fields = BTreeMap::new();
                for (key, value) in fields {
                    transformed_fields.insert(
                        key,
                        Self::transform_expr(arena, transformer, value, ctx.clone()).await?,
                    );
                }
                Expr::Object(transformed_fields)
            }

            Expr::FuncCall(func, args) => {
                let mut transformed_args = Vec::new();
                for arg in args {
                    transformed_args
                        .push(Self::transform_expr(arena, transformer, arg, ctx.clone()).await?);
                }
                Expr::FuncCall(func, transformed_args)
            }
            Expr::MethodCall(method, obj, args) => {
                let obj = arena.transform_node(obj, transformer, ctx.clone()).await?;
                let mut transformed_args = Vec::new();
                for arg in args {
                    transformed_args
                        .push(Self::transform_expr(arena, transformer, arg, ctx.clone()).await?);
                }
                Expr::MethodCall(method, obj, transformed_args)
            }

            Expr::Add(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Add(left, right)
            }
            Expr::Sub(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Sub(left, right)
            }
            Expr::Mul(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Mul(left, right)
            }
            Expr::Div(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Div(left, right)
            }
            Expr::Rem(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Rem(left, right)
            }
            Expr::Pos(expr) => Expr::Pos(arena.transform_node(expr, transformer, ctx).await?),
            Expr::Neg(expr) => Expr::Neg(arena.transform_node(expr, transformer, ctx).await?),
            Expr::Gt(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Gt(left, right)
            }
            Expr::Lt(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Lt(left, right)
            }
            Expr::Ge(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Ge(left, right)
            }
            Expr::Le(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Le(left, right)
            }
            Expr::Eq(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Eq(left, right)
            }
            Expr::Ne(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::Ne(left, right)
            }
            Expr::In(left, right) => {
                let left = arena.transform_node(left, transformer, ctx.clone()).await?;
                let right = arena.transform_node(right, transformer, ctx).await?;
                Expr::In(left, right)
            }
            Expr::And(exprs) => {
                let mut transformed_exprs = Vec::new();
                for e in exprs {
                    transformed_exprs
                        .push(Self::transform_expr(arena, transformer, e, ctx.clone()).await?);
                }
                Expr::And(transformed_exprs)
            }
            Expr::Or(exprs) => {
                let mut transformed_exprs = Vec::new();
                for e in exprs {
                    transformed_exprs
                        .push(Self::transform_expr(arena, transformer, e, ctx.clone()).await?);
                }
                Expr::Or(transformed_exprs)
            }
            Expr::Not(expr) => {
                let expr = arena.transform_node(expr, transformer, ctx).await?;
                Expr::Not(expr)
            }

            Expr::Let(name, value, body) => {
                let value = arena
                    .transform_node(value, transformer, ctx.clone())
                    .await?;
                let body = arena.transform_node(body, transformer, ctx.clone()).await?;
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
    pub fn optimize(self, arena: &mut ExprArena) -> Self {
        use Expr::*;

        match self {
            // Leaf nodes - no optimization needed.
            Field(_) | Str(_) | I64(_) | F64(_) | Bool(_) | Null => self,

            // Field access - optimize the object.
            FieldAccess(obj, field) => FieldAccess(arena.optimize_node(obj), field),

            // Array - optimize all elements.
            Array(elements) => Array(elements.into_iter().map(|e| e.optimize(arena)).collect()),

            // Object - optimize all field values.
            Object(fields) => Object(
                fields
                    .into_iter()
                    .map(|(key, value)| (key, value.optimize(arena)))
                    .collect(),
            ),

            // Function call - optimize all arguments.
            FuncCall(func, args) => {
                FuncCall(func, args.into_iter().map(|a| a.optimize(arena)).collect())
            }

            // Method call - optimize object and arguments.
            MethodCall(method, obj, args) => MethodCall(
                method,
                arena.optimize_node(obj),
                args.into_iter().map(|a| a.optimize(arena)).collect(),
            ),

            Add(left, right) => Add(arena.optimize_node(left), arena.optimize_node(right)),
            Sub(left, right) => Sub(arena.optimize_node(left), arena.optimize_node(right)),
            Mul(left, right) => Mul(arena.optimize_node(left), arena.optimize_node(right)),
            Div(left, right) => Div(arena.optimize_node(left), arena.optimize_node(right)),
            Rem(left, right) => Rem(arena.optimize_node(left), arena.optimize_node(right)),
            Pos(expr) => Pos(arena.optimize_node(expr)),
            Neg(expr) => Neg(arena.optimize_node(expr)),

            // Comparison operators - optimize and fold constants.
            Gt(left, right) => {
                let left = arena.get(left).unwrap().clone().optimize(arena);
                let right = arena.get(right).unwrap().clone().optimize(arena);
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a > *b),
                    (F64(a), F64(b)) => Bool(*a > *b),
                    (Str(a), Str(b)) => Bool(a > b),
                    _ => Gt(arena.alloc(left), arena.alloc(right)),
                }
            }
            Lt(left, right) => {
                let left = arena.get(left).unwrap().clone().optimize(arena);
                let right = arena.get(right).unwrap().clone().optimize(arena);
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a < *b),
                    (F64(a), F64(b)) => Bool(*a < *b),
                    (Str(a), Str(b)) => Bool(a < b),
                    _ => Lt(arena.alloc(left), arena.alloc(right)),
                }
            }
            Ge(left, right) => {
                let left = arena.get(left).unwrap().clone().optimize(arena);
                let right = arena.get(right).unwrap().clone().optimize(arena);
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a >= *b),
                    (F64(a), F64(b)) => Bool(*a >= *b),
                    (Str(a), Str(b)) => Bool(a >= b),
                    _ => Ge(arena.alloc(left), arena.alloc(right)),
                }
            }
            Le(left, right) => {
                let left = arena.get(left).unwrap().clone().optimize(arena);
                let right = arena.get(right).unwrap().clone().optimize(arena);
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a <= *b),
                    (F64(a), F64(b)) => Bool(*a <= *b),
                    (Str(a), Str(b)) => Bool(a <= b),
                    _ => Le(arena.alloc(left), arena.alloc(right)),
                }
            }
            Eq(left, right) => {
                let left = arena.get(left).unwrap().clone().optimize(arena);
                let right = arena.get(right).unwrap().clone().optimize(arena);
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a == *b),
                    (F64(a), F64(b)) => Bool(*a == *b),
                    (Str(a), Str(b)) => Bool(a == b),
                    (Bool(a), Bool(b)) => Bool(*a == *b),
                    (Null, Null) => Bool(true),
                    _ => Eq(arena.alloc(left), arena.alloc(right)),
                }
            }
            Ne(left, right) => {
                let left = arena.get(left).unwrap().clone().optimize(arena);
                let right = arena.get(right).unwrap().clone().optimize(arena);
                match (&left, &right) {
                    (I64(a), I64(b)) => Bool(*a != *b),
                    (F64(a), F64(b)) => Bool(*a != *b),
                    (Str(a), Str(b)) => Bool(a != b),
                    (Bool(a), Bool(b)) => Bool(*a != *b),
                    (Null, Null) => Bool(false),
                    _ => Ne(arena.alloc(left), arena.alloc(right)),
                }
            }
            In(left, right) => {
                let left = arena.get(left).unwrap().clone().optimize(arena);
                let right = arena.get(right).unwrap().clone().optimize(arena);
                In(arena.alloc(left), arena.alloc(right))
            }

            // AND optimization.
            And(exprs) => {
                let mut optimized: Vec<Expr> = Vec::new();
                let mut has_false = false;

                for expr in exprs {
                    let opt_expr = expr.optimize(arena);
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
                    let opt_expr = expr.optimize(arena);
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
                let opt_expr = arena.get(expr).unwrap().clone().optimize(arena);
                match opt_expr {
                    Bool(true) => Bool(false),
                    Bool(false) => Bool(true),
                    Not(inner) => {
                        // Double negation: NOT NOT expr -> expr
                        arena.get(inner).unwrap().clone()
                    }
                    other => Not(arena.alloc(other)),
                }
            }

            // Optimize the binding value and body.
            Let(name, value, body) => {
                Let(name, arena.optimize_node(value), arena.optimize_node(body))
            }
        }
    }
}

impl Expr {
    pub(crate) fn eq_in(&self, other: &Self, arena: &ExprArena, other_arena: &ExprArena) -> bool {
        let nodes_eq = |a, b| {
            arena
                .get(a)
                .unwrap()
                .eq_in(other_arena.get(b).unwrap(), arena, other_arena)
        };
        let lists_eq = |a: &[Expr], b: &[Expr]| {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| a.eq_in(b, arena, other_arena))
        };
        match (self, other) {
            (Self::Field(a), Self::Field(b)) | (Self::Str(a), Self::Str(b)) => a == b,
            (Self::I64(a), Self::I64(b)) => a == b,
            (Self::F64(a), Self::F64(b)) => {
                ordered_float::OrderedFloat(*a) == ordered_float::OrderedFloat(*b)
            }
            (Self::Bool(a), Self::Bool(b)) => a == b,
            (Self::Null, Self::Null) => true,
            (Self::FieldAccess(a, field_a), Self::FieldAccess(b, field_b)) => {
                field_a == field_b && nodes_eq(*a, *b)
            }
            (Self::Array(a), Self::Array(b))
            | (Self::And(a), Self::And(b))
            | (Self::Or(a), Self::Or(b)) => lists_eq(a, b),
            (Self::Object(a), Self::Object(b)) => {
                a.len() == b.len()
                    && a.iter()
                        .zip(b)
                        .all(|((ka, a), (kb, b))| ka == kb && a.eq_in(b, arena, other_arena))
            }
            (Self::FuncCall(a, args_a), Self::FuncCall(b, args_b)) => {
                a == b && lists_eq(args_a, args_b)
            }
            (Self::MethodCall(a, obj_a, args_a), Self::MethodCall(b, obj_b, args_b)) => {
                a == b && nodes_eq(*obj_a, *obj_b) && lists_eq(args_a, args_b)
            }
            (Self::Add(al, ar), Self::Add(bl, br))
            | (Self::Sub(al, ar), Self::Sub(bl, br))
            | (Self::Mul(al, ar), Self::Mul(bl, br))
            | (Self::Div(al, ar), Self::Div(bl, br))
            | (Self::Rem(al, ar), Self::Rem(bl, br))
            | (Self::Gt(al, ar), Self::Gt(bl, br))
            | (Self::Lt(al, ar), Self::Lt(bl, br))
            | (Self::Ge(al, ar), Self::Ge(bl, br))
            | (Self::Le(al, ar), Self::Le(bl, br))
            | (Self::Eq(al, ar), Self::Eq(bl, br))
            | (Self::Ne(al, ar), Self::Ne(bl, br))
            | (Self::In(al, ar), Self::In(bl, br)) => nodes_eq(*al, *bl) && nodes_eq(*ar, *br),
            (Self::Pos(a), Self::Pos(b))
            | (Self::Neg(a), Self::Neg(b))
            | (Self::Not(a), Self::Not(b)) => nodes_eq(*a, *b),
            (Self::Let(a, av, ab), Self::Let(b, bv, bb)) => {
                a == b && nodes_eq(*av, *bv) && nodes_eq(*ab, *bb)
            }
            _ => false,
        }
    }
}

pub(crate) struct ExprDebug<'a>(pub(crate) &'a Expr, pub(crate) &'a ExprArena);

struct ExprListDebug<'a>(&'a [Expr], &'a ExprArena);

impl fmt::Debug for ExprListDebug<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_list()
            .entries(self.0.iter().map(|expr| ExprDebug(expr, self.1)))
            .finish()
    }
}

struct ExprMapDebug<'a>(&'a BTreeMap<CompactString, Expr>, &'a ExprArena);

impl fmt::Debug for ExprMapDebug<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_map()
            .entries(
                self.0
                    .iter()
                    .map(|(key, expr)| (key, ExprDebug(expr, self.1))),
            )
            .finish()
    }
}

impl fmt::Debug for ExprDebug<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.0 {
            Expr::Field(name) => f.debug_tuple("Field").field(name).finish(),
            Expr::FieldAccess(obj, field) => f
                .debug_tuple("FieldAccess")
                .field(&ExprDebug(self.1.get(*obj).unwrap(), self.1))
                .field(field)
                .finish(),
            Expr::Str(value) => f.debug_tuple("Str").field(value).finish(),
            Expr::I64(value) => f.debug_tuple("I64").field(value).finish(),
            Expr::F64(value) => f.debug_tuple("F64").field(value).finish(),
            Expr::Bool(value) => f.debug_tuple("Bool").field(value).finish(),
            Expr::Null => f.write_str("Null"),
            Expr::Array(elements) => f
                .debug_tuple("Array")
                .field(&ExprListDebug(elements, self.1))
                .finish(),
            Expr::Object(fields) => f
                .debug_tuple("Object")
                .field(&ExprMapDebug(fields, self.1))
                .finish(),
            Expr::FuncCall(func, args) => f
                .debug_tuple("FuncCall")
                .field(func)
                .field(&ExprListDebug(args, self.1))
                .finish(),
            Expr::MethodCall(method, obj, args) => f
                .debug_tuple("MethodCall")
                .field(method)
                .field(&ExprDebug(self.1.get(*obj).unwrap(), self.1))
                .field(&ExprListDebug(args, self.1))
                .finish(),
            Expr::Add(left, right) => f
                .debug_tuple("Add")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Sub(left, right) => f
                .debug_tuple("Sub")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Mul(left, right) => f
                .debug_tuple("Mul")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Div(left, right) => f
                .debug_tuple("Div")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Rem(left, right) => f
                .debug_tuple("Rem")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Pos(expr) => f
                .debug_tuple("Pos")
                .field(&ExprDebug(self.1.get(*expr).unwrap(), self.1))
                .finish(),
            Expr::Neg(expr) => f
                .debug_tuple("Neg")
                .field(&ExprDebug(self.1.get(*expr).unwrap(), self.1))
                .finish(),
            Expr::Gt(left, right) => f
                .debug_tuple("Gt")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Lt(left, right) => f
                .debug_tuple("Lt")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Ge(left, right) => f
                .debug_tuple("Ge")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Le(left, right) => f
                .debug_tuple("Le")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Eq(left, right) => f
                .debug_tuple("Eq")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::Ne(left, right) => f
                .debug_tuple("Ne")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::In(left, right) => f
                .debug_tuple("In")
                .field(&ExprDebug(self.1.get(*left).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*right).unwrap(), self.1))
                .finish(),
            Expr::And(exprs) => f
                .debug_tuple("And")
                .field(&ExprListDebug(exprs, self.1))
                .finish(),
            Expr::Or(exprs) => f
                .debug_tuple("Or")
                .field(&ExprListDebug(exprs, self.1))
                .finish(),
            Expr::Not(expr) => f
                .debug_tuple("Not")
                .field(&ExprDebug(self.1.get(*expr).unwrap(), self.1))
                .finish(),
            Expr::Let(name, value, body) => f
                .debug_tuple("Let")
                .field(name)
                .field(&ExprDebug(self.1.get(*value).unwrap(), self.1))
                .field(&ExprDebug(self.1.get(*body).unwrap(), self.1))
                .finish(),
        }
    }
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

    #[test]
    fn test_optimize() {
        let mut arena = crate::ExprArena::default();
        let expr = Expr::And(vec![Expr::Bool(true), Expr::Bool(false)]);
        let optimized = expr.optimize(&mut arena);
        assert!(matches!(optimized, Expr::Bool(false)));

        let expr = Expr::Or(vec![Expr::Bool(false), Expr::Bool(true)]);
        let optimized = expr.optimize(&mut arena);
        assert!(matches!(optimized, Expr::Bool(true)));

        let expr = Expr::Not(alloc!(arena, Expr::Bool(true)));
        let optimized = expr.optimize(&mut arena);
        assert!(matches!(optimized, Expr::Bool(false)));
    }
}
