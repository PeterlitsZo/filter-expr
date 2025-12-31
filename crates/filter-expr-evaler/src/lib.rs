//! Evaluator for filter expressions.

mod asm;
mod asm_codegen;
mod ast_runner;
mod bc;
mod bc_codegen;
mod bc_runner;
mod ctx;
mod error;
mod value;

use std::sync::{Arc, Mutex};

use filter_expr::{Expr, FilterExpr};
use moka::sync::Cache;
use regex::Regex;

use crate::ast_runner::AstRunner;
use crate::bc::Bytecode;

pub use crate::ctx::{Context, ExprFn, ExprFnContext, SimpleContext};
pub use crate::error::Error;
pub use crate::value::{Value, ValueType};

struct FilterExprEvalerCache {
    /// The cached compiled bytecode.
    ///
    /// Used to avoid compiling the same expression multiple times.
    pub(crate) cached_bytecode: Cache<Expr, Arc<Bytecode>>,

    /// The cached compiled regex.
    ///
    /// It is useful to avoid compiling the same regex multiple times.
    pub(crate) cached_regex: Cache<String, Arc<Regex>>,
}

pub struct FilterExprEvaler {
    /// The global cache shared by runners.
    cache: Arc<Mutex<FilterExprEvalerCache>>,
}

impl FilterExprEvaler {
    /// Create a new filter expression evaluator.
    pub fn new() -> Self {
        Self {
            cache: Arc::new(Mutex::new(FilterExprEvalerCache {
                cached_bytecode: Cache::new(128),
                cached_regex: Cache::new(128),
            })),
        }
    }

    /// Evaluate the filter expression using the default runner.
    pub async fn eval(&self, filter_expr: &FilterExpr, ctx: &dyn Context) -> Result<bool, Error> {
        self.eval_by_bytecode_runner(filter_expr, ctx).await
    }

    /// Evaluate the filter expression in the given context.
    pub async fn eval_by_bytecode_runner(&self, filter_expr: &FilterExpr, ctx: &dyn Context) -> Result<bool, Error> {
        if let Some(expr) = filter_expr.expr() {
            let value = FilterExprEvalerInner::new(self, expr)
                .eval_by_bytecode_runner(ctx)
                .await?;
            match value {
                Value::Bool(b) => Ok(b),
                _ => Err(Error::InvalidValue(format!("{value:?}"))),
            }
        } else {
            Ok(true)
        }
    }

    /// Evaluate the filter expression using AST runner.
    pub async fn eval_by_ast_runner(&self, filter_expr: &FilterExpr, ctx: &dyn Context) -> Result<bool, Error> {
        if let Some(expr) = filter_expr.expr() {
            let value = FilterExprEvalerInner::new(self, expr)
                .eval_by_ast_runner(ctx)
                .await?;
            match value {
                Value::Bool(b) => Ok(b),
                _ => Err(Error::InvalidValue(format!("{value:?}"))),
            }
        } else {
            Ok(true)
        }
    }

    /// Evaluate the filter expression using bytecode execution.
    pub async fn eval_by_bytecode(&self, filter_expr: &FilterExpr, ctx: &dyn Context) -> Result<bool, Error> {
        if let Some(expr) = filter_expr.expr() {
            let value = FilterExprEvalerInner::new(self, expr)
                .eval_by_bytecode_runner(ctx)
                .await?;
            match value {
                Value::Bool(b) => Ok(b),
                _ => Err(Error::InvalidValue(format!("{value:?}"))),
            }
        } else {
            Ok(true)
        }
    }
}

struct FilterExprEvalerInner<'a> {
    evaler: &'a FilterExprEvaler,
    expr: &'a Expr,
}

impl<'a> FilterExprEvalerInner<'a> {
    pub fn new(evaler: &'a FilterExprEvaler, expr: &'a Expr) -> Self {
        Self { evaler, expr }
    }

    /// Evaluate the expression using AST runner.
    pub async fn eval_by_ast_runner(&self, ctx: &dyn Context) -> Result<Value, Error> {
        AstRunner::new(self.expr, self.evaler.cache.clone())
            .run(ctx)
            .await
    }

    /// Evaluate the expression using bytecode runner.
    pub async fn eval_by_bytecode_runner(&self, ctx: &dyn Context) -> Result<Value, Error> {
        let bytecode = {
            let cache = self
                .evaler
                .cache
                .lock()
                .map_err(|e| Error::Internal(format!("failed to lock cache: {e}")))?;
            
            // Check if bytecode is cached, generate if not.
            if let Some(cached) = cache.cached_bytecode.get(self.expr) {
                cached
            } else {
                drop(cache);
                // Generate bytecode and cache it.
                let asm = asm_codegen::AsmCodegen::new().codegen(self.expr);
                let bytecode = bc_codegen::BytecodeCodegen::new().codegen(asm);
                let bytecode_arc = Arc::new(bytecode);
                
                let cache = self
                    .evaler
                    .cache
                    .lock()
                    .map_err(|e| Error::Internal(format!("failed to lock cache: {e}")))?;
                cache.cached_bytecode.insert(self.expr.clone(), bytecode_arc.clone());
                bytecode_arc
            }
        };

        let runner = bc_runner::BytecodeRunner::new(&bytecode, self.evaler.cache.clone());

        // Safety: The bytecode is generated by a internal AsmCodegen.
        unsafe { runner.run(ctx).await }
    }
}

#[cfg(test)]
mod tests {
    use crate::ctx::ExprFn;

    use super::*;

    #[tokio::test]
    async fn test_parse_and_then_eval() {
        // Parse the filter-expr:
        //
        //     name = 'John' AND age > 18 AND 1 > 0
        // =====================================================================
        parse_and_do_test_cases(
            "name = 'John' AND age > 18 AND 1 > 0",
            vec![
                (simple_context! { "name": "John", "age": 19 }, true),
                (simple_context! { "name": "John", "age": 18 }, false),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0
        // =====================================================================
        parse_and_do_test_cases(
            r#"name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0"#,
            vec![
                (simple_context! { "name": "John", "age": 19 }, true),
                (simple_context! { "name": "John", "age": 23 }, false),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     matches(name, "^J.*n$")
        // =====================================================================
        parse_and_do_test_cases(
            r#"matches(name, "^J.*n$")"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Jane" }, false),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     custom_add(1, 2) = 3
        // =====================================================================
        fn with_custom_add_fn(mut ctx: SimpleContext) -> SimpleContext {
            ctx.add_fn("custom_add".to_string(), Box::new(CustomAddFn));
            ctx
        }
        parse_and_do_test_cases(
            r#"custom_add(a, b) = 3"#,
            vec![
                (with_custom_add_fn(simple_context! { "a": 1, "b": 2 }), true),
                (
                    with_custom_add_fn(simple_context! { "a": 1, "b": 3 }),
                    false,
                ),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name != null
        // =====================================================================
        parse_and_do_test_cases(
            r#"name != null"#,
            vec![
                (simple_context! { "name": Value::Null }, false),
                (simple_context! { "name": "John" }, true),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     open > 1.5 AND age > 17.5 AND age < 18.5 AND is_peter = true
        // =====================================================================
        parse_and_do_test_cases(
            r#"open > 1.5 AND age > 17.5 AND age < 18.5 AND is_peter = true"#,
            vec![(
                simple_context! { "open": 1.6, "age": 18, "is_peter": true },
                true,
            )],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name.to_uppercase() = 'JOHN'
        // =====================================================================
        parse_and_do_test_cases(
            r#"name.to_uppercase() = 'JOHN'"#,
            vec![
                (simple_context! { "name": "john" }, true),
                (simple_context! { "name": "Jane" }, false),
                (simple_context! { "name": "John" }, true),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name.contains('John')
        // =====================================================================
        parse_and_do_test_cases(
            r#"name.contains('John')"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Jane" }, false),
                (simple_context! { "name": "The John is a good boy." }, true),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     type(name) = 'str'
        //     type(name) = 'null'
        //     type(foo.contains('bar')) = 'bool'
        //     type(age) = 'i64'
        //     type(open) = 'f64'
        //     type(maybe_i64_or_f64) IN ['i64', 'f64']
        // =====================================================================
        parse_and_do_test_cases(
            r#"type(name) = 'str'"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": 18 }, false),
                (simple_context! { "name": Value::Null }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(name) = 'null'"#,
            vec![
                (simple_context! { "name": "John" }, false),
                (simple_context! { "name": Value::Null }, true),
                (simple_context! { "name": 18 }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(foo.contains('bar')) = 'bool'"#,
            vec![
                (simple_context! { "foo": "foobar" }, true),
                (simple_context! { "foo": "bar and foo" }, true),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(age) = 'i64'"#,
            vec![
                (simple_context! { "age": 18 }, true),
                (simple_context! { "age": 18.5 }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(open) = 'f64'"#,
            vec![
                (simple_context! { "open": 18 }, false),
                (simple_context! { "open": 18.5 }, true),
                (simple_context! { "open": "18" }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"type(maybe_i64_or_f64) IN ['i64', 'f64']"#,
            vec![
                (simple_context! { "maybe_i64_or_f64": 18 }, true),
                (simple_context! { "maybe_i64_or_f64": 18.5 }, true),
                (simple_context! { "maybe_i64_or_f64": "18" }, false),
            ],
        )
        .await;

        // Parse the filter-expr:
        //
        //     name.starts_with('J')
        //     name.ends_with('n')
        // =====================================================================
        parse_and_do_test_cases(
            r#"name.starts_with('J')"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Peterlits" }, false),
            ],
        )
        .await;

        parse_and_do_test_cases(
            r#"name.ends_with('n')"#,
            vec![
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Jane" }, false),
            ],
        )
        .await;
    }

    async fn parse_and_do_test_cases(input: &str, test_cases: Vec<(SimpleContext, bool)>) {
        let filter_expr =
            FilterExpr::parse(input).unwrap_or_else(|_| panic!("failed to parse: {input}"));
        let evaler = FilterExprEvaler::new();

        for (ctx, expected) in test_cases {
            let result = evaler
                .eval_by_bytecode_runner(&filter_expr, &ctx)
                .await
                .unwrap_or_else(|_| panic!("failed to eval by bytecode runner: {input}"));
            assert_eq!(
                result, expected,
                "{input} failed with context with bytecode runner {ctx:?}"
            );
            let result = evaler
                .eval_by_ast_runner(&filter_expr, &ctx)
                .await
                .unwrap_or_else(|_| panic!("failed to eval by ast runner: {input}"));
            assert_eq!(
                result, expected,
                "{input} failed with context with ast runner {ctx:?}"
            );
        }
    }

    struct CustomAddFn;

    #[async_trait::async_trait]
    impl ExprFn for CustomAddFn {
        async fn call(&self, ctx: ExprFnContext) -> Result<Value, Error> {
            if ctx.args.len() != 2 {
                return Err(Error::InvalidArgumentCountForFunction {
                    function: "custom_add".to_string(),
                    expected: 2,
                    got: ctx.args.len(),
                });
            }
            let a = match ctx.args[0] {
                Value::I64(a) => a,
                _ => {
                    return Err(Error::InvalidArgumentTypeForFunction {
                        function: "custom_add".to_string(),
                        index: 0,
                        expected: ValueType::I64,
                        got: ctx.args[0].typ(),
                    });
                }
            };
            let b = match ctx.args[1] {
                Value::I64(b) => b,
                _ => {
                    return Err(Error::InvalidArgumentTypeForFunction {
                        function: "custom_add".to_string(),
                        index: 1,
                        expected: ValueType::I64,
                        got: ctx.args[1].typ(),
                    });
                }
            };
            Ok(Value::i64(a + b))
        }
    }
}
