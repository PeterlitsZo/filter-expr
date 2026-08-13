//! Evaluator for filter expressions.

mod asm;
mod asm_codegen;
mod ast_runner;
mod bc;
mod bc_codegen;
mod bc_runner;
mod builtin;
mod callable;
mod ctx;
mod error;
mod value;

use std::collections::BTreeMap;
use std::sync::{Arc, RwLock};

use filter_expr::{Expr, FilterExpr};
use moka::sync::Cache;
use regex::Regex;

use crate::ast_runner::AstRunner;
use crate::bc::Bytecode;

pub use crate::callable::{ArcFunction, Function, FunctionContext};
pub use crate::callable::{ArcMethod, Method, MethodContext};
pub use crate::ctx::{Context, SimpleContext};
pub use crate::error::{Error, ErrorKind, Result};
pub use crate::value::{Userdata, Value, ValueType};

/// The environment for the filter expression evaluator.
#[derive(Clone)]
pub struct FilterExprEvalerEnv {
    inner: Arc<RwLock<FilterExprEvalerEnvInner>>,
}

struct FilterExprEvalerEnvInner {
    /// The cached compiled regex.
    ///
    /// It is useful to avoid compiling the same regex multiple times.
    cached_regex: Cache<String, Arc<Regex>>,

    /// The functions.
    functions: BTreeMap<String, ArcFunction>,

    /// The methods.
    methods: BTreeMap<(String, ValueType), ArcMethod>,
}

impl FilterExprEvalerEnv {
    /// Create a new environment.
    pub(crate) fn new() -> Self {
        // Initialize the builtin functions.
        let mut functions: BTreeMap<String, ArcFunction> = BTreeMap::new();
        functions.insert("matches".to_string(), Arc::new(builtin::FunctionMatches));
        functions.insert("type".to_string(), Arc::new(builtin::FunctionType));

        // Initialize the builtin methods.
        let mut methods: BTreeMap<(String, ValueType), ArcMethod> = BTreeMap::new();
        methods.insert(
            ("to_uppercase".to_string(), ValueType::Str),
            Arc::new(builtin::MethodStrToUppercase),
        );
        methods.insert(
            ("to_lowercase".to_string(), ValueType::Str),
            Arc::new(builtin::MethodStrToLowercase),
        );
        methods.insert(
            ("contains".to_string(), ValueType::Str),
            Arc::new(builtin::MethodStrContains),
        );
        methods.insert(
            ("starts_with".to_string(), ValueType::Str),
            Arc::new(builtin::MethodStrStartsWith),
        );
        methods.insert(
            ("ends_with".to_string(), ValueType::Str),
            Arc::new(builtin::MethodStrEndsWith),
        );

        let inner = Arc::new(RwLock::new(FilterExprEvalerEnvInner {
            cached_regex: Cache::new(128),
            functions,
            methods,
        }));

        Self { inner }
    }

    /// Add a function to the environment.
    pub(crate) fn add_function(&self, name: String, function: ArcFunction) -> Result<()> {
        self.inner
            .write()
            .map_err(|e| {
                Error::new(ErrorKind::Internal, "failed to lock env")
                    .with_metadata("error", e.to_string())
            })?
            .functions
            .insert(name, function);
        Ok(())
    }

    /// Add a method to the environment.
    pub(crate) fn add_method(
        &self,
        name: String,
        obj_type: ValueType,
        method: ArcMethod,
    ) -> Result<()> {
        self.inner
            .write()
            .map_err(|e| {
                Error::new(ErrorKind::Internal, "failed to lock env")
                    .with_metadata("error", e.to_string())
            })?
            .methods
            .insert((name, obj_type), method);
        Ok(())
    }

    /// Get a function from the environment.
    pub(crate) fn get_function(&self, name: &str) -> Result<ArcFunction> {
        let inner = self.inner.read().map_err(|e| {
            Error::new(ErrorKind::Internal, "failed to lock env")
                .with_metadata("error", e.to_string())
        })?;
        let function = inner.functions.get(name).ok_or_else(|| {
            Error::new(ErrorKind::FailedToGet, "no such function").with_metadata("function", name)
        })?;
        Ok(Arc::clone(function))
    }

    /// Get a method from the environment.
    pub(crate) fn get_method(&self, name: &str, obj_type: ValueType) -> Result<ArcMethod> {
        let inner = self.inner.read().map_err(|e| {
            Error::new(ErrorKind::Internal, "failed to lock env")
                .with_metadata("error", e.to_string())
        })?;
        let method = inner
            .methods
            .get(&(name.to_string(), obj_type))
            .ok_or_else(|| {
                Error::new(ErrorKind::FailedToGet, "no such method")
                    .with_metadata("method", name)
                    .with_metadata("obj_type", obj_type)
            })?;
        Ok(Arc::clone(method))
    }
}

impl FilterExprEvalerEnv {
    /// Get a regex (if cached, return the cached one; otherwise, compile and
    /// cache it).
    pub(crate) fn get_regex(&self, pattern: &str) -> Result<Arc<Regex>> {
        let inner = self.inner.read().map_err(|e| {
            Error::new(ErrorKind::Internal, "failed to lock env")
                .with_metadata("error", e.to_string())
        })?;
        let cached_regex = inner.cached_regex.get(pattern);
        if let Some(cached) = cached_regex {
            Ok(cached)
        } else {
            let regex = Regex::new(pattern).map_err(|e| {
                Error::new(ErrorKind::InvalidValue, "failed to compile regex")
                    .with_metadata("pattern", pattern)
                    .with_source(e)
            })?;
            let regex_arc = Arc::new(regex);
            inner
                .cached_regex
                .insert(pattern.to_string(), regex_arc.clone());
            Ok(regex_arc)
        }
    }
}

pub struct FilterExprEvaler {
    /// The cached compiled bytecode.
    ///
    /// Used to avoid compiling the same expression multiple times.
    cached_bytecode: Cache<Expr, Arc<Bytecode>>,

    /// The global environment shared by runners.
    env: FilterExprEvalerEnv,
}

impl Default for FilterExprEvaler {
    fn default() -> Self {
        Self::new()
    }
}

impl FilterExprEvaler {
    /// Create a new filter expression evaluator.
    pub fn new() -> Self {
        Self {
            cached_bytecode: Cache::new(128),
            env: FilterExprEvalerEnv::new(),
        }
    }

    /// Evaluate the filter expression using the default runner.
    pub async fn eval(&self, filter_expr: &FilterExpr, ctx: &dyn Context) -> Result<bool> {
        self.eval_by_bytecode_runner(filter_expr, ctx).await
    }

    /// Evaluate the filter expression using AST runner.
    pub async fn eval_by_ast_runner(
        &self,
        filter_expr: &FilterExpr,
        ctx: &dyn Context,
    ) -> Result<bool> {
        if let Some(expr) = filter_expr.expr() {
            let ast_runner = AstRunner::new(expr, self.env.clone());
            let value = ast_runner.run(ctx).await?;

            match value {
                Value::Bool(b) => Ok(b),
                _ => Err(Error::new(ErrorKind::InvalidValue, "expected bool")
                    .with_metadata("got", value.typ())),
            }
        } else {
            Ok(true)
        }
    }

    /// Evaluate the filter expression using bytecode execution.
    pub async fn eval_by_bytecode_runner(
        &self,
        filter_expr: &FilterExpr,
        ctx: &dyn Context,
    ) -> Result<bool> {
        if let Some(expr) = filter_expr.expr() {
            let bytecode = {
                let cached_bytecode = self.cached_bytecode.get(expr);

                // Check if bytecode is cached, generate if not.
                if let Some(cached) = cached_bytecode {
                    cached
                } else {
                    // Generate bytecode and cache it.
                    let asm = asm_codegen::AsmCodegen::new().codegen(expr);
                    let bytecode = bc_codegen::BytecodeCodegen::new().codegen(asm);
                    let bytecode_arc = Arc::new(bytecode);

                    self.cached_bytecode
                        .insert(expr.clone(), bytecode_arc.clone());
                    bytecode_arc
                }
            };

            let runner = bc_runner::BytecodeRunner::new(&bytecode, self.env.clone());
            let value = unsafe { runner.run(ctx).await }?;

            match value {
                Value::Bool(b) => Ok(b),
                _ => Err(Error::new(ErrorKind::InvalidValue, "expected bool")
                    .with_metadata("got", value.typ())),
            }
        } else {
            Ok(true)
        }
    }

    pub fn add_function<N>(&self, name: N, function: ArcFunction) -> Result<()>
    where
        N: Into<String>,
    {
        self.env.add_function(name.into(), function)
    }

    pub fn add_method<N>(&self, name: N, obj_type: ValueType, method: ArcMethod) -> Result<()>
    where
        N: Into<String>,
    {
        self.env.add_method(name.into(), obj_type, method)
    }
}

#[cfg(test)]
mod tests {
    use crate::callable::Function;

    use super::*;

    #[tokio::test]
    async fn test_parse_and_then_eval() {
        let evaler = FilterExprEvaler::new();
        evaler
            .add_function("custom_add", Arc::new(CustomAddFn))
            .unwrap();
        evaler
            .add_function("new_magic", Arc::new(NewMagic))
            .unwrap();

        macro_rules! parse_and_do_test_cases {
            ($input:expr, $test_cases:expr $(,)?) => {
                parse_and_do_test_cases(&evaler, $input, $test_cases).await
            };
        }

        // Parse the filter-expr:
        //
        //     name = 'John' AND age > 18 AND 1 > 0
        // =====================================================================
        parse_and_do_test_cases!(
            "name = 'John' AND age > 18 AND 1 > 0",
            &[
                (simple_context! { "name": "John", "age": 19 }, true),
                (simple_context! { "name": "John", "age": 18 }, false),
            ],
        );

        // Parse the filter-expr:
        //
        //     name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0
        // =====================================================================
        parse_and_do_test_cases!(
            r#"name = "John" AND age IN [18, 19, 20, 22] AND 1 > 0"#,
            &[
                (simple_context! { "name": "John", "age": 19 }, true),
                (simple_context! { "name": "John", "age": 23 }, false),
            ],
        );

        // Parse the filter-expr:
        //
        //     matches(name, "^J.*n$")
        // =====================================================================
        parse_and_do_test_cases!(
            r#"matches(name, "^J.*n$")"#,
            &[
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Jane" }, false),
            ],
        );

        // Parse the filter-expr:
        //
        //     custom_add(1, 2) = 3
        // =====================================================================
        parse_and_do_test_cases!(
            r#"custom_add(a, b) = 3"#,
            &[
                (simple_context! { "a": 1, "b": 2 }, true),
                (simple_context! { "a": 1, "b": 3 }, false),
            ],
        );

        // Parse the filter-expr:
        //
        //     name != null
        // =====================================================================
        parse_and_do_test_cases!(
            r#"name != null"#,
            &[
                (simple_context! { "name": Value::Null }, false),
                (simple_context! { "name": "John" }, true),
            ],
        );

        // Parse the filter-expr:
        //
        //     open > 1.5 AND age > 17.5 AND age < 18.5 AND is_peter = true
        // =====================================================================
        parse_and_do_test_cases!(
            r#"open > 1.5 AND age > 17.5 AND age < 18.5 AND is_peter = true"#,
            &[(
                simple_context! { "open": 1.6, "age": 18, "is_peter": true },
                true,
            )],
        );

        // Parse the filter-expr:
        //
        //     name.to_uppercase() = 'JOHN'
        // =====================================================================
        parse_and_do_test_cases!(
            r#"name.to_uppercase() = 'JOHN'"#,
            &[
                (simple_context! { "name": "john" }, true),
                (simple_context! { "name": "Jane" }, false),
                (simple_context! { "name": "John" }, true),
            ],
        );

        // Parse the filter-expr:
        //
        //     name.contains('John')
        // =====================================================================
        parse_and_do_test_cases!(
            r#"name.contains('John')"#,
            &[
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Jane" }, false),
                (simple_context! { "name": "The John is a good boy." }, true),
            ],
        );

        // Parse the filter-expr:
        //
        //     type(name) = 'str'
        //     type(name) = 'null'
        //     type(foo.contains('bar')) = 'bool'
        //     type(age) = 'i64'
        //     type(open) = 'f64'
        //     type(maybe_i64_or_f64) IN ['i64', 'f64']
        // =====================================================================
        parse_and_do_test_cases!(
            r#"type(name) = 'str'"#,
            &[
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": 18 }, false),
                (simple_context! { "name": Value::Null }, false),
            ],
        );

        parse_and_do_test_cases!(
            r#"type(name) = 'null'"#,
            &[
                (simple_context! { "name": "John" }, false),
                (simple_context! { "name": Value::Null }, true),
                (simple_context! { "name": 18 }, false),
            ],
        );

        parse_and_do_test_cases!(
            r#"type(foo.contains('bar')) = 'bool'"#,
            &[
                (simple_context! { "foo": "foobar" }, true),
                (simple_context! { "foo": "bar and foo" }, true),
            ],
        );

        parse_and_do_test_cases!(
            r#"type(age) = 'i64'"#,
            &[
                (simple_context! { "age": 18 }, true),
                (simple_context! { "age": 18.5 }, false),
            ],
        );

        parse_and_do_test_cases!(
            r#"type(open) = 'f64'"#,
            &[
                (simple_context! { "open": 18 }, false),
                (simple_context! { "open": 18.5 }, true),
                (simple_context! { "open": "18" }, false),
            ],
        );

        parse_and_do_test_cases!(
            r#"type(maybe_i64_or_f64) IN ['i64', 'f64']"#,
            &[
                (simple_context! { "maybe_i64_or_f64": 18 }, true),
                (simple_context! { "maybe_i64_or_f64": 18.5 }, true),
                (simple_context! { "maybe_i64_or_f64": "18" }, false),
            ],
        );

        // Parse the filter-expr:
        //
        //     name.starts_with('J')
        //     name.ends_with('n')
        // =====================================================================
        parse_and_do_test_cases!(
            r#"name.starts_with('J')"#,
            &[
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Peterlits" }, false),
            ],
        );

        parse_and_do_test_cases!(
            r#"name.ends_with('n')"#,
            &[
                (simple_context! { "name": "John" }, true),
                (simple_context! { "name": "Jane" }, false),
            ],
        );

        // Parse the filter-expr:
        //
        //     new_magic('foo') = new_magic(expected)
        //     new_magic('foo') = expected
        // =====================================================================
        parse_and_do_test_cases!(
            r#"new_magic('foo') = new_magic(expected)"#,
            &[
                (simple_context! { "expected": "foo" }, true),
                (simple_context! { "expected": "bar" }, false),
            ],
        );
        parse_and_do_test_cases!(
            r#"new_magic('foo') = expected"#,
            &[
                (simple_context! { "expected": "foo" }, true),
                (simple_context! { "expected": "bar" }, false),
                (simple_context! { "expected": Value::Userdata(Arc::new(MagicUserdata::Foo)) }, true),
                (simple_context! { "expected": Value::Userdata(Arc::new(MagicUserdata::Bar)) }, false),
            ],
        );
    }

    async fn parse_and_do_test_cases(
        evaler: &FilterExprEvaler,
        input: &str,
        test_cases: &[(SimpleContext, bool)],
    ) {
        let filter_expr =
            FilterExpr::parse(input).unwrap_or_else(|_| panic!("failed to parse: {input}"));

        for (ctx, expected) in test_cases {
            let result = evaler
                .eval_by_bytecode_runner(&filter_expr, ctx)
                .await
                .unwrap_or_else(|e| {
                    panic!("failed to eval by bytecode runner (input={input}): {e}")
                });
            assert_eq!(
                result, *expected,
                "{input} failed with context with bytecode runner {ctx:?}"
            );
            let result = evaler
                .eval_by_ast_runner(&filter_expr, ctx)
                .await
                .unwrap_or_else(|e| panic!("failed to eval by ast runner (input={input}): {e}"));
            assert_eq!(
                result, *expected,
                "{input} failed with context with ast runner {ctx:?}"
            );
        }
    }

    #[derive(Debug, PartialEq, PartialOrd)]
    enum MagicUserdata {
        Foo,
        Bar,
    }

    impl Userdata for MagicUserdata {
        fn as_any(&self) -> &dyn std::any::Any {
            self
        }

        fn partial_eq(&self, other: &Value) -> Result<bool> {
            match other {
                Value::Str(other) => Ok(match other.as_str() {
                    "foo" => *self == Self::Foo,
                    "bar" => *self == Self::Bar,
                    _ => false,
                }),
                Value::Userdata(other) => Ok(other
                    .as_any()
                    .downcast_ref::<MagicUserdata>()
                    .is_some_and(|other| self == other)),
                _ => Ok(false),
            }
        }

        fn partial_ord(&self, other: &Value) -> Result<Option<std::cmp::Ordering>> {
            match other {
                Value::Userdata(other) => Ok(other
                    .as_any()
                    .downcast_ref::<MagicUserdata>()
                    .and_then(|other| self.partial_cmp(other))),
                _ => Err(Error::new(
                    ErrorKind::TypeMismatch,
                    "cannot compare MagicUserdata with a value having different type",
                )),
            }
        }
    }

    struct NewMagic;

    #[async_trait::async_trait]
    impl Function for NewMagic {
        async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value> {
            if ctx.args.len() != 1 {
                return Err(
                    Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                        .with_metadata("function", "new_magic")
                        .with_metadata("expected", 1)
                        .with_metadata("got", ctx.args.len()),
                );
            }
            let val = match ctx.args[0] {
                Value::Str(ref val) => val.clone(),
                _ => {
                    return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                        .with_metadata("function", "custom_add")
                        .with_metadata("index", 0)
                        .with_metadata("expected", ValueType::Str)
                        .with_metadata("got", ctx.args[0].typ()));
                }
            };
            match val.as_str() {
                "foo" => Ok(Value::Userdata(Arc::new(MagicUserdata::Foo))),
                "bar" => Ok(Value::Userdata(Arc::new(MagicUserdata::Bar))),
                _ => Err(Error::new(
                    ErrorKind::InvalidValue,
                    "want a string contains 'foo' or 'bar'",
                )),
            }
        }
    }

    struct CustomAddFn;

    #[async_trait::async_trait]
    impl Function for CustomAddFn {
        async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value> {
            if ctx.args.len() != 2 {
                return Err(
                    Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                        .with_metadata("function", "custom_add")
                        .with_metadata("expected", 2)
                        .with_metadata("got", ctx.args.len()),
                );
            }
            let a = match ctx.args[0] {
                Value::I64(a) => a,
                _ => {
                    return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                        .with_metadata("function", "custom_add")
                        .with_metadata("index", 0)
                        .with_metadata("expected", ValueType::I64)
                        .with_metadata("got", ctx.args[0].typ()));
                }
            };
            let b = match ctx.args[1] {
                Value::I64(b) => b,
                _ => {
                    return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                        .with_metadata("function", "custom_add")
                        .with_metadata("index", 1)
                        .with_metadata("expected", ValueType::I64)
                        .with_metadata("got", ctx.args[1].typ()));
                }
            };
            Ok(Value::i64(a + b))
        }
    }
}
