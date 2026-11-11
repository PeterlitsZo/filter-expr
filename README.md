# filter-expr

Crates:

- The core crate's README: [filter-expr](crates/filter-expr/README.md)
- The evaler crate's README: [filter-expr-evaler](crates/filter-expr-evaler/README.md)

The `filter-expr` crate defines how to parse the filter expression, and then you
can use the `FilterExpr` as the AST of the filter expression. It do not define
how to evaluate it -- you can do it by yourself or use the `filter-expr-evaler`
crate.

This project is inspired by CEL and the filter syntax in AIP-160 -- but has
different syntax.

## Quick Start

```rust
use filter_expr::FilterExpr;
use filter_expr_evaler::{CompiledFilterExpr, FilterExprEvaler, simple_context};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parse the filter expression.
    let filter = FilterExpr::parse("name = 'John' AND age > 18")?;

    // Provide values for the variables in the expression.
    let ctx = simple_context! {
        "name": "John",
        "age": 19,
    };

    // Compile once, then reuse the handle across contexts.
    let evaler = FilterExprEvaler::new();
    let compiled = CompiledFilterExpr::compile(&filter);
    assert!(evaler.run(&compiled, &ctx).await?);

    Ok(())
}
```

Reuse or clone `compiled` across contexts and evaluators. Each evaluator supplies
its own functions, methods, and regex cache.

## Development

Install [Lefthook](https://lefthook.dev/), then run the following command from
the repository root before starting development:

```sh
lefthook install
```

This enables pre-commit hooks.

## Benchmarks

Run the stage benchmarks with an optimized build:

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```
