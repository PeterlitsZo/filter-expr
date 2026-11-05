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

## Benchmarks

Run the stage benchmarks with an optimized build:

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```

The suite covers comparisons, logical expressions, arithmetic and membership,
function/method calls, and short-circuit evaluation. It reports nanoseconds per
operation (median/min/max of 20 batch averages) for:

- Parsing, including tokenization.
- AST to assembly generation.
- Assembly to bytecode generation.
- AST execution.
- Bytecode execution without a cache lookup.
- Public `eval` with a warmed bytecode cache.
- Parsing, compilation, and bytecode execution together, bypassing the cache.

Each stage gets 1,024 warm-up operations and 1,024 operations per sample. Override
the batch size with `BENCH_ITERATIONS`, for example:

```sh
BENCH_ITERATIONS=4096 cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```

Runtime, evaluator, and context creation happen outside the clock. Each execution
creates a fresh runner; generated output destruction is included in timings.
Assembly inputs are cloned before timing the bytecode generation stage. Async
operations run sequentially within one runtime entry. Stage timings are measured
independently and should not be added together as an exact end-to-end estimate.
These are local timing summaries, not statistical regression tests; compare runs
on the same machine under similar load. The `bench-internals` feature only enables
the internal benchmark entry point and is not needed for normal library use.

## Quick Start

```rust
use filter_expr::FilterExpr;
use filter_expr_evaler::{FilterExprEvaler, simple_context};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parse the filter expression.
    let filter = FilterExpr::parse("name = 'John' AND age > 18")?;

    // Provide values for the variables in the expression.
    let ctx = simple_context! {
        "name": "John",
        "age": 19,
    };

    // Evaluate the expression against the context.
    let evaler = FilterExprEvaler::new();
    assert!(evaler.eval(&filter, &ctx).await?);

    Ok(())
}
```
