# filter-expr-evaler

> NOTE: This library is still under development.

A library for evaluating filter expressions.

```rust
use filter_expr::FilterExpr;
use filter_expr_evaler::{FilterExprEvaler, simple_context};

// Parse the filter expression.
let filter_expr = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

// Build the evaler.
let filter_expr_evaler = FilterExprEvaler::new();

// Do the evaluation.
let ctx = simple_context! {
    "name": "John",
    "age": 19,
};
let result = filter_expr_evaler.eval(&filter_expr, &ctx).await.unwrap();
assert_eq!(result, true);
```
