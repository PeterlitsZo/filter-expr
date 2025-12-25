# filter-expr-evaler

> NOTE: This library is still under development.

A library for evaluating filter expressions.

```rust
use filter_expr::FilterExpr;
use filter_expr_evaler::{FilterExprEvaler, simple_context};

// Parse the filter expression.
let filter_expr = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

// Build the evaler to evaluate in a given context.
let filter_expr_evaler = FilterExprEvaler::new(filter_expr);

// Do the evaluation.
let ctx = simple_context! {
    "name": "John",
    "age": 19,
};
let result = filter_expr.eval(&ctx).await.unwrap();
assert_eq!(result, true);
```
