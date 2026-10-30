# filter-expr-evaler

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

## Builtin Functions

- **`matches(text: str, pattern: str) -> bool`**. Checks whether a regular
  expression matches anywhere in the text. For example,
  `matches('foobar', '^foo')` returns `true`.
- **`type(value) -> str`**. Returns the type name of a value. For example,
  `type(42)` returns `'i64'`.

## Builtin Methods

### str

- **`to_uppercase() -> str`**. Converts to Unicode uppercase.
- **`to_lowercase() -> str`**. Converts to Unicode lowercase.
- **`len() -> i64`**. Returns the length in UTF-8 bytes.
- **`is_empty() -> bool`**. Checks whether the string is empty.
- **`contains(needle: str) -> bool`**. Checks for a substring.
- **`starts_with(prefix: str) -> bool`**. Checks for a matching prefix.
- **`ends_with(suffix: str) -> bool`**. Checks for a matching suffix.
- **`trim() -> str`**. Removes leading and trailing Unicode whitespace.
- **`trim_start() -> str`**. Removes leading Unicode whitespace.
- **`trim_end() -> str`**. Removes trailing Unicode whitespace.
- **`strip_prefix(prefix: str) -> str or null`**. Removes a matching prefix
  once.
- **`strip_suffix(suffix: str) -> str or null`**. Removes a matching suffix
  once.
