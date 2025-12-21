# filter-expr

A library for parsing the filter expression.

```rust
use filter_expr::{FilterExpr, SimpleContext};

let f = FilterExpr::parse("name = 'John' and age > 18").unwrap();
let ctx = SimpleContext::new(HashMap::from([
    ("name".to_string(), "John".into()),
    ("age".to_string(), 19.into()),
]));
let result = f.eval(&ctx).await.unwrap();
assert_eq!(result, true);
```

It is based on [AIP-160](https://google.aip.dev/160) filter syntax.
