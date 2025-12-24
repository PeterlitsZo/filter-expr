# filter-expr

> NOTE: This library is still under development.

A library for parsing the filter expression.

```rust
use filter_expr::{FilterExpr, SimpleContext};

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();
let ctx = SimpleContext::new(HashMap::from([
    ("name".to_string(), "John".into()),
    ("age".to_string(), 19.into()),
]));
let result = f.eval(&ctx).await.unwrap();
assert_eq!(result, true);
```

## Syntax

### Filter

```
<filter> = <expr> | <empty>

<empty> =
```

### Expression

```
<expr> = <or_test> ('OR' <or_test>)*

<or_test> = <and_test> ('AND' <and_test>)*
<and_test> = ['NOT'] <comparison>
```

### Comparison

```
<comparison> = <primary> <operator> <comparison>
             | <primary>

<primary> = <func-call>
          | <method-call>
          | <value>

<operator> = '=' | '>' | '<' | '>=' | '<=' | '!=' | 'IN'

<func-call> = <ident> '(' [<value> (',' <value>)* ','?] ')'
<method-call> = <value> '.' <ident> '(' [<value> (',' <value>)* ','?] ')'

<value> = <str>
        | <i64>
        | <f64>
        | <bool>
        | <null>
        | <ident>
        | <array>
        | '(' <expr> ')'

<array> = '[' [<value> (',' <value>)* ','?] ']'
```
