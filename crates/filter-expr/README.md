# filter-expr

> NOTE: This library is still under development.

A library for parsing the filter expression.

```rust
use filter_expr::{FilterExpr, SimpleContext};

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

You can evaluate it by yourself or use the `filter-expr-evaler` crate.

## Syntax

### Filter

```text
<filter> = <expr> | <empty>

<empty> =
```

### Expression

```text
<expr> = <or_test> ('OR' <or_test>)*

<or_test> = <and_test> ('AND' <and_test>)*
<and_test> = ['NOT'] <comparison>
```

### Comparison

```text
<comparison> = <primary> <operator> <primary>
             | <primary>

<operator> = '=' | '>' | '<' | '>=' | '<=' | '!=' | 'IN'
```

### Primary

```text
<primary> = <func-call>
          | <method-call>
          | <value>

<func-call> = <function-path> '(' [<value> (',' <value>)* ','?] ')'
<method-call> = <value> '.' <ident> '(' [<value> (',' <value>)* ','?] ')'

<function-path> = <ident> ('::' <ident>)*
```

### Value

```text
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
