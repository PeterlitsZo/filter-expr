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
          | <field-access>
          | <value>

<operator> = '=' | '>' | '<' | '>=' | '<=' | '!=' | 'IN'

<func-call> = <ident> '(' [<value> (',' <value>)* ','?] ')'
<method-call> = <value> '.' <ident> '(' [<value> (',' <value>)* ','?] ')'
<field-access> = <value> '.' <ident>

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
