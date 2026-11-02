# filter-expr

A library for parsing the filter expression as an AST.

```rust
use filter_expr::FilterExpr;

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

You can evaluate it by yourself or use the `filter-expr-evaler` crate.

## Examples

You can compare values with `=`, `!=`, `>`, `>=`, `<`, and `<=`:

```text
age >= 18 AND rating > 4.5
```

You can combine conditions with `AND`, `OR`, `NOT`, and parentheses:

```text
active AND (admin OR NOT banned)
```

You can check whether a value belongs to an array with `IN`:

```text
status IN [200, 201, 204]
```

You can quote strings with single or double quotes and escape quotes with a
backslash:

```text
first_name = "John" AND last_name = 'O\'Brien'
```

You can use boolean and `null` literals:

```text
active = true AND deleted_at = null
```

You can call functions and chain methods provided by your evaluator:

```text
matches(name.trim().to_lowercase(), '^john')
```

You can parse nested field access with dot notation:

```text
user.address.city = 'London'
```

## Syntax Reference

### Filter

```
<filter> = <expr> | <empty>

<empty> =
```

### Expression

```
<expr> = <or_test> ('OR' <or_test>)*

<or_test> = <and_test> ('AND' <and_test>)*
<and_test> = ('NOT' | '!') <and_test> | <comparison>
```

### Comparison

```
<comparison> = <primary> <operator> <comparison>
             | <primary>

<primary> = <func-call>
          | <method-call>
          | <field-access>
          | <value>

<operator> = '=' | '>' | '<' | '>=' | '<=' | '!=' | 'IN' | 'NOT' 'IN'

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
