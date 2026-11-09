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

You can compare values with `=`, `==`, `!=`, `>`, `>=`, `<`, and `<=`:

```text
age >= 18 AND rating > 4.5
```

You can use arithmetic operators `+ - * / %` and unary `+` / `-`:

```text
price * quantity - discount >= 100
/* OR */
(count + 1) % 2 = 0
```

You can combine conditions with `AND` / `and` / `&&`, `OR` / `or` / `||`,
and `NOT` / `not` / `!`, plus parentheses:

```text
active AND (admin OR NOT banned)
/* OR */
active and (admin or not banned)
/* OR */
active && (admin || !banned)
```

You can check whether a value belongs to an array with `IN` / `in`, or exclude
values with `NOT IN` / `not in`:

```text
status IN [200, 201, 204]
/* OR */
status in [200, 201, 204]
/* OR */
status not in [400, 404, 500]
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

You can bind intermediate results with `let name := value then body`:

```text
let is_foo := a = "foo" then
let is_lucky := n in [1, 2, 3] then
is_foo || is_lucky
```

## Syntax Reference

### Filter

```
<filter> = <expr> | <empty>

<empty> =
```

### Expression

```
<expr> = <let-expr> | <logical-expr>
<let-expr> = 'let' <ident> ':=' <logical-expr> 'then' <expr>
<logical-expr> = <or_test> (<or> <or_test>)*

<or_test> = <and_test> (<and> <and_test>)*
<and_test> = <not> <and_test> | <comparison>

<or> = 'OR' | 'or' | '||'
<and> = 'AND' | 'and' | '&&'
<not> = 'NOT' | 'not' | '!'
```

### Comparison

```
<comparison> = <additive> <operator> <comparison>
             | <additive>

<additive> = <multiplicative> (('+' | '-') <multiplicative>)*
<multiplicative> = <unary> (('*' | '/' | '%') <unary>)*
<unary> = ('+' | '-') <unary> | <primary>

<primary> = <func-call>
          | <method-call>
          | <field-access>
          | <value>

<operator> = '=' | '==' | '>' | '<' | '>=' | '<=' | '!=' | <in> | ('NOT' | 'not') <in>
<in> = 'IN' | 'in'

<namespace-prefix> = (<ident> '::')+
<func-call> = <namespace-prefix>? <ident> '(' [<expr> (',' <expr>)* ','?] ')'
<method-call> = <value> '.' <ident> '(' [<expr> (',' <expr>)* ','?] ')'
<field-access> = <value> '.' <ident>

<value> = <str>
        | <i64>
        | <f64>
        | <bool>
        | <null>
        | <ident>
        | <array>
        | <object>
        | '(' <expr> ')'

<array> = '[' [<expr> (',' <expr>)* ','?] ']'
<object> = '{' [<str> ':' <expr> (',' <str> ':' <expr>)* ','?] '}'
```
