# filter-expr

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
<filter> = <expression> | <empty>

<empty> =
```

### Expression

```
<expression> = <sequence> ('AND' <sequence>)*

<sequence> = <factor>+
<factor> = <term> ('OR' <term>)*
<term> = ['NOT' | '-'] <simple>
<simple> = <restriction> | <composite>
```

### Restriction

```
<restriction> = <comparable> [<operator> <arg>]
```

E.g. `name = 'John'` or `age > 18` or `1 > 0`.

### Composite

```
<composite> = '(' <expression> ')'
```

E.g. `(age + 18)`.

### Comparable

```
<comparable> = <member> | <function> | <value>
```

E.g. `name` or `age` or `1`.

### Operator

```
<operator> = '=' | '>' | '<' | '>=' | '<=' | '!=' | 'IN' | 'NOT' 'IN'
```

### Arg

```
<arg> = <comparable> | <composite>
```

E.g. `'John'` or `18` or `1.0` or `(age + 18)`.

### Member

```
<member> = <field> ('.' <field>)*
```

E.g. `name` or `user.name` or `user.address.city`.

### Field

E.g. `name` or `user_name` or `user_address_city`.

### Value

```
<value> = <string>
        | <integer>
        | <float>
        | <boolean>
        | <null>
        | <array>
        | <object>

<string> = <single-quoted-string> | <double-quoted-string>
<boolean> = 'true' | 'false'
<integer> = /* an integer number */
<float> = /* a floating-point number */
<null> = 'null'
<array> = '[' [<value> (',' <value>)* ','?] ']'
<object> = '{' [<string> ':' <value> (',' <string> ':' <value>)* ','?] '}'
```

Examples:

- `'John'`
- `18`
- `1.0`
- `true`
- `false`
- `null`
- `[1, 2, 3]`
- `{'a': 1, 'b': 2}`
