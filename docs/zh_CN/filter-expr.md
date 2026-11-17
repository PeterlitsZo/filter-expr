# Crate filter-expr

用于将过滤表达式解析为抽象语法树（AST）的库。

```rust
use filter_expr::FilterExpr;

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

你可以自行实现求值，也可以使用 `filter-expr-evaler` crate。

## 示例

可以使用 `=`, `==`, `!=`, `>`, `>=`, `<`, `<=` 比较值：

```text
age >= 18 AND rating > 4.5
```

可以使用算术运算符 `+ - * / %` 以及一元 `+` / `-`：

```text
price * quantity - discount >= 100
/* OR */
(count + 1) % 2 = 0
```

可以使用 `AND` / `and` / `&&`、`OR` / `or` / `||`、`NOT` / `not` / `!` 以及括号组合条件：

```text
active AND (admin OR NOT banned)
/* OR */
active and (admin or not banned)
/* OR */
active && (admin || !banned)
```

可以使用 `IN` / `in` 判断值是否属于数组，或使用 `NOT IN` / `not in` 排除数组中的值：

```text
status IN [200, 201, 204]
/* OR */
status in [200, 201, 204]
/* OR */
status not in [400, 404, 500]
```

字符串可使用单引号或双引号，并用反斜杠转义引号：

```text
first_name = "John" AND last_name = 'O\'Brien'
```

可以使用布尔值和 `null` 字面量：

```text
active = true AND deleted_at = null
```

可以调用求值器提供的函数，并链式调用方法：

```text
matches(name.trim().to_lowercase(), '^john')
```

可以使用点号访问嵌套字段：

```text
user.address.city = 'London'
```

可以使用 `let name := value then body` 绑定中间结果：

```text
let is_foo := a = "foo" then
let is_lucky := n in [1, 2, 3] then
is_foo || is_lucky
```

## 语法参考

### 过滤器

```
<filter> = <expr> | <empty>

<empty> =
```

### 表达式

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

### 比较

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
