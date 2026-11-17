# Crate filter-expr

用於將篩選運算式解析為抽象語法樹（AST）的函式庫。

```rust
use filter_expr::FilterExpr;

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

你可以自行實作求值，也可以使用 `filter-expr-evaler` crate。

## 範例

可以使用 `=`, `==`, `!=`, `>`, `>=`, `<`, `<=` 比較值：

```text
age >= 18 AND rating > 4.5
```

可以使用算術運算子 `+ - * / %` 以及一元 `+` / `-`：

```text
price * quantity - discount >= 100
/* OR */
(count + 1) % 2 = 0
```

可以使用 `AND` / `and` / `&&`、`OR` / `or` / `||`、`NOT` / `not` / `!` 以及括號組合條件：

```text
active AND (admin OR NOT banned)
/* OR */
active and (admin or not banned)
/* OR */
active && (admin || !banned)
```

可以使用 `IN` / `in` 判斷值是否屬於陣列，或使用 `NOT IN` / `not in` 排除陣列中的值：

```text
status IN [200, 201, 204]
/* OR */
status in [200, 201, 204]
/* OR */
status not in [400, 404, 500]
```

字串可使用單引號或雙引號，並用反斜線跳脫引號：

```text
first_name = "John" AND last_name = 'O\'Brien'
```

可以使用布林值和 `null` 字面值：

```text
active = true AND deleted_at = null
```

可以呼叫求值器提供的函式，並串接方法呼叫：

```text
matches(name.trim().to_lowercase(), '^john')
```

可以使用點號存取巢狀欄位：

```text
user.address.city = 'London'
```

可以使用 `let name := value then body` 繫結中間結果：

```text
let is_foo := a = "foo" then
let is_lucky := n in [1, 2, 3] then
is_foo || is_lucky
```

## 語法參考

### 篩選器

```
<filter> = <expr> | <empty>

<empty> =
```

### 運算式

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

### 比較

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
