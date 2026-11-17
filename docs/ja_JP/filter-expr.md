# Crate filter-expr

フィルター式を抽象構文木（AST）に解析するライブラリです。

```rust
use filter_expr::FilterExpr;

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

評価処理を独自に実装することも、`filter-expr-evaler` クレートを利用することもできます。

## 使用例

`=`, `==`, `!=`, `>`, `>=`, `<`, `<=` で値を比較できます。

```text
age >= 18 AND rating > 4.5
```

算術演算子 `+ - * / %` と単項演算子 `+` / `-` を使用できます。

```text
price * quantity - discount >= 100
/* OR */
(count + 1) % 2 = 0
```

`AND` / `and` / `&&`、`OR` / `or` / `||`、`NOT` / `not` / `!` と括弧で条件を組み合わせられます。

```text
active AND (admin OR NOT banned)
/* OR */
active and (admin or not banned)
/* OR */
active && (admin || !banned)
```

`IN` / `in` で値が配列に含まれるか確認し、`NOT IN` / `not in` で配列内の値を除外できます。

```text
status IN [200, 201, 204]
/* OR */
status in [200, 201, 204]
/* OR */
status not in [400, 404, 500]
```

文字列はシングルクォートまたはダブルクォートで囲み、引用符はバックスラッシュでエスケープできます。

```text
first_name = "John" AND last_name = 'O\'Brien'
```

真偽値と `null` のリテラルを使用できます。

```text
active = true AND deleted_at = null
```

評価器が提供する関数を呼び出したり、メソッドを連鎖させたりできます。

```text
matches(name.trim().to_lowercase(), '^john')
```

ドット記法でネストしたフィールドへのアクセスを記述できます。

```text
user.address.city = 'London'
```

`let name := value then body` で中間結果を束縛できます。

```text
let is_foo := a = "foo" then
let is_lucky := n in [1, 2, 3] then
is_foo || is_lucky
```

## 構文リファレンス

### フィルター

```
<filter> = <expr> | <empty>

<empty> =
```

### 式

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
