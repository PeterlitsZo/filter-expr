# Crate filter-expr

필터 표현식을 추상 구문 트리(AST)로 파싱하는 라이브러리입니다.

```rust
use filter_expr::FilterExpr;

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

직접 평가를 구현하거나 `filter-expr-evaler` crate를 사용할 수 있습니다.

## 예제

`=`, `==`, `!=`, `>`, `>=`, `<`, `<=`로 값을 비교할 수 있습니다.

```text
age >= 18 AND rating > 4.5
```

산술 연산자 `+ - * / %`와 단항 연산자 `+` / `-`를 사용할 수 있습니다.

```text
price * quantity - discount >= 100
/* OR */
(count + 1) % 2 = 0
```

`AND` / `and` / `&&`, `OR` / `or` / `||`, `NOT` / `not` / `!` 및 괄호로 조건을 조합할 수 있습니다.

```text
active AND (admin OR NOT banned)
/* OR */
active and (admin or not banned)
/* OR */
active && (admin || !banned)
```

`IN` / `in`으로 값이 배열에 포함되는지 확인하거나 `NOT IN` / `not in`으로 배열의 값을 제외할 수 있습니다.

```text
status IN [200, 201, 204]
/* OR */
status in [200, 201, 204]
/* OR */
status not in [400, 404, 500]
```

문자열을 작은따옴표나 큰따옴표로 감싸고 역슬래시로 따옴표를 이스케이프할 수 있습니다.

```text
first_name = "John" AND last_name = 'O\'Brien'
```

불리언 및 `null` 리터럴을 사용할 수 있습니다.

```text
active = true AND deleted_at = null
```

평가기가 제공하는 함수를 호출하고 메서드를 연속으로 호출할 수 있습니다.

```text
matches(name.trim().to_lowercase(), '^john')
```

점 표기법으로 중첩된 필드에 접근할 수 있습니다.

```text
user.address.city = 'London'
```

`let name := value then body`로 중간 결과를 바인딩할 수 있습니다.

```text
let is_foo := a = "foo" then
let is_lucky := n in [1, 2, 3] then
is_foo || is_lucky
```

## 문법 참조

### 필터

```
<filter> = <expr> | <empty>

<empty> =
```

### 표현식

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

### 비교

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
