# Crate filter-expr

Uma biblioteca para analisar expressões de filtro e gerar uma árvore sintática abstrata (AST).

```rust
use filter_expr::FilterExpr;

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

Você pode implementar a avaliação ou usar o crate `filter-expr-evaler`.

## Exemplos

Você pode comparar valores com `=`, `==`, `!=`, `>`, `>=`, `<` e `<=`:

```text
age >= 18 AND rating > 4.5
```

Você pode usar os operadores aritméticos `+ - * / %` e os unários `+` / `-`:

```text
price * quantity - discount >= 100
/* OR */
(count + 1) % 2 = 0
```

Você pode combinar condições com `AND` / `and` / `&&`, `OR` / `or` / `||`, `NOT` / `not` / `!` e parênteses:

```text
active AND (admin OR NOT banned)
/* OR */
active and (admin or not banned)
/* OR */
active && (admin || !banned)
```

Você pode verificar se um valor pertence a um array com `IN` / `in` ou excluir valores com `NOT IN` / `not in`:

```text
status IN [200, 201, 204]
/* OR */
status in [200, 201, 204]
/* OR */
status not in [400, 404, 500]
```

Você pode delimitar strings com aspas simples ou duplas e escapar as aspas com uma barra invertida:

```text
first_name = "John" AND last_name = 'O\'Brien'
```

Você pode usar literais booleanos e `null`:

```text
active = true AND deleted_at = null
```

Você pode chamar funções e encadear métodos fornecidos pelo avaliador:

```text
matches(name.trim().to_lowercase(), '^john')
```

Você pode acessar campos aninhados com a notação de ponto:

```text
user.address.city = 'London'
```

Você pode vincular resultados intermediários com `let name := value then body`:

```text
let is_foo := a = "foo" then
let is_lucky := n in [1, 2, 3] then
is_foo || is_lucky
```

## Referência de sintaxe

### Filtro

```
<filter> = <expr> | <empty>

<empty> =
```

### Expressão

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

### Comparação

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
