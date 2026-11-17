# Crate filter-expr

Una biblioteca para analizar expresiones de filtrado y generar un árbol de sintaxis abstracta (AST).

```rust
use filter_expr::FilterExpr;

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

Puedes implementar la evaluación por tu cuenta o usar el crate `filter-expr-evaler`.

## Ejemplos

Puedes comparar valores con `=`, `==`, `!=`, `>`, `>=`, `<` y `<=`:

```text
age >= 18 AND rating > 4.5
```

Puedes usar los operadores aritméticos `+ - * / %` y los unarios `+` / `-`:

```text
price * quantity - discount >= 100
/* OR */
(count + 1) % 2 = 0
```

Puedes combinar condiciones con `AND` / `and` / `&&`, `OR` / `or` / `||`, `NOT` / `not` / `!` y paréntesis:

```text
active AND (admin OR NOT banned)
/* OR */
active and (admin or not banned)
/* OR */
active && (admin || !banned)
```

Puedes comprobar si un valor pertenece a un array con `IN` / `in`, o excluir valores con `NOT IN` / `not in`:

```text
status IN [200, 201, 204]
/* OR */
status in [200, 201, 204]
/* OR */
status not in [400, 404, 500]
```

Puedes delimitar cadenas con comillas simples o dobles y escapar las comillas con una barra invertida:

```text
first_name = "John" AND last_name = 'O\'Brien'
```

Puedes usar literales booleanos y `null`:

```text
active = true AND deleted_at = null
```

Puedes llamar a funciones y encadenar métodos proporcionados por el evaluador:

```text
matches(name.trim().to_lowercase(), '^john')
```

Puedes acceder a campos anidados con la notación de punto:

```text
user.address.city = 'London'
```

Puedes vincular resultados intermedios con `let name := value then body`:

```text
let is_foo := a = "foo" then
let is_lucky := n in [1, 2, 3] then
is_foo || is_lucky
```

## Referencia de sintaxis

### Filtro

```
<filter> = <expr> | <empty>

<empty> =
```

### Expresión

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

### Comparación

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
