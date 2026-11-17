# Crate filter-expr

Eine Bibliothek zum Parsen von Filterausdrücken in einen abstrakten Syntaxbaum (AST).

```rust
use filter_expr::FilterExpr;

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

Du kannst die Auswertung selbst implementieren oder das Crate `filter-expr-evaler` verwenden.

## Beispiele

Du kannst Werte mit `=`, `==`, `!=`, `>`, `>=`, `<` und `<=` vergleichen:

```text
age >= 18 AND rating > 4.5
```

Du kannst die arithmetischen Operatoren `+ - * / %` sowie die unären Operatoren `+` / `-` verwenden:

```text
price * quantity - discount >= 100
/* OR */
(count + 1) % 2 = 0
```

Du kannst Bedingungen mit `AND` / `and` / `&&`, `OR` / `or` / `||`, `NOT` / `not` / `!` und Klammern kombinieren:

```text
active AND (admin OR NOT banned)
/* OR */
active and (admin or not banned)
/* OR */
active && (admin || !banned)
```

Du kannst mit `IN` / `in` prüfen, ob ein Wert in einem Array enthalten ist, oder mit `NOT IN` / `not in` Werte ausschließen:

```text
status IN [200, 201, 204]
/* OR */
status in [200, 201, 204]
/* OR */
status not in [400, 404, 500]
```

Du kannst Zeichenketten in einfache oder doppelte Anführungszeichen setzen und Anführungszeichen mit einem Backslash maskieren:

```text
first_name = "John" AND last_name = 'O\'Brien'
```

Du kannst boolesche Literale und `null` verwenden:

```text
active = true AND deleted_at = null
```

Du kannst Funktionen aufrufen und Methodenaufrufe verketten, die dein Evaluator bereitstellt:

```text
matches(name.trim().to_lowercase(), '^john')
```

Du kannst mit der Punktnotation auf verschachtelte Felder zugreifen:

```text
user.address.city = 'London'
```

Du kannst Zwischenergebnisse mit `let name := value then body` binden:

```text
let is_foo := a = "foo" then
let is_lucky := n in [1, 2, 3] then
is_foo || is_lucky
```

## Syntaxreferenz

### Filter

```
<filter> = <expr> | <empty>

<empty> =
```

### Ausdruck

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

### Vergleich

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
