# Crate filter-expr

Une bibliothèque pour analyser les expressions de filtrage et produire un arbre syntaxique abstrait (AST).

```rust
use filter_expr::FilterExpr;

let f = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

assert_eq!(f.expr(), Some(Expr::and_([
    Expr::eq_(Expr::field_("name"), Expr::str_("John")),
    Expr::gt_(Expr::field_("age"), Expr::i64_(18)),
])));
```

Vous pouvez implémenter l’évaluation vous-même ou utiliser le crate `filter-expr-evaler`.

## Exemples

Vous pouvez comparer des valeurs avec `=`, `==`, `!=`, `>`, `>=`, `<` et `<=` :

```text
age >= 18 AND rating > 4.5
```

Vous pouvez utiliser les opérateurs arithmétiques `+ - * / %` et les opérateurs unaires `+` / `-` :

```text
price * quantity - discount >= 100
/* OR */
(count + 1) % 2 = 0
```

Vous pouvez combiner des conditions avec `AND` / `and` / `&&`, `OR` / `or` / `||`, `NOT` / `not` / `!` et des parenthèses :

```text
active AND (admin OR NOT banned)
/* OR */
active and (admin or not banned)
/* OR */
active && (admin || !banned)
```

Vous pouvez vérifier si une valeur appartient à un tableau avec `IN` / `in`, ou exclure des valeurs avec `NOT IN` / `not in` :

```text
status IN [200, 201, 204]
/* OR */
status in [200, 201, 204]
/* OR */
status not in [400, 404, 500]
```

Vous pouvez délimiter les chaînes avec des guillemets simples ou doubles et échapper les guillemets avec une barre oblique inverse :

```text
first_name = "John" AND last_name = 'O\'Brien'
```

Vous pouvez utiliser des littéraux booléens et `null` :

```text
active = true AND deleted_at = null
```

Vous pouvez appeler les fonctions et enchaîner les méthodes fournies par votre évaluateur :

```text
matches(name.trim().to_lowercase(), '^john')
```

Vous pouvez accéder aux champs imbriqués avec la notation pointée :

```text
user.address.city = 'London'
```

Vous pouvez lier des résultats intermédiaires avec `let name := value then body` :

```text
let is_foo := a = "foo" then
let is_lucky := n in [1, 2, 3] then
is_foo || is_lucky
```

## Référence de syntaxe

### Filtre

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

### Comparaison

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
