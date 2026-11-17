# Crate filter-expr-evaler

Une bibliothèque pour évaluer les expressions de filtrage.

```rust
use filter_expr::FilterExpr;
use filter_expr_evaler::{CompiledFilterExpr, FilterExprEvaler, simple_context};

// Parse the filter expression.
let filter_expr = FilterExpr::parse("name = 'John' AND age > 18").unwrap();

// Build the evaler.
let filter_expr_evaler = FilterExprEvaler::new();

// Compile once and reuse the handle for evaluation.
let compiled = CompiledFilterExpr::compile(&filter_expr);
let ctx = simple_context! {
    "name": "John",
    "age": 19,
};
let result = filter_expr_evaler.run(&compiled, &ctx).await.unwrap();
assert_eq!(result, true);
```

## Fonctions intégrées

- **`DateTime::now() -> datetime`**. Renvoie l’instantané temporel de cette évaluation.
- **`DateTime::from_rfc3339(text: str) -> datetime`**. Analyse une chaîne RFC 3339 avec `Z` ou un décalage UTC et renvoie une date et une heure ; les fractions de seconde sont prises en charge.
  Renvoie une erreur si l’analyse échoue. Utilise l’analyseur de Jiff et accepte également certains de ses formats étendus.
  Exemple: `DateTime::from_rfc3339('2026-11-17T12:30:00+08:00')`.
- **`matches(text: str, pattern: str) -> bool`**. Vérifie si une expression régulière correspond à une partie du texte. Par exemple, `matches('foobar', '^foo')` renvoie `true`.
- **`type(value) -> str`**. Renvoie le nom du type d’une valeur. Par exemple, `type(42)` renvoie `'i64'`.
- **`abs(x)`**. Valeur absolue.
- **`ceil(x)`**. Arrondit à l’entier supérieur le plus proche.
- **`floor(x)`**. Arrondit à l’entier inférieur le plus proche.
- **`round(x)`**. Arrondit à l’entier le plus proche ; à égale distance, arrondit en s’éloignant de zéro.
- **`pow(x, exponent)`**. Élève `x` à la puissance indiquée.
- **`sqrt(x)`**. Racine carrée.
- **`exp(x)`**. Élève le nombre d’Euler e à la puissance `x`.
- **`ln(x)`**. Logarithme naturel.
- **`log(x, base)`**. Logarithme dans la base indiquée.
- **`log2(x)`**. Logarithme en base 2.
- **`log10(x)`**. Logarithme en base 10.

## Méthodes intégrées

### str

- **`to_uppercase() -> str`**. Convertit en majuscules Unicode.
- **`to_lowercase() -> str`**. Convertit en minuscules Unicode.
- **`len() -> i64`**. Renvoie la longueur en octets UTF-8.
- **`is_empty() -> bool`**. Vérifie si la chaîne est vide.
- **`contains(needle: str) -> bool`**. Vérifie la présence d’une sous-chaîne.
- **`starts_with(prefix: str) -> bool`**. Vérifie si la chaîne commence par le préfixe indiqué.
- **`ends_with(suffix: str) -> bool`**. Vérifie si la chaîne se termine par le suffixe indiqué.
- **`trim() -> str`**. Supprime les espaces blancs Unicode au début et à la fin.
- **`trim_start() -> str`**. Supprime les espaces blancs Unicode au début.
- **`trim_end() -> str`**. Supprime les espaces blancs Unicode à la fin.
- **`strip_prefix(prefix: str) -> str or null`**. Supprime une seule fois le préfixe correspondant.
- **`strip_suffix(suffix: str) -> str or null`**. Supprime une seule fois le suffixe correspondant.
