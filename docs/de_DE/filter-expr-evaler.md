# Crate filter-expr-evaler

Eine Bibliothek zur Auswertung von Filterausdrücken.

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

## Integrierte Funktionen

- **`DateTime::now() -> datetime`**. Gibt den Zeit-Snapshot dieser Auswertung zurück.
- **`matches(text: str, pattern: str) -> bool`**. Prüft, ob ein regulärer Ausdruck an irgendeiner Stelle im Text übereinstimmt. Zum Beispiel gibt `matches('foobar', '^foo')` den Wert `true` zurück.
- **`type(value) -> str`**. Gibt den Typnamen eines Werts zurück. Zum Beispiel gibt `type(42)` den Wert `'i64'` zurück.
- **`abs(x)`**. Absolutwert.
- **`ceil(x)`**. Rundet auf die nächste ganze Zahl auf.
- **`floor(x)`**. Rundet auf die nächste ganze Zahl ab.
- **`round(x)`**. Rundet auf die nächste ganze Zahl; bei gleichem Abstand wird von null weg gerundet.
- **`pow(x, exponent)`**. Potenziert `x` mit dem angegebenen Exponenten.
- **`sqrt(x)`**. Quadratwurzel.
- **`exp(x)`**. Berechnet die Eulersche Zahl e hoch `x`.
- **`ln(x)`**. Natürlicher Logarithmus.
- **`log(x, base)`**. Logarithmus zur angegebenen Basis.
- **`log2(x)`**. Logarithmus zur Basis 2.
- **`log10(x)`**. Logarithmus zur Basis 10.

## Integrierte Methoden

### str

- **`to_uppercase() -> str`**. Wandelt in Unicode-Großbuchstaben um.
- **`to_lowercase() -> str`**. Wandelt in Unicode-Kleinbuchstaben um.
- **`len() -> i64`**. Gibt die Länge in UTF-8-Bytes zurück.
- **`is_empty() -> bool`**. Prüft, ob die Zeichenkette leer ist.
- **`contains(needle: str) -> bool`**. Prüft, ob eine Teilzeichenkette enthalten ist.
- **`starts_with(prefix: str) -> bool`**. Prüft, ob die Zeichenkette mit dem angegebenen Präfix beginnt.
- **`ends_with(suffix: str) -> bool`**. Prüft, ob die Zeichenkette mit dem angegebenen Suffix endet.
- **`trim() -> str`**. Entfernt führende und abschließende Unicode-Leerraumzeichen.
- **`trim_start() -> str`**. Entfernt führende Unicode-Leerraumzeichen.
- **`trim_end() -> str`**. Entfernt abschließende Unicode-Leerraumzeichen.
- **`strip_prefix(prefix: str) -> str or null`**. Entfernt das passende Präfix einmal.
- **`strip_suffix(suffix: str) -> str or null`**. Entfernt das passende Suffix einmal.
