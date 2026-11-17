# filter-expr

Crates:

- [`filter-expr`](./filter-expr.md).
- [`filter-expr-evaler`](./filter-expr-evaler.md).

Das Crate `filter-expr` definiert, wie Filterausdrücke geparst werden. Anschließend kannst du
`FilterExpr` als abstrakten Syntaxbaum (AST) des Ausdrucks verwenden. Die Auswertung ist nicht
festgelegt; du kannst sie selbst implementieren oder das Crate `filter-expr-evaler` verwenden.

Dieses Projekt ist von CEL und der Filtersyntax in AIP-160 inspiriert,
verwendet aber eine andere Syntax.

## Schnellstart

```rust
use filter_expr::FilterExpr;
use filter_expr_evaler::{CompiledFilterExpr, FilterExprEvaler, simple_context};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Parse the filter expression.
    let filter = FilterExpr::parse("name = 'John' AND age > 18")?;

    // Provide values for the variables in the expression.
    let ctx = simple_context! {
        "name": "John",
        "age": 19,
    };

    // Compile once, then reuse the handle across contexts.
    let evaler = FilterExprEvaler::new();
    let compiled = CompiledFilterExpr::compile(&filter);
    assert!(evaler.run(&compiled, &ctx).await?);

    Ok(())
}
```

Verwende oder klone `compiled` für verschiedene Kontexte und Evaluatoren. Jeder Evaluator stellt
eigene Funktionen, Methoden und einen eigenen Regex-Cache bereit.

## Entwicklung

Installiere [Lefthook](https://lefthook.dev/) und führe vor Beginn der Entwicklung
den folgenden Befehl im Stammverzeichnis des Repositorys aus:

```sh
lefthook install
```

Dadurch werden die Pre-Commit-Hooks aktiviert.

## Benchmarks

Führe die Benchmarks der einzelnen Phasen mit einem optimierten Build aus:

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```
