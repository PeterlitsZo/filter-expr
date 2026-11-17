# filter-expr

Crates:

- [`filter-expr`](./filter-expr.md).
- [`filter-expr-evaler`](./filter-expr-evaler.md).

El crate `filter-expr` define cómo analizar una expresión de filtrado. Después puedes usar
`FilterExpr` como su árbol de sintaxis abstracta (AST). No define cómo evaluarla;
puedes implementarlo por tu cuenta o usar el crate `filter-expr-evaler`.

Este proyecto se inspira en CEL y en la sintaxis de filtrado de AIP-160,
pero utiliza una sintaxis diferente.

## Inicio rápido

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

Reutiliza o clona `compiled` con distintos contextos y evaluadores. Cada evaluador proporciona
sus propias funciones, métodos y caché de expresiones regulares.

## Desarrollo

Instala [Lefthook](https://lefthook.dev/) y, antes de empezar a desarrollar,
ejecuta el siguiente comando desde la raíz del repositorio:

```sh
lefthook install
```

Esto habilita los hooks de pre-commit.

## Pruebas de rendimiento

Ejecuta las pruebas de rendimiento por etapas con una compilación optimizada:

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```
