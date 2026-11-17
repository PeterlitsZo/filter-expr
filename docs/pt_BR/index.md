# filter-expr

Crates:

- [`filter-expr`](./filter-expr.md).
- [`filter-expr-evaler`](./filter-expr-evaler.md).

O crate `filter-expr` define como analisar uma expressão de filtro. Depois, você pode usar
`FilterExpr` como a árvore sintática abstrata (AST) da expressão. Ele não define como avaliá-la;
você pode implementar a avaliação ou usar o crate `filter-expr-evaler`.

Este projeto é inspirado em CEL e na sintaxe de filtro do AIP-160,
mas usa uma sintaxe diferente.

## Início rápido

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

Reutilize ou clone `compiled` com diferentes contextos e avaliadores. Cada avaliador fornece
suas próprias funções, métodos e cache de expressões regulares.

## Desenvolvimento

Instale o [Lefthook](https://lefthook.dev/) e, antes de começar o desenvolvimento,
execute o seguinte comando na raiz do repositório:

```sh
lefthook install
```

Isso habilita os hooks de pre-commit.

## Benchmarks

Execute os benchmarks por etapa com uma compilação otimizada:

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```
