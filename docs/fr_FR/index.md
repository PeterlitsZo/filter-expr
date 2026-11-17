# filter-expr

Crates :

- [`filter-expr`](./filter-expr.md).
- [`filter-expr-evaler`](./filter-expr-evaler.md).

Le crate `filter-expr` définit comment analyser une expression de filtrage. Vous pouvez ensuite
utiliser `FilterExpr` comme arbre syntaxique abstrait (AST) de l’expression. Il ne définit pas
son évaluation ; vous pouvez l’implémenter vous-même ou utiliser le crate `filter-expr-evaler`.

Ce projet s’inspire de CEL et de la syntaxe de filtrage d’AIP-160,
mais utilise une syntaxe différente.

## Démarrage rapide

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

Réutilisez ou clonez `compiled` avec différents contextes et évaluateurs. Chaque évaluateur fournit
ses propres fonctions, méthodes et cache d’expressions régulières.

## Développement

Installez [Lefthook](https://lefthook.dev/), puis exécutez la commande suivante
à la racine du dépôt avant de commencer le développement :

```sh
lefthook install
```

Cela active les hooks de pre-commit.

## Tests de performance

Exécutez les tests de performance par étape avec une compilation optimisée :

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```
