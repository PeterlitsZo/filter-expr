# filter-expr

Crates：

- `filter-expr`: [crates.io](https://crates.io/crates/filter-expr), [README](./filter-expr.md).
- `filter-expr-evaler`: [crates.io](https://crates.io/crates/filter-expr-evaler), [README](./filter-expr-evaler.md).

`filter-expr` crate 負責解析篩選運算式，解析後可將 `FilterExpr` 用作運算式的抽象語法樹（AST）。
它不定義如何求值；你可以自行實作，也可以使用 `filter-expr-evaler` crate。

本專案受 CEL 和 AIP-160 中篩選語法的啟發，但使用了不同的語法。

## 快速開始

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

可在不同上下文與求值器之間重複使用或複製 `compiled`。
每個求值器提供自己的函式、方法與正規表示式快取。

## 開發

安裝 [Lefthook](https://lefthook.dev/)，然後在開始開發前，
於儲存庫根目錄執行以下指令：

```sh
lefthook install
```

這會啟用 pre-commit 掛鉤。

## 效能基準測試

使用最佳化建置執行各階段的效能基準測試：

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```
