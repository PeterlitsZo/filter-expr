# filter-expr

クレート：

- [`filter-expr`](./filter-expr.md).
- [`filter-expr-evaler`](./filter-expr-evaler.md).

`filter-expr` クレートはフィルター式の解析を行い、`FilterExpr` を式の抽象構文木（AST）として利用できます。
評価方法は定義していません。独自に実装するか、`filter-expr-evaler` クレートを利用できます。

このプロジェクトは CEL と AIP-160 のフィルター構文に着想を得ていますが、構文は異なります。

## クイックスタート

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

`compiled` は異なるコンテキストや評価器で再利用・複製できます。
各評価器は独自の関数、メソッド、正規表現キャッシュを持ちます。

## 開発

[Lefthook](https://lefthook.dev/) をインストールし、開発を始める前に
リポジトリのルートディレクトリで次のコマンドを実行してください。

```sh
lefthook install
```

これにより pre-commit フックが有効になります。

## ベンチマーク

最適化ビルドで各ステージのベンチマークを実行します。

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```
