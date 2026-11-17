# filter-expr

Crate：

- `filter-expr`.
- `filter-expr-evaler`.

`filter-expr` crate 负责解析过滤表达式，解析后可将 `FilterExpr` 用作表达式的抽象语法树（AST）。
它不定义如何求值；你可以自行实现，也可以使用 `filter-expr-evaler` crate。

本项目受 CEL 和 AIP-160 中过滤语法的启发，但使用了不同的语法。

## 快速开始

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

可在不同上下文和求值器之间复用或克隆 `compiled`。
每个求值器提供自己的函数、方法和正则表达式缓存。

## 开发

安装 [Lefthook](https://lefthook.dev/)，然后在开始开发前，
从仓库根目录运行以下命令：

```sh
lefthook install
```

这会启用 pre-commit 钩子。

## 基准测试

使用优化构建运行各阶段的基准测试：

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```
