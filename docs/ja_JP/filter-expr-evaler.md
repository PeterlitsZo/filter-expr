# Crate filter-expr-evaler

フィルター式を評価するライブラリです。

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

## 組み込み関数

- **`DateTime::now() -> datetime`**. 今回の評価における時刻のスナップショットを返します。
- **`matches(text: str, pattern: str) -> bool`**. 正規表現がテキスト内のいずれかの位置にマッチするか確認します。例えば、`matches('foobar', '^foo')` は `true` を返します。
- **`type(value) -> str`**. 値の型名を返します。例えば、`type(42)` は `'i64'` を返します。
- **`abs(x)`**. 絶対値。
- **`ceil(x)`**. 最も近い整数に切り上げます。
- **`floor(x)`**. 最も近い整数に切り捨てます。
- **`round(x)`**. 最も近い整数に丸めます。中間値はゼロから遠ざかる方向に丸めます。
- **`pow(x, exponent)`**. `x` を指定された指数で累乗します。
- **`sqrt(x)`**. 平方根。
- **`exp(x)`**. ネイピア数 e の `x` 乗を計算します。
- **`ln(x)`**. 自然対数。
- **`log(x, base)`**. 指定された底の対数。
- **`log2(x)`**. 底が 2 の対数。
- **`log10(x)`**. 底が 10 の対数。

## 組み込みメソッド

### str

- **`to_uppercase() -> str`**. Unicode の大文字に変換します。
- **`to_lowercase() -> str`**. Unicode の小文字に変換します。
- **`len() -> i64`**. UTF-8 のバイト数を返します。
- **`is_empty() -> bool`**. 文字列が空かどうかを確認します。
- **`contains(needle: str) -> bool`**. 部分文字列が含まれるか確認します。
- **`starts_with(prefix: str) -> bool`**. 指定された接頭辞で始まるか確認します。
- **`ends_with(suffix: str) -> bool`**. 指定された接尾辞で終わるか確認します。
- **`trim() -> str`**. 先頭と末尾の Unicode 空白文字を除去します。
- **`trim_start() -> str`**. 先頭の Unicode 空白文字を除去します。
- **`trim_end() -> str`**. 末尾の Unicode 空白文字を除去します。
- **`strip_prefix(prefix: str) -> str or null`**. 一致する接頭辞を一度だけ除去します。
- **`strip_suffix(suffix: str) -> str or null`**. 一致する接尾辞を一度だけ除去します。
