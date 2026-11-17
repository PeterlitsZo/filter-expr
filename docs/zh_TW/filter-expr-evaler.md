# Crate filter-expr-evaler

用於求值篩選運算式的函式庫。

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

## 內建函式

- **`DateTime::now() -> datetime`**. 回傳本次求值的時間快照。
- **`matches(text: str, pattern: str) -> bool`**. 檢查正規表示式是否符合文字中的任意位置。例如，`matches('foobar', '^foo')` 回傳 `true`。
- **`type(value) -> str`**. 回傳值的型別名稱。例如，`type(42)` 回傳 `'i64'`。
- **`abs(x)`**. 絕對值。
- **`ceil(x)`**. 向上取整至最近的整數。
- **`floor(x)`**. 向下取整至最近的整數。
- **`round(x)`**. 捨入至最近的整數，恰好居中時朝遠離零的方向捨入。
- **`pow(x, exponent)`**. 計算 `x` 的指定次方。
- **`sqrt(x)`**. 平方根。
- **`exp(x)`**. 計算自然常數 e 的 `x` 次方。
- **`ln(x)`**. 自然對數。
- **`log(x, base)`**. 以指定底數計算對數。
- **`log2(x)`**. 以 2 為底的對數。
- **`log10(x)`**. 以 10 為底的對數。

## 內建方法

### str

- **`to_uppercase() -> str`**. 轉換為 Unicode 大寫。
- **`to_lowercase() -> str`**. 轉換為 Unicode 小寫。
- **`len() -> i64`**. 回傳 UTF-8 編碼的位元組長度。
- **`is_empty() -> bool`**. 檢查字串是否為空。
- **`contains(needle: str) -> bool`**. 檢查是否包含子字串。
- **`starts_with(prefix: str) -> bool`**. 檢查是否以指定前綴開頭。
- **`ends_with(suffix: str) -> bool`**. 檢查是否以指定後綴結尾。
- **`trim() -> str`**. 移除開頭與結尾的 Unicode 空白字元。
- **`trim_start() -> str`**. 移除開頭的 Unicode 空白字元。
- **`trim_end() -> str`**. 移除結尾的 Unicode 空白字元。
- **`strip_prefix(prefix: str) -> str or null`**. 移除一次符合的前綴。
- **`strip_suffix(suffix: str) -> str or null`**. 移除一次符合的後綴。
