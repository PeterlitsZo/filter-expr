# Crate filter-expr-evaler

用于求值过滤表达式的库。

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

## 内置函数

- **`DateTime::now() -> datetime`**. 返回本次求值的时间快照。
- **`DateTime::from_rfc3339(text: str) -> datetime`**. 将带 `Z` 或 UTC 偏移的 RFC 3339 字符串解析为日期时间，支持小数秒。
  解析失败时返回错误。使用 Jiff 解析器，也接受其支持的部分扩展格式。
  例如: `DateTime::from_rfc3339('2026-11-17T12:30:00+08:00')`.
- **`matches(text: str, pattern: str) -> bool`**. 检查正则表达式是否匹配文本中的任意位置。例如，`matches('foobar', '^foo')` 返回 `true`。
- **`type(value) -> str`**. 返回值的类型名称。例如，`type(42)` 返回 `'i64'`。
- **`abs(x)`**. 绝对值。
- **`ceil(x)`**. 向上取整到最近的整数。
- **`floor(x)`**. 向下取整到最近的整数。
- **`round(x)`**. 舍入到最近的整数，恰好居中时向远离零的方向舍入。
- **`pow(x, exponent)`**. 计算 `x` 的指定次幂。
- **`sqrt(x)`**. 平方根。
- **`exp(x)`**. 计算自然常数 e 的 `x` 次幂。
- **`ln(x)`**. 自然对数。
- **`log(x, base)`**. 以指定底数计算对数。
- **`log2(x)`**. 以 2 为底的对数。
- **`log10(x)`**. 以 10 为底的对数。

## 内置方法

### str

- **`to_uppercase() -> str`**. 转换为 Unicode 大写。
- **`to_lowercase() -> str`**. 转换为 Unicode 小写。
- **`len() -> i64`**. 返回 UTF-8 编码的字节长度。
- **`is_empty() -> bool`**. 检查字符串是否为空。
- **`contains(needle: str) -> bool`**. 检查是否包含子串。
- **`starts_with(prefix: str) -> bool`**. 检查是否以指定前缀开头。
- **`ends_with(suffix: str) -> bool`**. 检查是否以指定后缀结尾。
- **`trim() -> str`**. 移除开头和结尾的 Unicode 空白字符。
- **`trim_start() -> str`**. 移除开头的 Unicode 空白字符。
- **`trim_end() -> str`**. 移除结尾的 Unicode 空白字符。
- **`strip_prefix(prefix: str) -> str or null`**. 移除一次匹配的前缀。
- **`strip_suffix(suffix: str) -> str or null`**. 移除一次匹配的后缀。
