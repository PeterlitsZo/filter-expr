# Crate filter-expr-evaler

필터 표현식을 평가하는 라이브러리입니다.

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

## 내장 함수

- **`DateTime::now() -> datetime`**. 이번 평가의 시간 스냅샷을 반환합니다.
- **`DateTime::from_rfc3339(text: str) -> datetime`**. `Z` 또는 UTC 오프셋이 있는 RFC 3339 문자열을 날짜와 시간으로 파싱하며, 소수 초를 지원합니다.
  파싱에 실패하면 오류를 반환합니다. Jiff 파서를 사용하므로 일부 확장 형식도 허용합니다.
  예: `DateTime::from_rfc3339('2026-11-17T12:30:00+08:00')`.
- **`matches(text: str, pattern: str) -> bool`**. 정규식이 텍스트의 어느 위치에서든 일치하는지 확인합니다. 예를 들어 `matches('foobar', '^foo')`는 `true`를 반환합니다.
- **`type(value) -> str`**. 값의 타입 이름을 반환합니다. 예를 들어 `type(42)`는 `'i64'`를 반환합니다.
- **`abs(x)`**. 절댓값.
- **`ceil(x)`**. 가장 가까운 정수로 올림합니다.
- **`floor(x)`**. 가장 가까운 정수로 내림합니다.
- **`round(x)`**. 가장 가까운 정수로 반올림하며, 정확히 중간인 값은 0에서 멀어지는 방향으로 반올림합니다.
- **`pow(x, exponent)`**. `x`를 지정된 지수만큼 거듭제곱합니다.
- **`sqrt(x)`**. 제곱근.
- **`exp(x)`**. 자연상수 e의 `x`제곱을 계산합니다.
- **`ln(x)`**. 자연로그.
- **`log(x, base)`**. 지정된 밑의 로그.
- **`log2(x)`**. 밑이 2인 로그.
- **`log10(x)`**. 밑이 10인 로그.

## 내장 메서드

### str

- **`to_uppercase() -> str`**. Unicode 대문자로 변환합니다.
- **`to_lowercase() -> str`**. Unicode 소문자로 변환합니다.
- **`len() -> i64`**. UTF-8 바이트 길이를 반환합니다.
- **`is_empty() -> bool`**. 문자열이 비어 있는지 확인합니다.
- **`contains(needle: str) -> bool`**. 부분 문자열이 포함되어 있는지 확인합니다.
- **`starts_with(prefix: str) -> bool`**. 지정된 접두사로 시작하는지 확인합니다.
- **`ends_with(suffix: str) -> bool`**. 지정된 접미사로 끝나는지 확인합니다.
- **`trim() -> str`**. 앞뒤의 Unicode 공백 문자를 제거합니다.
- **`trim_start() -> str`**. 앞쪽의 Unicode 공백 문자를 제거합니다.
- **`trim_end() -> str`**. 뒤쪽의 Unicode 공백 문자를 제거합니다.
- **`strip_prefix(prefix: str) -> str or null`**. 일치하는 접두사를 한 번 제거합니다.
- **`strip_suffix(suffix: str) -> str or null`**. 일치하는 접미사를 한 번 제거합니다.
