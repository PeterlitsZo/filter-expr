# filter-expr

Crate:

- [`filter-expr`](./filter-expr.md).
- [`filter-expr-evaler`](./filter-expr-evaler.md).

`filter-expr` crate는 필터 표현식을 파싱하며, 파싱한 `FilterExpr`를 표현식의 추상 구문 트리(AST)로 사용할 수 있습니다.
평가 방법은 정의하지 않습니다. 직접 구현하거나 `filter-expr-evaler` crate를 사용할 수 있습니다.

이 프로젝트는 CEL과 AIP-160의 필터 문법에서 영감을 받았지만, 문법은 다릅니다.

## 빠른 시작

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

여러 컨텍스트와 평가기에서 `compiled`를 재사용하거나 복제할 수 있습니다.
각 평가기는 자체 함수, 메서드, 정규식 캐시를 제공합니다.

## 개발

[Lefthook](https://lefthook.dev/)을 설치한 뒤, 개발을 시작하기 전에
저장소 루트에서 다음 명령을 실행하세요.

```sh
lefthook install
```

이 명령은 pre-commit 훅을 활성화합니다.

## 벤치마크

최적화된 빌드로 각 단계의 벤치마크를 실행하세요.

```sh
cargo bench -p filter-expr-evaler --features bench-internals --bench stages
```
