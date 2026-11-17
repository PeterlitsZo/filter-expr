# Crate filter-expr-evaler

Uma biblioteca para avaliar expressões de filtro.

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

## Funções integradas

- **`DateTime::now() -> datetime`**. Retorna o instante de tempo capturado para esta avaliação.
- **`matches(text: str, pattern: str) -> bool`**. Verifica se uma expressão regular corresponde a alguma parte do texto. Por exemplo, `matches('foobar', '^foo')` retorna `true`.
- **`type(value) -> str`**. Retorna o nome do tipo de um valor. Por exemplo, `type(42)` retorna `'i64'`.
- **`abs(x)`**. Valor absoluto.
- **`ceil(x)`**. Arredonda para cima até o inteiro mais próximo.
- **`floor(x)`**. Arredonda para baixo até o inteiro mais próximo.
- **`round(x)`**. Arredonda para o inteiro mais próximo; em caso de empate, afasta-se de zero.
- **`pow(x, exponent)`**. Eleva `x` à potência indicada.
- **`sqrt(x)`**. Raiz quadrada.
- **`exp(x)`**. Eleva o número de Euler e à potência `x`.
- **`ln(x)`**. Logaritmo natural.
- **`log(x, base)`**. Logaritmo na base indicada.
- **`log2(x)`**. Logaritmo na base 2.
- **`log10(x)`**. Logaritmo na base 10.

## Métodos integrados

### str

- **`to_uppercase() -> str`**. Converte para maiúsculas Unicode.
- **`to_lowercase() -> str`**. Converte para minúsculas Unicode.
- **`len() -> i64`**. Retorna o comprimento em bytes UTF-8.
- **`is_empty() -> bool`**. Verifica se a string está vazia.
- **`contains(needle: str) -> bool`**. Verifica se contém uma substring.
- **`starts_with(prefix: str) -> bool`**. Verifica se começa com o prefixo indicado.
- **`ends_with(suffix: str) -> bool`**. Verifica se termina com o sufixo indicado.
- **`trim() -> str`**. Remove os caracteres de espaço em branco Unicode do início e do fim.
- **`trim_start() -> str`**. Remove os caracteres de espaço em branco Unicode do início.
- **`trim_end() -> str`**. Remove os caracteres de espaço em branco Unicode do fim.
- **`strip_prefix(prefix: str) -> str or null`**. Remove uma vez o prefixo correspondente.
- **`strip_suffix(suffix: str) -> str or null`**. Remove uma vez o sufixo correspondente.
