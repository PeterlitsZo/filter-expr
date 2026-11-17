# Crate filter-expr-evaler

Una biblioteca para evaluar expresiones de filtrado.

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

## Funciones integradas

- **`DateTime::now() -> datetime`**. Devuelve la instantánea temporal de esta evaluación.
- **`matches(text: str, pattern: str) -> bool`**. Comprueba si una expresión regular coincide en alguna parte del texto. Por ejemplo, `matches('foobar', '^foo')` devuelve `true`.
- **`type(value) -> str`**. Devuelve el nombre del tipo de un valor. Por ejemplo, `type(42)` devuelve `'i64'`.
- **`abs(x)`**. Valor absoluto.
- **`ceil(x)`**. Redondea hacia arriba al entero más cercano.
- **`floor(x)`**. Redondea hacia abajo al entero más cercano.
- **`round(x)`**. Redondea al entero más cercano; en caso de empate, se aleja de cero.
- **`pow(x, exponent)`**. Eleva `x` a la potencia indicada.
- **`sqrt(x)`**. Raíz cuadrada.
- **`exp(x)`**. Eleva el número de Euler e a la potencia `x`.
- **`ln(x)`**. Logaritmo natural.
- **`log(x, base)`**. Logaritmo en la base indicada.
- **`log2(x)`**. Logaritmo en base 2.
- **`log10(x)`**. Logaritmo en base 10.

## Métodos integrados

### str

- **`to_uppercase() -> str`**. Convierte a mayúsculas Unicode.
- **`to_lowercase() -> str`**. Convierte a minúsculas Unicode.
- **`len() -> i64`**. Devuelve la longitud en bytes UTF-8.
- **`is_empty() -> bool`**. Comprueba si la cadena está vacía.
- **`contains(needle: str) -> bool`**. Comprueba si contiene una subcadena.
- **`starts_with(prefix: str) -> bool`**. Comprueba si empieza con el prefijo indicado.
- **`ends_with(suffix: str) -> bool`**. Comprueba si termina con el sufijo indicado.
- **`trim() -> str`**. Elimina los espacios en blanco Unicode al principio y al final.
- **`trim_start() -> str`**. Elimina los espacios en blanco Unicode al principio.
- **`trim_end() -> str`**. Elimina los espacios en blanco Unicode al final.
- **`strip_prefix(prefix: str) -> str or null`**. Elimina una vez el prefijo coincidente.
- **`strip_suffix(suffix: str) -> str or null`**. Elimina una vez el sufijo coincidente.
