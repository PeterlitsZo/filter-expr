# ROADMAP

## 2026-12

- [ ] Support `obj[name]`.
- [x] Fix `transform_children` skipping recursive transformation of `Array`
      elements.
- [ ] Support `some_userdata.foo.bar`.
- [x] Support the `DateTime` type and `DateTime::now` function.
- [ ] Support the `DateTime::from_rfc3339` function to create a `DateTime` value
      by given RFC 3339 string. Like
      `DateTime::from_rfc3339("2025-01-01T00:00:00Z")`.
- [ ] Support compare two `DateTime` values.
- [ ] Support those methods for `DateTime`:

  - [ ] `to_rfc3339`
  - [ ] `get_day_of_month`
  - [ ] `get_day_of_week`
  - [ ] `get_day_of_year`
  - [ ] `get_year`
  - [ ] `get_month`
  - [ ] `get_date`
  - [ ] `get_hour`
  - [ ] `get_minute`
  - [ ] `get_second`
  - [ ] `get_millisecond`
  - [ ] `get_microsecond`
  - [ ] `get_nanosecond`

- [ ] Handle missing EOF checks during tokenization, such as unterminated
      string literals.
- [x] Remove the AST runner.
- [x] Support `let now := DateTime::now() then foo.bar < now`.
- [ ] Remove the `impl Ord`, `impl PartialOrd` and `impl Hash` for `Expr`
      because it's useless now.
- [ ] Add comment for all `pub` functions and methods.

## 2026-11

- [ ] Support version comment.
- [x] Add benchmark tests and ensure the performance is not too bad.
- [x] Support the math functions:

  - [x] `abs`
  - [x] `ceil`
  - [x] `floor`
  - [x] `round`
  - [x] `pow`
  - [x] `sqrt`
  - [x] `exp`
  - [x] `ln`
  - [x] `log`
  - [x] `log2`
  - [x] `log10`

- [x] Support the field-access syntax. Like `foo.bar.baz`.
- [x] Support the `Object` type -- just like the JSON object.
- [ ] Support get the item of the `Array` and `Object` by index or key. Like
      `array[0] = "John"` or `object.name = "John"`.
- [x] Support `==` as the alias of `=` for the equality comparison.
- [x] Support escape the special characters in the string. Like `\'` to escape
      the single quote.
- [x] Support `+`, `-`, `*`, `/` and `%` operators for the arithmetic
      operations.
- [x] Support `&&`, `||` and `!` operators for the logical operations.
      Support `and`, `or` and `not` as the aliases of `&&`, `||` and `!`.
- [ ] Use tool `prek` for pre-commit hooks.
- [ ] Add fuzzing tests and more tests.
- [ ] Support custom method.

## 2026-10

- [x] Enhance the README document.
- [x] Emit the bytecode for the expression evaluation. It should speed up the
      function `matches` - we can compile the regex pattern before the
      evaluation.
- [x] Support comment.

## 2026-01

- [x] Support the `Evaler` struct to evaluate the expression and move the
      evaluation logic to a separate crate (it should be useful if some project
      want to evaluate the expression by itself).
- [x] Let the `tokio` dependency be optional.
- [x] Rename the `field` to `var` in source code.
- [x] Support those methods for string:

  - [x] `to_uppercase`
  - [x] `to_lowercase`
  - [x] `contains`
  - [x] `starts_with`
  - [x] `ends_with`
  - [x] `len`
  - [x] `is_empty`
  - [x] `trim`
  - [x] `trim_start`
  - [x] `trim_end`
  - [x] `strip_prefix`
  - [x] `strip_suffix`

- [x] Support the `type` function to get the type of the value. Like
      `type(foobar) = 'i64'`.
