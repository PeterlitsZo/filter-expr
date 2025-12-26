# 2026-01

- [x] Support the `Evaler` struct to evaluate the expression and move the
      evaluation logic to a separate crate (it should be useful if some project
      want to evaluate the expression by itself).
- [x] Let the `tokio` dependency be optional.
- [ ] More documentation.
- [ ] Add fuzzing tests and more tests.
- [ ] Rename the `field` to `var` in source code.
- [ ] Support custom method.
- [ ] Support those methods for string:

  - [x] `to_uppercase`
  - [x] `to_lowercase`
  - [x] `contains`
  - [x] `starts_with`
  - [x] `ends_with`
  - [ ] `len`
  - [ ] `is_empty`
  - [ ] `trim`
  - [ ] `trim_start`
  - [ ] `trim_end`
  - [ ] `strip_prefix`
  - [ ] `strip_suffix`

- [ ] Support the `DateTime` type.
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

- [x] Support the `type` function to get the type of the value. Like
      `type(foobar) = 'i64'`.
- [ ] Support the `Object` type -- just like the JSON object.
- [ ] Support get the item of the `Array` and `Object` by index or key. Like
      `array[0] = "John"` or `object.name = "John"`.
- [ ] Support `==` as the alias of `=` for the equality comparison.
- [ ] Support escape the special characters in the string. Like `\'` to escape
      the single quote.
- [ ] Support `+`, `-`, `*`, `/` operators for the arithmetic operations.
- [ ] Support `&&`, `||` and `!` operators for the logical operations.
      Support `and`, `or` and `not` as the aliases of `&&`, `||` and `!`.
- [ ] Use tool `prek` for pre-commit hooks.
- [ ] Emit the bytecode for the expression evaluation. It should speed up the
      function `matches` - we can compile the regex pattern before the
      evaluation.
- [ ] Add benchmark tests and ensure the performance is not too bad.
- [ ] Support the math functions:

  - [ ] `abs`
  - [ ] `ceil`
  - [ ] `floor`
  - [ ] `round`
  - [ ] `pow`
  - [ ] `sqrt`
  - [ ] `exp`
  - [ ] `ln`
  - [ ] `log`
  - [ ] `log2`
  - [ ] `log10`

- [ ] Support the regex! Like `regex("foobar")` to create a regex value.
      (I think maybe we can use the syntax `/foobar/` or `r"foobar"` to create
      the regex value -- but maybe not a good idea.)
