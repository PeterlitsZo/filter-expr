# CHANGELOG

## Unreleased

- Support leading `/*! v1 */` language version comments.
- Treat comment-only input as an empty expression.

## v0.6.1

- Support string concatenation with the `+` operator.

## v0.6.0

- Support `DateTime::from_unix_ts()` and `datetime.to_unix_ts()` for UNIX
  seconds.
- [BREAKING] Use an owned index arena for AST nodes and update AST APIs.
- [BREAKING] Use `CompactString` for AST strings and object keys to reduce
  short-string allocations; re-export it as `filter_expr::CompactString`.

## v0.5.0

- [BREAKING] Support `let name := value then body`. Now `let` and `then` are
  reserved keywords.
- Fix AST transformations skipping array elements.
- Require pure evaluation. Optimizations may skip calls and their errors.
- Support field access on custom userdata.
- [BREAKING] Remove the AST runner.
- [BREAKING] Support DateTime and `DateTime::now()`.
- Support `&&` / `||` and lowercase `and`, `or`, `not`, and `in` aliases,
  including `not in`; lowercase operator words are now reserved keywords.
- [BREAKING] Replace evaluator `eval` and `eval_by_bytecode_runner` with
  `CompiledFilterExpr::compile` and evaluator `run`.

## v0.4.1

- Support Wasm caching with LRU.
- Reject trailing tokens and unterminated strings.

## v0.4.0

- Support arithmetic operators `+`, `-`, `*`, `/`, `%`, and unary `+` / `-`.
- Support `==` as an alias for `=` in equality comparisons.
- Support object literals, construction, evaluation, equality, and field access.

## v0.3.0

- Support `NOT IN`.
- Support block comments.
- Support built-in math functions.
- Add string length, emptiness, trimming, and prefix/suffix removal methods.

## v0.2.0

- Support escape sequences in string literals.
- Add custom userdata with fallible comparisons; allow integer/float equality.
- [BREAKING] Replace `Value` comparison traits with fallible methods and
  redesign errors around `ErrorKind`, metadata, and sources.

## v0.1.x

- Parse and asynchronously evaluate logical expressions, comparisons, `IN`,
  literals, arrays, and field access.
- Support custom contexts, functions, and methods, with cached bytecode
  evaluation.
- Provide regex matching, type inspection, string case conversion, and
  substring/prefix/suffix checks.
