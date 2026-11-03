# CHANGELOG

## Unreleased

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
