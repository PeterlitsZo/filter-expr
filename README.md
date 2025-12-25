# filter-expr

> NOTE: This library is still under development.

Crates:

- The core crate's README: [filter-expr](crates/filter-expr/README.md)
- The evaler crate's README: [filter-expr-evaler](crates/filter-expr-evaler/README.md)

The `filter-expr` crate defines how to parse the filter expression, and then you
can use the `FilterExpr` as the AST of the filter expression. It do not define
how to evaluate it -- you can do it by yourself or use the `filter-expr-evaler`
crate.

This project is inspired by CEL and the filter syntax in AIP-160 -- but has
different syntax.
