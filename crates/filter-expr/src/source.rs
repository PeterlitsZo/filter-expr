use std::collections::HashSet;
use std::fmt::{Display, Write};

use num_traits::ToPrimitive;

use crate::token::{Lexer, Token};
use crate::{CompactString, Expr, ExprArena, ExprId};

// Leave room for the parser's recursive frames when reading grouped source back.
const MAX_DEPTH: usize = 256;

#[derive(Debug, thiserror::Error)]
pub enum SourceError {
    #[error("invalid source identifier: {0}")]
    InvalidIdentifier(CompactString),
    #[error("invalid expression reference: {0:?}")]
    InvalidReference(ExprId),
    #[error("cyclic expression reference: {0:?}")]
    CyclicReference(ExprId),
    #[error("expression exceeds the source export depth limit of {MAX_DEPTH}")]
    DepthLimit,
    #[error("non-finite floats cannot be represented in source")]
    NonFiniteFloat,
    #[error("a BigInt within the i64 range cannot retain its type in source")]
    SmallBigInt,
    #[error("empty {0} cannot be represented in source")]
    EmptyLogical(&'static str),
}

pub(crate) fn to_source(expr: Option<&Expr>, arena: &ExprArena) -> Result<String, SourceError> {
    let mut writer = SourceWriter {
        arena,
        output: String::from("/*! v1 */"),
        path: HashSet::new()
    };
    if let Some(expr) = expr {
        writer.output.push(' ');
        writer.expr(expr, 0, 0)?;
    }
    Ok(writer.output)
}

struct SourceWriter<'a> {
    arena: &'a ExprArena,
    output: String,
    path: HashSet<ExprId>,
}

impl SourceWriter<'_> {
    fn node(&mut self, id: ExprId, depth: usize, minimum: u8) -> Result<(), SourceError> {
        let expr = self.arena.get(id).ok_or(SourceError::InvalidReference(id))?;
        if !self.path.insert(id) {
            return Err(SourceError::CyclicReference(id));
        }
        let result = self.expr(expr, depth, minimum);
        self.path.remove(&id);
        result
    }

    fn name(&mut self, name: &str, namespaced: bool) -> Result<(), SourceError> {
        let valid_segment = |segment: &str| {
            if segment.is_empty()
                || segment.starts_with(|c: char| c.is_ascii_digit())
                || !segment.chars().all(|c| c.is_alphanumeric() || c == '_')
            {
                return false;
            }
            let mut lexer = Lexer::new(segment);
            matches!(lexer.next_token(), Ok(Some(Token::Ident(value))) if value == segment)
                && matches!(lexer.next_token(), Ok(None))
        };
        let valid = if namespaced {
            name.split("::").all(valid_segment)
        } else {
            valid_segment(name)
        };
        if !valid {
            return Err(SourceError::InvalidIdentifier(name.into()));
        }
        self.output.push_str(name);
        Ok(())
    }

    fn string(&mut self, value: &str) {
        self.output.push('\'');
        for c in value.chars() {
            match c {
                '\\' => self.output.push_str("\\\\"),
                '\'' => self.output.push_str("\\'"),
                '\n' => self.output.push_str("\\n"),
                '\r' => self.output.push_str("\\r"),
                '\t' => self.output.push_str("\\t"),
                _ => self.output.push(c),
            }
        }
        self.output.push('\'');
    }

    fn number(&mut self, value: impl Display, float: bool) {
        let start = self.output.len();
        // Rust's float Display writes decimal notation, including subnormal values.
        write!(self.output, "{value}").unwrap();
        if float && !self.output[start..].contains('.') {
            self.output.push_str(".0");
        }
    }

    fn list(&mut self, items: &[Expr], depth: usize) -> Result<(), SourceError> {
        for (index, item) in items.iter().enumerate() {
            if index != 0 {
                self.output.push_str(", ");
            }
            self.expr(item, depth, 0)?;
        }
        Ok(())
    }

    fn logical(
        &mut self,
        expr: &Expr,
        and: bool,
        depth: usize,
        first: &mut bool,
    ) -> Result<(), SourceError> {
        if depth >= MAX_DEPTH {
            return Err(SourceError::DepthLimit);
        }
        if let Expr::And(items) | Expr::Or(items) = expr {
            if items.len() == 1 {
                return self.logical(&items[0], and, depth + 1, first);
            }
        }
        let items = match expr {
            Expr::And(items) if and => Some(items),
            Expr::Or(items) if !and => Some(items),
            _ => None,
        };
        if let Some(items) = items {
            if items.is_empty() {
                return Err(SourceError::EmptyLogical(if and { "AND" } else { "OR" }));
            }
            for item in items {
                self.logical(item, and, depth + 1, first)?;
            }
        } else {
            if !*first {
                self.output.push_str(if and { " AND " } else { " OR " });
            }
            *first = false;
            self.expr(expr, depth, if and { 2 } else { 1 })?;
        }
        Ok(())
    }

    fn expr(&mut self, expr: &Expr, depth: usize, minimum: u8) -> Result<(), SourceError> {
        if depth >= MAX_DEPTH {
            return Err(SourceError::DepthLimit);
        }

        // Negative literals gain a Neg node when parsed back from source.
        let negative_literal = match expr {
            Expr::I64(value) => *value < 0,
            Expr::BigInt(value) => value.sign() == num_bigint::Sign::Minus,
            Expr::F64(value) => value.is_sign_negative(),
            _ => false,
        };
        if negative_literal && depth + 1 >= MAX_DEPTH {
            return Err(SourceError::DepthLimit);
        }

        if let Expr::And(items) | Expr::Or(items) = expr {
            if items.len() == 1 {
                return self.expr(&items[0], depth + 1, minimum);
            }
        }

        // These levels mirror the parser, including right-associative comparisons.
        let precedence = match expr {
            Expr::Let(..) => 0,
            Expr::Or(_) => 1,
            Expr::And(_) => 2,
            Expr::Not(_) => 3,
            Expr::Gt(..) | Expr::Lt(..) | Expr::Ge(..) | Expr::Le(..)
            | Expr::Eq(..) | Expr::Ne(..) | Expr::In(..) => 4,
            Expr::Add(..) | Expr::Sub(..) => 5,
            Expr::Mul(..) | Expr::Div(..) | Expr::Rem(..) => 6,
            Expr::Pos(_) | Expr::Neg(_) => 7,
            _ if negative_literal => 7,
            Expr::FieldAccess(..) | Expr::GetItem(..) | Expr::MethodCall(..) => 8,
            _ => 9,
        };
        // A dot following a numeric receiver would be consumed by the number lexer.
        let numeric_receiver = minimum == 8
            && matches!(expr, Expr::I64(_) | Expr::BigInt(_) | Expr::F64(_));
        let grouped = precedence < minimum || numeric_receiver;
        if grouped {
            self.output.push('(');
        }
        match expr {
            Expr::Field(name) => self.name(name, false)?,
            Expr::FieldAccess(obj, field) => {
                self.node(*obj, depth + 1, 8)?;
                self.output.push('.');
                self.name(field, false)?;
            }
            Expr::GetItem(obj, index) => {
                self.node(*obj, depth + 1, 8)?;
                self.output.push('[');
                self.node(*index, depth + 1, 0)?;
                self.output.push(']');
            }
            Expr::Str(value) => self.string(value),
            Expr::I64(value) => self.number(value, false),
            Expr::BigInt(value) => {
                if value.to_i64().is_some() {
                    return Err(SourceError::SmallBigInt);
                }
                self.number(value, false);
            }
            Expr::F64(value) => {
                if !value.is_finite() {
                    return Err(SourceError::NonFiniteFloat);
                }
                self.number(value, true);
            }
            Expr::Bool(value) => self.output.push_str(if *value { "true" } else { "false" }),
            Expr::Null => self.output.push_str("null"),
            Expr::Array(items) => {
                self.output.push('[');
                self.list(items, depth + 1)?;
                self.output.push(']');
            }
            Expr::Object(fields) => {
                self.output.push('{');
                for (index, (key, value)) in fields.iter().enumerate() {
                    if index != 0 {
                        self.output.push_str(", ");
                    }
                    self.string(key);
                    self.output.push_str(": ");
                    self.expr(value, depth + 1, 0)?;
                }
                self.output.push('}');
            }
            Expr::FuncCall(name, args) => {
                self.name(name, true)?;
                self.output.push('(');
                self.list(args, depth + 1)?;
                self.output.push(')');
            }
            Expr::MethodCall(name, obj, args) => {
                self.node(*obj, depth + 1, 8)?;
                self.output.push('.');
                self.name(name, false)?;
                self.output.push('(');
                self.list(args, depth + 1)?;
                self.output.push(')');
            }
            Expr::Add(a, b) | Expr::Sub(a, b) | Expr::Mul(a, b) | Expr::Div(a, b)
            | Expr::Rem(a, b) | Expr::Gt(a, b) | Expr::Lt(a, b) | Expr::Ge(a, b)
            | Expr::Le(a, b) | Expr::Eq(a, b) | Expr::Ne(a, b) | Expr::In(a, b) => {
                let op = match expr {
                    Expr::Add(..) => " + ",
                    Expr::Sub(..) => " - ",
                    Expr::Mul(..) => " * ",
                    Expr::Div(..) => " / ",
                    Expr::Rem(..) => " % ",
                    Expr::Gt(..) => " > ",
                    Expr::Lt(..) => " < ",
                    Expr::Ge(..) => " >= ",
                    Expr::Le(..) => " <= ",
                    Expr::Eq(..) => " = ",
                    Expr::Ne(..) => " != ",
                    Expr::In(..) => " IN ",
                    _ => unreachable!(),
                };
                self.node(*a, depth + 1, precedence + u8::from(precedence == 4))?;
                self.output.push_str(op);
                self.node(*b, depth + 1, precedence + u8::from(precedence != 4))?;
            }
            Expr::Pos(value) | Expr::Neg(value) | Expr::Not(value) => {
                self.output.push_str(match expr {
                    Expr::Pos(_) => "+",
                    Expr::Neg(_) => "-",
                    _ => "NOT ",
                });
                self.node(*value, depth + 1, precedence)?;
            }
            Expr::And(_) | Expr::Or(_) => {
                self.logical(expr, matches!(expr, Expr::And(_)), depth, &mut true)?;
            }
            Expr::Let(name, value, body) => {
                self.output.push_str("let ");
                self.name(name, false)?;
                self.output.push_str(" := ");
                self.node(*value, depth + 1, 1)?;
                self.output.push_str(" then ");
                self.node(*body, depth + 1, 0)?;
            }
        }
        if grouped {
            self.output.push(')');
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use crate::{FilterExpr, LanguageVersion};

    #[test]
    fn test_to_source() {
        #[derive(serde::Deserialize)]
        #[serde(deny_unknown_fields)]
        struct Case {
            source: String,
            target: String,
        }

        let cases = indoc::indoc! { r#"
            - source: ""
              target: "/*! v1 */"
            - source: "/*! v1 */"
              target: "/*! v1 */"
            - source: "name = 'John' AND age > 18"
              target: "/*! v1 */ name = 'John' AND age > 18"
            - source: "1 + 2 * 3"
              target: "/*! v1 */ 1 + 2 * 3"
            - source: "(a + b) * c"
              target: "/*! v1 */ (a + b) * c"
            - source: "a - (b - c)"
              target: "/*! v1 */ a - (b - c)"
            - source: "a / (b * c)"
              target: "/*! v1 */ a / (b * c)"
            - source: "a + (b + c)"
              target: "/*! v1 */ a + (b + c)"
            - source: "(a = b) = c"
              target: "/*! v1 */ (a = b) = c"
            - source: "a = (b = c)"
              target: "/*! v1 */ a = b = c"
            - source: "(a OR b) AND c"
              target: "/*! v1 */ (a OR b) AND c"
            - source: "NOT (a AND b)"
              target: "/*! v1 */ NOT (a AND b)"
            - source: "NOT a = b"
              target: "/*! v1 */ NOT a = b"
            - source: "-(a + b)"
              target: "/*! v1 */ -(a + b)"
            - source: "-42"
              target: "/*! v1 */ -42"
            - source: "(1).abs()"
              target: "/*! v1 */ (1).abs()"
            - source: "(-1).abs()"
              target: "/*! v1 */ (-1).abs()"
            - source: "(a + b).field[0]"
              target: "/*! v1 */ (a + b).field[0]"
            - source: "f(a + b, [1, 2])"
              target: "/*! v1 */ f(a + b, [1, 2])"
            - source: "(let x := 1 then x) + 2"
              target: "/*! v1 */ (let x := 1 then x) + 2"
            - source: "let x := (let y := 1 then y) then x + 2"
              target: "/*! v1 */ let x := (let y := 1 then y) then x + 2"
        "# };
        let cases: Vec<Case> = serde_yaml_ng::from_str(cases).unwrap();

        for Case { source: input, target: expected } in cases {
            let filter = FilterExpr::parse(&input).unwrap();
            let source = filter.to_source().unwrap();
            assert_eq!(source, expected, "{input}");

            let reparsed = FilterExpr::parse(&source).unwrap();
            assert_eq!(reparsed.language_version(), LanguageVersion::V1);
            assert_eq!(reparsed.is_empty(), filter.is_empty());
            assert_eq!(format!("{reparsed:?}"), format!("{filter:?}"), "{input}");
            assert_eq!(reparsed.to_source().unwrap(), source);
        }
    }
}
