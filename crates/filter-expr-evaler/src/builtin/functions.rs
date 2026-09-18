use crate::{Error, ErrorKind, Function, FunctionContext, Value, ValueType};

/// Implements the built-in `matches(text, pattern) -> bool` function.
///
/// Both arguments must be strings. `pattern` is interpreted as a regular expression,
/// and the function reports whether it matches anywhere in `text`.
///
/// # Examples
///
/// ```text
/// matches("foobar", "^foo") // => true
/// matches("foobar", "^bar") // => false
/// ```
pub(crate) struct FunctionMatches;

#[async_trait::async_trait]
impl Function for FunctionMatches {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 2 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "matches")
                    .with_metadata("expected", 2)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let text = match &ctx.args[0] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "matches")
                    .with_metadata("index", 0)
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };
        let pattern = match &ctx.args[1] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "matches")
                    .with_metadata("index", 1)
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.args[1].typ()));
            }
        };

        let regex = ctx.env.get_regex(pattern.as_str())?;

        let matches = regex.is_match(text);

        Ok(Value::Bool(matches))
    }
}

/// Implements the built-in `type(value) -> str` function.
///
/// Returns the evaluator's canonical type name: `"str"`, `"i64"`, `"f64"`,
/// `"bool"`, `"null"`, `"array"`, or `"userdata"`.
///
/// # Examples
///
/// ```text
/// type("foobar") // => "str"
/// type(42)       // => "i64"
/// type(null)     // => "null"
/// ```
pub(crate) struct FunctionType;

#[async_trait::async_trait]
impl Function for FunctionType {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "matches")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        Ok(Value::str(match ctx.args[0].typ() {
            ValueType::Str => "str",
            ValueType::I64 => "i64",
            ValueType::F64 => "f64",
            ValueType::Bool => "bool",
            ValueType::Null => "null",
            ValueType::Array => "array",
            ValueType::Userdata => "userdata",
        }))
    }
}
