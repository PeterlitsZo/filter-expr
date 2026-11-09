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
/// `"bool"`, `"datetime"`, `"null"`, `"array"`, `"object"`, or `"userdata"`.
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
            ValueType::DateTime => "datetime",
            ValueType::Null => "null",
            ValueType::Array => "array",
            ValueType::Object => "object",
            ValueType::Userdata => "userdata",
        }))
    }
}

/// Implements the built-in `abs(x)` function.
pub(crate) struct FunctionAbs;

#[async_trait::async_trait]
impl Function for FunctionAbs {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "abs")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        match ctx.args[0] {
            Value::I64(value) => value.checked_abs().map(Value::I64).ok_or_else(|| {
                Error::new(ErrorKind::InvalidValue, "integer absolute value overflow")
                    .with_metadata("function", "abs")
            }),
            Value::F64(value) => Ok(Value::F64(value.abs())),
            _ => Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                .with_metadata("function", "abs")
                .with_metadata("index", 0)
                .with_metadata("expected", "i64 or f64")
                .with_metadata("got", ctx.args[0].typ())),
        }
    }
}

/// Implements the built-in `ceil(x)` function.
pub(crate) struct FunctionCeil;

#[async_trait::async_trait]
impl Function for FunctionCeil {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "ceil")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        match ctx.args[0] {
            Value::I64(value) => Ok(Value::I64(value)),
            Value::F64(value) => Ok(Value::F64(value.ceil())),
            _ => Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                .with_metadata("function", "ceil")
                .with_metadata("index", 0)
                .with_metadata("expected", "i64 or f64")
                .with_metadata("got", ctx.args[0].typ())),
        }
    }
}

/// Implements the built-in `floor(x)` function.
pub(crate) struct FunctionFloor;

#[async_trait::async_trait]
impl Function for FunctionFloor {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "floor")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        match ctx.args[0] {
            Value::I64(value) => Ok(Value::I64(value)),
            Value::F64(value) => Ok(Value::F64(value.floor())),
            _ => Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                .with_metadata("function", "floor")
                .with_metadata("index", 0)
                .with_metadata("expected", "i64 or f64")
                .with_metadata("got", ctx.args[0].typ())),
        }
    }
}

/// Implements the built-in `round(x)` function.
pub(crate) struct FunctionRound;

#[async_trait::async_trait]
impl Function for FunctionRound {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "round")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        match ctx.args[0] {
            Value::I64(value) => Ok(Value::I64(value)),
            Value::F64(value) => Ok(Value::F64(value.round())),
            _ => Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                .with_metadata("function", "round")
                .with_metadata("index", 0)
                .with_metadata("expected", "i64 or f64")
                .with_metadata("got", ctx.args[0].typ())),
        }
    }
}

/// Implements the built-in `pow(x, exponent)` function.
pub(crate) struct FunctionPow;

#[async_trait::async_trait]
impl Function for FunctionPow {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 2 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "pow")
                    .with_metadata("expected", 2)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let x = match ctx.args[0] {
            Value::I64(value) => value as f64,
            Value::F64(value) => value,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "pow")
                    .with_metadata("index", 0)
                    .with_metadata("expected", "i64 or f64")
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };
        let exponent = match ctx.args[1] {
            Value::I64(value) => value as f64,
            Value::F64(value) => value,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "pow")
                    .with_metadata("index", 1)
                    .with_metadata("expected", "i64 or f64")
                    .with_metadata("got", ctx.args[1].typ()));
            }
        };

        Ok(Value::F64(x.powf(exponent)))
    }
}

/// Implements the built-in `sqrt(x)` function.
pub(crate) struct FunctionSqrt;

#[async_trait::async_trait]
impl Function for FunctionSqrt {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "sqrt")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let x = match ctx.args[0] {
            Value::I64(value) => value as f64,
            Value::F64(value) => value,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "sqrt")
                    .with_metadata("index", 0)
                    .with_metadata("expected", "i64 or f64")
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };

        Ok(Value::F64(x.sqrt()))
    }
}

/// Implements the built-in `exp(x)` function.
pub(crate) struct FunctionExp;

#[async_trait::async_trait]
impl Function for FunctionExp {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "exp")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let x = match ctx.args[0] {
            Value::I64(value) => value as f64,
            Value::F64(value) => value,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "exp")
                    .with_metadata("index", 0)
                    .with_metadata("expected", "i64 or f64")
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };

        Ok(Value::F64(x.exp()))
    }
}

/// Implements the built-in `ln(x)` function.
pub(crate) struct FunctionLn;

#[async_trait::async_trait]
impl Function for FunctionLn {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "ln")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let x = match ctx.args[0] {
            Value::I64(value) => value as f64,
            Value::F64(value) => value,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "ln")
                    .with_metadata("index", 0)
                    .with_metadata("expected", "i64 or f64")
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };

        Ok(Value::F64(x.ln()))
    }
}

/// Implements the built-in `log(x, base)` function.
pub(crate) struct FunctionLog;

#[async_trait::async_trait]
impl Function for FunctionLog {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 2 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "log")
                    .with_metadata("expected", 2)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let x = match ctx.args[0] {
            Value::I64(value) => value as f64,
            Value::F64(value) => value,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "log")
                    .with_metadata("index", 0)
                    .with_metadata("expected", "i64 or f64")
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };
        let base = match ctx.args[1] {
            Value::I64(value) => value as f64,
            Value::F64(value) => value,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "log")
                    .with_metadata("index", 1)
                    .with_metadata("expected", "i64 or f64")
                    .with_metadata("got", ctx.args[1].typ()));
            }
        };

        Ok(Value::F64(x.log(base)))
    }
}

/// Implements the built-in `log2(x)` function.
pub(crate) struct FunctionLog2;

#[async_trait::async_trait]
impl Function for FunctionLog2 {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "log2")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let x = match ctx.args[0] {
            Value::I64(value) => value as f64,
            Value::F64(value) => value,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "log2")
                    .with_metadata("index", 0)
                    .with_metadata("expected", "i64 or f64")
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };

        Ok(Value::F64(x.log2()))
    }
}

/// Implements the built-in `log10(x)` function.
pub(crate) struct FunctionLog10;

#[async_trait::async_trait]
impl Function for FunctionLog10 {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "log10")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let x = match ctx.args[0] {
            Value::I64(value) => value as f64,
            Value::F64(value) => value,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("function", "log10")
                    .with_metadata("index", 0)
                    .with_metadata("expected", "i64 or f64")
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };

        Ok(Value::F64(x.log10()))
    }
}

/// Returns the time of the first `now()` call in this evaluation.
pub(crate) struct FunctionDateTimeNow;

#[async_trait::async_trait]
impl Function for FunctionDateTimeNow {
    async fn call(&self, ctx: FunctionContext<'_, '_>) -> Result<Value, Error> {
        if !ctx.args.is_empty() {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("function", "DateTime::now")
                    .with_metadata("expected", 0)
                    .with_metadata("got", ctx.args.len()),
            );
        }
        Ok(Value::datetime(ctx.state.now()))
    }
}
