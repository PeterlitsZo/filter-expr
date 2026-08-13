use crate::{Error, ErrorKind, Method, MethodContext, Value, ValueType};

pub(crate) struct MethodStrToUppercase;

#[async_trait::async_trait]
impl Method for MethodStrToUppercase {
    async fn call(&self, ctx: MethodContext<'_, '_>) -> Result<Value, Error> {
        let s = match ctx.obj {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::Internal, "object is not a string")
                    .with_metadata("method", "to_uppercase")
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.obj.typ()));
            }
        };

        if !ctx.args.is_empty() {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("method", "to_uppercase")
                    .with_metadata("expected", 0)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        Ok(Value::str(s.to_uppercase()))
    }
}

pub(crate) struct MethodStrToLowercase;

#[async_trait::async_trait]
impl Method for MethodStrToLowercase {
    async fn call(&self, ctx: MethodContext<'_, '_>) -> Result<Value, Error> {
        let s = match ctx.obj {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::Internal, "object is not a string")
                    .with_metadata("method", "to_lowercase")
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.obj.typ()));
            }
        };

        if !ctx.args.is_empty() {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("method", "to_lowercase")
                    .with_metadata("expected", 0)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        Ok(Value::str(s.to_lowercase()))
    }
}

pub(crate) struct MethodStrContains;

#[async_trait::async_trait]
impl Method for MethodStrContains {
    async fn call(&self, ctx: MethodContext<'_, '_>) -> Result<Value, Error> {
        let s = match ctx.obj {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::Internal, "object is not a string")
                    .with_metadata("method", "contains")
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.obj.typ()));
            }
        };

        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("method", "contains")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let arg = match &ctx.args[0] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("method", "contains")
                    .with_metadata("index", 0)
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };

        Ok(Value::bool(s.contains(arg.as_str())))
    }
}

pub(crate) struct MethodStrStartsWith;

#[async_trait::async_trait]
impl Method for MethodStrStartsWith {
    async fn call(&self, ctx: MethodContext<'_, '_>) -> Result<Value, Error> {
        let s = match ctx.obj {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::Internal, "object is not a string")
                    .with_metadata("method", "starts_with")
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.obj.typ()));
            }
        };

        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("method", "starts_with")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let arg = match &ctx.args[0] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("method", "starts_with")
                    .with_metadata("index", 0)
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };

        Ok(Value::bool(s.starts_with(arg.as_str())))
    }
}

pub(crate) struct MethodStrEndsWith;

#[async_trait::async_trait]
impl Method for MethodStrEndsWith {
    async fn call(&self, ctx: MethodContext<'_, '_>) -> Result<Value, Error> {
        let s = match ctx.obj {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::Internal, "object is not a string")
                    .with_metadata("method", "ends_with")
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.obj.typ()));
            }
        };

        if ctx.args.len() != 1 {
            return Err(
                Error::new(ErrorKind::TypeMismatch, "invalid argument count")
                    .with_metadata("method", "ends_with")
                    .with_metadata("expected", 1)
                    .with_metadata("got", ctx.args.len()),
            );
        }

        let arg = match &ctx.args[0] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::new(ErrorKind::TypeMismatch, "invalid argument type")
                    .with_metadata("method", "ends_with")
                    .with_metadata("index", 0)
                    .with_metadata("expected", ValueType::Str)
                    .with_metadata("got", ctx.args[0].typ()));
            }
        };

        Ok(Value::bool(s.ends_with(arg.as_str())))
    }
}
