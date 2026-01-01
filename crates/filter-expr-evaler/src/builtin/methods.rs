use crate::{Error, Method, MethodContext, Value};

pub(crate) struct MethodStrToUppercase;

#[async_trait::async_trait]
impl Method for MethodStrToUppercase {
    async fn call(&self, ctx: MethodContext<'_, '_>) -> Result<Value, Error> {
        let s = match ctx.obj {
            Value::Str(s) => s,
            _ => {
                return Err(Error::Internal("object is not a string".to_string()));
            }
        };

        if !ctx.args.is_empty() {
            return Err(Error::InvalidArgumentCountForMethod {
                method: "to_uppercase".to_string(),
                expected: 0,
                got: ctx.args.len(),
            });
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
                return Err(Error::Internal("object is not a string".to_string()));
            }
        };

        if !ctx.args.is_empty() {
            return Err(Error::InvalidArgumentCountForMethod {
                method: "to_lowercase".to_string(),
                expected: 0,
                got: ctx.args.len(),
            });
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
                return Err(Error::Internal("object is not a string".to_string()));
            }
        };

        if ctx.args.len() != 1 {
            return Err(Error::InvalidArgumentCountForMethod {
                method: "contains".to_string(),
                expected: 1,
                got: ctx.args.len(),
            });
        }

        let arg = match &ctx.args[0] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::Internal("argument is not a string".to_string()));
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
                return Err(Error::Internal("object is not a string".to_string()));
            }
        };

        if ctx.args.len() != 1 {
            return Err(Error::InvalidArgumentCountForMethod {
                method: "starts_with".to_string(),
                expected: 1,
                got: ctx.args.len(),
            });
        }

        let arg = match &ctx.args[0] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::Internal("argument is not a string".to_string()));
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
                return Err(Error::Internal("object is not a string".to_string()));
            }
        };

        if ctx.args.len() != 1 {
            return Err(Error::InvalidArgumentCountForMethod {
                method: "ends_with".to_string(),
                expected: 1,
                got: ctx.args.len(),
            });
        }

        let arg = match &ctx.args[0] {
            Value::Str(s) => s,
            _ => {
                return Err(Error::Internal("argument is not a string".to_string()));
            }
        };

        Ok(Value::bool(s.ends_with(arg.as_str())))
    }
}
