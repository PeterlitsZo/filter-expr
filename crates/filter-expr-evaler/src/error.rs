use std::{
    collections::HashMap,
    fmt::{self, Display},
};

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
pub struct Error {
    kind: ErrorKind,
    msg: String,
    metadata: HashMap<String, String>,
    source: Option<anyhow::Error>,
}

impl Error {
    pub fn new<T>(kind: ErrorKind, msg: T) -> Self
    where
        T: Into<String>,
    {
        return Self {
            kind,
            msg: msg.into(),
            metadata: HashMap::new(),
            source: None,
        };
    }

    pub fn with_source<T>(mut self, source: T) -> Self
    where
        T: Into<anyhow::Error>,
    {
        self.source = Some(source.into());
        self
    }

    pub fn with_metadata<K, V>(mut self, key: K, value: V) -> Self
    where
        K: Into<String>,
        V: Display,
    {
        self.metadata.insert(key.into(), value.to_string());
        self
    }
}

impl Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}: {}", self.kind, self.msg)?;

        if !self.metadata.is_empty() {
            write!(f, " {:?}", self.metadata)?;
        }

        if let Some(source) = &self.source {
            write!(f, ": {source}")?;
        }

        Ok(())
    }
}

impl std::error::Error for Error {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        self.source.as_ref().map(|source| source.as_ref())
    }
}

#[derive(Debug)]
pub enum ErrorKind {
    /// Internal unexpected error.
    Internal,

    /// Invalid value.
    InvalidValue,

    /// Type error.
    ///
    /// E.g. If you try to compare a string and a number, this error will be
    /// raised. If you pass the values to function/method, but arguments'
    /// type or number do not match its need, this error will be raised as well.
    TypeMismatch,

    /// Error when get the variable/function/method.
    ///
    /// Raised when not such variable/function/method or cannot get it.
    FailedToGet,
}
