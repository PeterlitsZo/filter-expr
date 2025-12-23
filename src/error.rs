#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("parse the expression: {0}")]
    Parse(String),

    #[error("no such variable `{0}`")]
    NoSuchVar(String),

    #[error("no such function `{0}`")]
    NoSuchFunction(String),

    #[error("invalid value {0}")]
    InvalidValue(String),

    #[error("type mismatch: expected same type for comparison, got {0:?} and {1:?}")]
    TypeMismatch(String, String),

    #[error("unsupported character: {0}")]
    UnsupportedCharacter(char),

    #[error("invalid argument count: expected {expected}, got {got}")]
    InvalidArgumentCount { expected: usize, got: usize },

    #[error("invalid argument type: expected {expected}, got {got}")]
    InvalidArgumentType { expected: String, got: String },

    #[error("internal: {0}")]
    Internal(String),
}
