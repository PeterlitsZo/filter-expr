#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("parse the expression: {0}")]
    Parse(String),

    #[error("no such field `{0}`")]
    NoSuchField(String),

    #[error("invalid value: {0}")]
    InvalidValue(String),

    #[error("type mismatch: expected same type for comparison, got {0:?} and {1:?}")]
    TypeMismatch(String, String),

    #[error("unsupported character: {0}")]
    UnsupportedCharacter(char),

    #[error("invalid function: {0}")]
    InvalidFunction(String),

    #[error("invalid argument count: expected {expected}, got {got}")]
    InvalidArgumentCount { expected: usize, got: usize },

    #[error("invalid argument type: expected {expected}, got {got}")]
    InvalidArgumentType { expected: String, got: String },

    #[error("internal: {0}")]
    Internal(String),
}
