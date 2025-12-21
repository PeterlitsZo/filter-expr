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
}
