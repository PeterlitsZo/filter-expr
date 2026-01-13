#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("transform the expression: {0}")]
    Transform(Box<dyn std::error::Error + Send + Sync>),

    #[error("parse the expression: {0}")]
    Parse(String),

    #[error("unsupported character: {0}")]
    UnsupportedCharacter(char),
}
