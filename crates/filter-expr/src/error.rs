#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("parse the expression: {0}")]
    Parse(String),

    #[error("unsupported character: {0}")]
    UnsupportedCharacter(char),
}
