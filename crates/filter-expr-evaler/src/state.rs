use std::sync::OnceLock;

/// State shared by calls within one evaluation, never across evaluations.
#[derive(Debug, Default)]
pub struct EvaluationState {
    now: OnceLock<jiff::Timestamp>,
}

impl EvaluationState {
    /// Return the time captured by the first call in this evaluation.
    pub fn now(&self) -> jiff::Timestamp {
        *self.now.get_or_init(jiff::Timestamp::now)
    }
}
