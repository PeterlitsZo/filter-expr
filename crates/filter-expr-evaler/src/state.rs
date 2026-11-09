use std::sync::OnceLock;

/// State shared by calls within one evaluation, never across evaluations.
///
/// Lazily captures immutable values from the evaluation's world.
#[derive(Debug, Default)]
pub struct EvaluationState {
    now: OnceLock<jiff::Timestamp>,
}

impl EvaluationState {
    /// Return the time captured by the first executed call in this evaluation.
    ///
    /// Later calls reuse it. Optimizations may delay or skip the capture.
    pub fn now(&self) -> jiff::Timestamp {
        *self.now.get_or_init(jiff::Timestamp::now)
    }
}
