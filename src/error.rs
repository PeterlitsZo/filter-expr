use crate::ExprValueType;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("internal: {0}")]
    Internal(String),

    #[error("parse the expression: {0}")]
    Parse(String),

    #[error("invalid value {0}")]
    InvalidValue(String),

    #[error("type mismatch: expected same type for comparison, got {0:?} and {1:?}")]
    TypeMismatch(String, String),

    #[error("unsupported character: {0}")]
    UnsupportedCharacter(char),

    /// The variable is not found.
    #[error("no such variable {var:?}")]
    NoSuchVar { var: String },

    /// The function is not found.
    #[error("no such function {function:?}")]
    NoSuchFunction { function: String },

    /// The method is not found.
    #[error("no such method {method:?} for type {obj_type:?}")]
    NoSuchMethod {
        method: String,
        obj_type: ExprValueType,
    },

    /// Invalid argument count for the given function.
    #[error(
        "invalid argument count for function {function:?}: expected {expected} argument(s), but got {got} argument(s)"
    )]
    InvalidArgumentCountForFunction {
        function: String,
        expected: usize,
        got: usize,
    },

    /// Invalid argument type for the given function's index-th argument.
    #[error(
        "invalid argument type for function {function:?}'s index {index} argument: expected {expected:?}, got {got:?}"
    )]
    InvalidArgumentTypeForFunction {
        function: String,
        index: usize,
        expected: ExprValueType,
        got: ExprValueType,
    },

    /// Invalid argument count for the given method.
    #[error("invalid argument count for method {method:?}: expected {expected} argument(s), but got {got} argument(s)")]
    InvalidArgumentCountForMethod {
        method: String,
        expected: usize,
        got: usize,
    },

    /// Invalid argument type for the given method's index-th argument.
    #[error("invalid argument type for method {method:?}'s index {index} argument: expected {expected:?}, got {got:?}")]
    InvalidArgumentTypeForMethod {
        method: String,
        index: usize,
        expected: ExprValueType,
        got: ExprValueType,
    },
}
