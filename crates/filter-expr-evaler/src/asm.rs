use core::fmt;
use std::sync::Arc;

use filter_expr::FunctionPath;

#[derive(Debug, Clone, PartialEq, Default)]
pub(crate) struct Asm {
    pub(crate) locals: Vec<AsmLocal>,
    pub(crate) functions: Vec<AsmFunction>,
    pub(crate) methods: Vec<AsmMethod>,
    pub(crate) labels: Vec<AsmLabel>,
    pub(crate) str_pool: Vec<Arc<String>>,

    pub(crate) instructions: Vec<AsmInstruction>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AsmLocal {
    pub(crate) name: String,
}

impl AsmLocal {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AsmFunction {
    pub(crate) name: FunctionPath,
}

impl AsmFunction {
    pub(crate) fn new(name: FunctionPath) -> Self {
        Self { name: name.into() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AsmMethod {
    pub(crate) name: String,
}

impl AsmMethod {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct AsmLabel {
    pub(crate) name: String,
}

impl AsmLabel {
    pub(crate) fn new(name: impl Into<String>) -> Self {
        Self { name: name.into() }
    }
}

/// An instruction in the assembly code.
#[derive(Clone, PartialEq)]
pub(crate) enum AsmInstruction {
    /// Define a label. The argument is the index of the label to define.
    Label(u32),

    /// Load a local variable from the local variables table, and push it onto
    /// the stack. The argument is the index of the local variable in the local
    /// variables table.
    LoadLocal(u32),

    /// Load a string value from the string pool, and push it onto the stack.
    /// The argument is the index of the constant in the string pool.
    LoadString(u32),
    /// Load an i64 value, and push it onto the stack. The argument is the value
    /// to load.
    LoadI64(i64),
    /// Load an f64 value, and push it onto the stack. The argument is the value
    /// to load.
    LoadF64(f64),
    /// Load a bool value, and push it onto the stack. The argument is the value
    /// to load.
    LoadBool(bool),
    /// Load a null value, and push it onto the stack.
    LoadNull,

    /// Build an array from the top of the stack, and push it onto the stack.
    /// The argument is the number of elements in the array.
    BuildArray(u32),

    /// Call a function. The first argument is the index of the function in the
    /// functions table. The second argument is the number of arguments.
    CallFunction(u32, u32),
    /// Call a method. The first argument is the index of the method in the
    /// methods table. The second argument is the number of arguments.
    CallMethod(u32, u32),

    /// Jump if the top of the stack is false. The argument is the index of the
    /// label to jump to. The top of the stack is popped.
    #[allow(unused)]
    PopJumpIfFalse(u32),
    /// Duplicate the top of the stack, then jump if it is false. The argument is
    /// the index of the label to jump to. The top of the stack is not popped.
    JumpIfFalse(u32),
    /// Jump if the top of the stack is true. The argument is the index of the
    /// label to jump to. The top of the stack is popped.
    #[allow(unused)]
    PopJumpIfTrue(u32),
    /// Duplicate the top of the stack, then jump if it is true. The argument is
    /// the index of the label to jump to. The top of the stack is not popped.
    JumpIfTrue(u32),
    /// Jump to the label. The argument is the index of the label to jump to.
    /// The stack is not changed.
    #[allow(unused)]
    Jump(u32),

    /// Duplicate the top of the stack.
    #[allow(unused)]
    Dup,

    /// Pop the two top values from the stack, and push true if the first is
    /// greater than the second, false otherwise.
    Gt,
    /// Pop the two top values from the stack, and push true if the first is
    /// less than the second, false otherwise.
    Lt,
    /// Pop the two top values from the stack, and push true if the first is
    /// greater than or equal to the second, false otherwise.
    Ge,
    /// Pop the two top values from the stack, and push true if the first is
    /// less than or equal to the second, false otherwise.
    Le,
    /// Pop the two top values from the stack, and push true if the first is
    /// equal to the second, false otherwise.
    Eq,
    /// Pop the two top values from the stack, and push true if the first is
    /// not equal to the second, false otherwise.
    Ne,
    /// Pop the two top values from the stack, and push true if the first is
    /// in the second, false otherwise.
    In,

    /// Pop the top value from the stack, and push true if the value is not true,
    /// false otherwise.
    Not,

    /// Return from the current function.
    Return,
}

impl fmt::Debug for AsmInstruction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            AsmInstruction::Label(index) => write!(f, "LABEL label_index={index}"),
            AsmInstruction::LoadLocal(index) => write!(f, "LOAD_LOCAL local_index={index}"),
            AsmInstruction::LoadString(index) => write!(f, "LOAD_STR str_index={index}"),
            AsmInstruction::LoadI64(value) => write!(f, "LOAD_I64 value={value}"),
            AsmInstruction::LoadF64(value) => write!(f, "LOAD_F64 value={value}"),
            AsmInstruction::LoadBool(value) => write!(f, "LOAD_BOOL value={value}"),
            AsmInstruction::LoadNull => write!(f, "LOAD_NULL"),
            AsmInstruction::BuildArray(length) => write!(f, "BUILD_ARRAY length={length}"),
            AsmInstruction::CallFunction(function_index, argument_count) => {
                write!(
                    f,
                    "CALL_FUNCTION fn_index={function_index} args_count={argument_count}"
                )
            }
            AsmInstruction::CallMethod(method_index, argument_count) => {
                write!(
                    f,
                    "CALL_METHOD method_index={method_index} args_count={argument_count}"
                )
            }
            AsmInstruction::PopJumpIfFalse(label_index) => {
                write!(f, "POP_JUMP_IF_FALSE label_index={label_index}")
            }
            AsmInstruction::JumpIfFalse(label_index) => {
                write!(f, "JUMP_IF_FALSE label_index={label_index}")
            }
            AsmInstruction::PopJumpIfTrue(label_index) => {
                write!(f, "POP_JUMP_IF_TRUE label_index={label_index}")
            }
            AsmInstruction::JumpIfTrue(label_index) => {
                write!(f, "JUMP_IF_TRUE label_index={label_index}")
            }
            AsmInstruction::Jump(label_index) => write!(f, "JUMP label_index={label_index}"),
            AsmInstruction::Dup => write!(f, "DUP"),
            AsmInstruction::Gt => write!(f, "GT"),
            AsmInstruction::Lt => write!(f, "LT"),
            AsmInstruction::Ge => write!(f, "GE"),
            AsmInstruction::Le => write!(f, "LE"),
            AsmInstruction::Eq => write!(f, "EQ"),
            AsmInstruction::Ne => write!(f, "NE"),
            AsmInstruction::In => write!(f, "IN"),
            AsmInstruction::Not => write!(f, "NOT"),
            AsmInstruction::Return => write!(f, "RETURN"),
        }
    }
}
