use std::sync::Arc;

use filter_expr::FunctionPath;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BytecodeLocal {
    pub(crate) name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BytecodeFunction {
    pub(crate) name: FunctionPath,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BytecodeMethod {
    pub(crate) name: String,
}

pub(crate) struct Bytecode {
    /// The information of the local variables.
    pub(crate) locals: Vec<BytecodeLocal>,
    /// The information of the functions.
    pub(crate) functions: Vec<BytecodeFunction>,
    /// The information of the methods.
    pub(crate) methods: Vec<BytecodeMethod>,

    /// The string pool.
    pub(crate) str_pool: Vec<Arc<String>>,

    /// The code of the bytecode.
    pub(crate) code: Vec<u8>,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) enum BytecodeOpcode {
    LoadLocal,

    LoadString,
    LoadI64,
    LoadF64,
    LoadBool,
    LoadNull,

    BuildArray,

    CallFunction,
    CallMethod,

    PopJumpIfFalse,
    JumpIfFalse,
    PopJumpIfTrue,
    JumpIfTrue,
    Jump,

    Dup,

    Gt,
    Lt,
    Ge,
    Le,
    Eq,
    Ne,
    In,

    Not,

    Return,
}

impl BytecodeOpcode {
    pub(crate) const fn into_u8(self) -> u8 {
        match self {
            BytecodeOpcode::LoadLocal => LOAD_LOCAL,
            BytecodeOpcode::LoadString => LOAD_STRING,
            BytecodeOpcode::LoadI64 => LOAD_I64,
            BytecodeOpcode::LoadF64 => LOAD_F64,
            BytecodeOpcode::LoadBool => LOAD_BOOL,
            BytecodeOpcode::LoadNull => LOAD_NULL,
            BytecodeOpcode::BuildArray => BUILD_ARRAY,
            BytecodeOpcode::CallFunction => CALL_FUNCTION,
            BytecodeOpcode::CallMethod => CALL_METHOD,
            BytecodeOpcode::PopJumpIfFalse => POP_JUMP_IF_FALSE,
            BytecodeOpcode::JumpIfFalse => JUMP_IF_FALSE,
            BytecodeOpcode::PopJumpIfTrue => POP_JUMP_IF_TRUE,
            BytecodeOpcode::JumpIfTrue => JUMP_IF_TRUE,
            BytecodeOpcode::Jump => JUMP,
            BytecodeOpcode::Dup => DUP,
            BytecodeOpcode::Gt => GT,
            BytecodeOpcode::Lt => LT,
            BytecodeOpcode::Ge => GE,
            BytecodeOpcode::Le => LE,
            BytecodeOpcode::Eq => EQ,
            BytecodeOpcode::Ne => NE,
            BytecodeOpcode::In => IN,
            BytecodeOpcode::Not => NOT,
            BytecodeOpcode::Return => RETURN,
        }
    }

    #[allow(dead_code)]
    pub(crate) const fn from_u8(value: u8) -> Option<Self> {
        match value {
            LOAD_LOCAL => Some(BytecodeOpcode::LoadLocal),
            LOAD_STRING => Some(BytecodeOpcode::LoadString),
            LOAD_I64 => Some(BytecodeOpcode::LoadI64),
            LOAD_F64 => Some(BytecodeOpcode::LoadF64),
            LOAD_BOOL => Some(BytecodeOpcode::LoadBool),
            LOAD_NULL => Some(BytecodeOpcode::LoadNull),
            BUILD_ARRAY => Some(BytecodeOpcode::BuildArray),
            CALL_FUNCTION => Some(BytecodeOpcode::CallFunction),
            CALL_METHOD => Some(BytecodeOpcode::CallMethod),
            POP_JUMP_IF_FALSE => Some(BytecodeOpcode::PopJumpIfFalse),
            JUMP_IF_FALSE => Some(BytecodeOpcode::JumpIfFalse),
            POP_JUMP_IF_TRUE => Some(BytecodeOpcode::PopJumpIfTrue),
            JUMP_IF_TRUE => Some(BytecodeOpcode::JumpIfTrue),
            JUMP => Some(BytecodeOpcode::Jump),
            DUP => Some(BytecodeOpcode::Dup),
            GT => Some(BytecodeOpcode::Gt),
            LT => Some(BytecodeOpcode::Lt),
            GE => Some(BytecodeOpcode::Ge),
            LE => Some(BytecodeOpcode::Le),
            EQ => Some(BytecodeOpcode::Eq),
            NE => Some(BytecodeOpcode::Ne),
            IN => Some(BytecodeOpcode::In),
            NOT => Some(BytecodeOpcode::Not),
            RETURN => Some(BytecodeOpcode::Return),
            _ => None,
        }
    }

    pub fn op_len(self) -> usize {
        match self {
            BytecodeOpcode::LoadLocal => 5,

            BytecodeOpcode::LoadString => 5,
            BytecodeOpcode::LoadI64 => 9,
            BytecodeOpcode::LoadF64 => 9,
            BytecodeOpcode::LoadBool => 2,
            BytecodeOpcode::LoadNull => 1,

            BytecodeOpcode::BuildArray => 5,

            BytecodeOpcode::CallFunction => 9,
            BytecodeOpcode::CallMethod => 9,

            BytecodeOpcode::PopJumpIfFalse => 5,
            BytecodeOpcode::JumpIfFalse => 5,
            BytecodeOpcode::PopJumpIfTrue => 5,
            BytecodeOpcode::JumpIfTrue => 5,
            BytecodeOpcode::Jump => 5,

            BytecodeOpcode::Dup => 1,
            BytecodeOpcode::Gt => 1,
            BytecodeOpcode::Lt => 1,
            BytecodeOpcode::Ge => 1,
            BytecodeOpcode::Le => 1,
            BytecodeOpcode::Eq => 1,
            BytecodeOpcode::Ne => 1,
            BytecodeOpcode::In => 1,

            BytecodeOpcode::Not => 1,

            BytecodeOpcode::Return => 1,
        }
    }
}

pub(crate) const LOAD_LOCAL: u8 = 0;
pub(crate) const LOAD_STRING: u8 = 1;
pub(crate) const LOAD_I64: u8 = 2;
pub(crate) const LOAD_F64: u8 = 3;
pub(crate) const LOAD_BOOL: u8 = 4;
pub(crate) const LOAD_NULL: u8 = 5;
pub(crate) const BUILD_ARRAY: u8 = 6;
pub(crate) const CALL_FUNCTION: u8 = 7;
pub(crate) const CALL_METHOD: u8 = 8;
pub(crate) const POP_JUMP_IF_FALSE: u8 = 9;
pub(crate) const JUMP_IF_FALSE: u8 = 10;
pub(crate) const POP_JUMP_IF_TRUE: u8 = 11;
pub(crate) const JUMP_IF_TRUE: u8 = 12;
pub(crate) const JUMP: u8 = 13;
pub(crate) const DUP: u8 = 14;
pub(crate) const GT: u8 = 15;
pub(crate) const LT: u8 = 16;
pub(crate) const GE: u8 = 17;
pub(crate) const LE: u8 = 18;
pub(crate) const EQ: u8 = 19;
pub(crate) const NE: u8 = 20;
pub(crate) const IN: u8 = 21;
pub(crate) const NOT: u8 = 22;
pub(crate) const RETURN: u8 = 23;
