use std::sync::Arc;

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BytecodeContextVar {
    pub(crate) name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BytecodeFunction {
    pub(crate) name: String,
}

#[derive(Debug, Clone, PartialEq)]
pub(crate) struct BytecodeMethod {
    pub(crate) name: String,
}

pub(crate) struct Bytecode {
    /// The information of the context variables.
    pub(crate) context_vars: Vec<BytecodeContextVar>,
    /// The number of local variable slots.
    pub(crate) local_count: u32,
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
    LoadContext,
    LoadLocal,
    StoreLocal,
    GetField,

    LoadString,
    LoadI64,
    LoadF64,
    LoadBool,
    LoadNull,

    BuildArray,
    BuildObject,

    CallFunction,
    CallMethod,

    PopJumpIfFalse,
    JumpIfFalse,
    PopJumpIfTrue,
    JumpIfTrue,
    Jump,

    Dup,

    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pos,
    Neg,

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
            BytecodeOpcode::LoadContext => LOAD_CONTEXT,
            BytecodeOpcode::LoadLocal => LOAD_LOCAL,
            BytecodeOpcode::StoreLocal => STORE_LOCAL,
            BytecodeOpcode::GetField => GET_FIELD,
            BytecodeOpcode::LoadString => LOAD_STRING,
            BytecodeOpcode::LoadI64 => LOAD_I64,
            BytecodeOpcode::LoadF64 => LOAD_F64,
            BytecodeOpcode::LoadBool => LOAD_BOOL,
            BytecodeOpcode::LoadNull => LOAD_NULL,
            BytecodeOpcode::BuildArray => BUILD_ARRAY,
            BytecodeOpcode::BuildObject => BUILD_OBJECT,
            BytecodeOpcode::CallFunction => CALL_FUNCTION,
            BytecodeOpcode::CallMethod => CALL_METHOD,
            BytecodeOpcode::PopJumpIfFalse => POP_JUMP_IF_FALSE,
            BytecodeOpcode::JumpIfFalse => JUMP_IF_FALSE,
            BytecodeOpcode::PopJumpIfTrue => POP_JUMP_IF_TRUE,
            BytecodeOpcode::JumpIfTrue => JUMP_IF_TRUE,
            BytecodeOpcode::Jump => JUMP,
            BytecodeOpcode::Dup => DUP,
            BytecodeOpcode::Add => ADD,
            BytecodeOpcode::Sub => SUB,
            BytecodeOpcode::Mul => MUL,
            BytecodeOpcode::Div => DIV,
            BytecodeOpcode::Rem => REM,
            BytecodeOpcode::Pos => POS,
            BytecodeOpcode::Neg => NEG,
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
            LOAD_CONTEXT => Some(BytecodeOpcode::LoadContext),
            LOAD_LOCAL => Some(BytecodeOpcode::LoadLocal),
            STORE_LOCAL => Some(BytecodeOpcode::StoreLocal),
            GET_FIELD => Some(BytecodeOpcode::GetField),
            LOAD_STRING => Some(BytecodeOpcode::LoadString),
            LOAD_I64 => Some(BytecodeOpcode::LoadI64),
            LOAD_F64 => Some(BytecodeOpcode::LoadF64),
            LOAD_BOOL => Some(BytecodeOpcode::LoadBool),
            LOAD_NULL => Some(BytecodeOpcode::LoadNull),
            BUILD_ARRAY => Some(BytecodeOpcode::BuildArray),
            BUILD_OBJECT => Some(BytecodeOpcode::BuildObject),
            CALL_FUNCTION => Some(BytecodeOpcode::CallFunction),
            CALL_METHOD => Some(BytecodeOpcode::CallMethod),
            POP_JUMP_IF_FALSE => Some(BytecodeOpcode::PopJumpIfFalse),
            JUMP_IF_FALSE => Some(BytecodeOpcode::JumpIfFalse),
            POP_JUMP_IF_TRUE => Some(BytecodeOpcode::PopJumpIfTrue),
            JUMP_IF_TRUE => Some(BytecodeOpcode::JumpIfTrue),
            JUMP => Some(BytecodeOpcode::Jump),
            DUP => Some(BytecodeOpcode::Dup),
            ADD => Some(BytecodeOpcode::Add),
            SUB => Some(BytecodeOpcode::Sub),
            MUL => Some(BytecodeOpcode::Mul),
            DIV => Some(BytecodeOpcode::Div),
            REM => Some(BytecodeOpcode::Rem),
            POS => Some(BytecodeOpcode::Pos),
            NEG => Some(BytecodeOpcode::Neg),
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
            BytecodeOpcode::LoadContext => 5,
            BytecodeOpcode::LoadLocal => 5,
            BytecodeOpcode::StoreLocal => 5,
            BytecodeOpcode::GetField => 5,

            BytecodeOpcode::LoadString => 5,
            BytecodeOpcode::LoadI64 => 9,
            BytecodeOpcode::LoadF64 => 9,
            BytecodeOpcode::LoadBool => 2,
            BytecodeOpcode::LoadNull => 1,

            BytecodeOpcode::BuildArray => 5,
            BytecodeOpcode::BuildObject => 5,

            BytecodeOpcode::CallFunction => 9,
            BytecodeOpcode::CallMethod => 9,

            BytecodeOpcode::PopJumpIfFalse => 5,
            BytecodeOpcode::JumpIfFalse => 5,
            BytecodeOpcode::PopJumpIfTrue => 5,
            BytecodeOpcode::JumpIfTrue => 5,
            BytecodeOpcode::Jump => 5,

            BytecodeOpcode::Dup => 1,
            BytecodeOpcode::Add => 1,
            BytecodeOpcode::Sub => 1,
            BytecodeOpcode::Mul => 1,
            BytecodeOpcode::Div => 1,
            BytecodeOpcode::Rem => 1,
            BytecodeOpcode::Pos => 1,
            BytecodeOpcode::Neg => 1,
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

macro_rules! define_opcodes {
    (@next $value:expr;) => {};
    (@next $value:expr; $name:ident, $($rest:ident,)*) => {
        pub(crate) const $name: u8 = $value;
        define_opcodes!(@next $name + 1; $($rest,)*);
    };
    ($($name:ident),* $(,)?) => {
        define_opcodes!(@next 0; $($name,)*);
    };
}

define_opcodes! {
    LOAD_CONTEXT,
    LOAD_LOCAL,
    STORE_LOCAL,
    GET_FIELD,

    LOAD_STRING,
    LOAD_I64,
    LOAD_F64,
    LOAD_BOOL,
    LOAD_NULL,

    BUILD_ARRAY,
    BUILD_OBJECT,

    CALL_FUNCTION,
    CALL_METHOD,

    POP_JUMP_IF_FALSE,
    JUMP_IF_FALSE,
    POP_JUMP_IF_TRUE,
    JUMP_IF_TRUE,
    JUMP,

    DUP,

    ADD,
    SUB,
    MUL,
    DIV,
    REM,
    POS,
    NEG,

    GT,
    LT,
    GE,
    LE,
    EQ,
    NE,
    IN,

    NOT,

    RETURN,
}
