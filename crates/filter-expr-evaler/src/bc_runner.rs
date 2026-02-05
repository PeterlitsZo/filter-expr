use std::sync::Arc;

use crate::{
    FilterExprEvalerEnv, FunctionContext, MethodContext,
    bc::{self, Bytecode},
    ctx::Context,
    error::Error,
    value::Value,
};

/// A runner for executing bytecode.
pub(crate) struct BytecodeRunner<'a> {
    /// The evaler environment.
    env: FilterExprEvalerEnv,

    /// The bytecode to run.
    bytecode: &'a Bytecode,

    /// The local slot to cache the local variables.
    ///
    /// If it is `None`, we need to get the value from the context.
    local_slot: Vec<Option<Value>>,

    /// The stack.
    stack: Vec<Value>,

    /// The program counter.
    pc: usize,
}

impl<'a> BytecodeRunner<'a> {
    /// Create a new bytecode runner from bytecode.
    pub(crate) fn new(bytecode: &'a Bytecode, env: FilterExprEvalerEnv) -> Self {
        Self {
            env,

            bytecode,

            local_slot: vec![None; bytecode.locals.len()],

            stack: Vec::with_capacity(8),
            pc: 0,
        }
    }

    /// Run the bytecode and return the result.
    ///
    /// # Safety
    ///
    /// Caller need make sure the bytecode is valid.  This implementation
    /// asserts that the bytecode is not broken.  If the bytecode is broken,
    /// maybe it will out of bounds or panic.
    pub(crate) async unsafe fn run(mut self, ctx: &dyn Context) -> Result<Value, Error> {
        let code = &self.bytecode.code;
        let code_len = code.len();

        while self.pc < code_len {
            let opcode = code[self.pc];
            self.pc += 1;

            match opcode {
                bc::LOAD_LOCAL => {
                    let index = unsafe { self.read_u32() };
                    let index = index as usize;

                    let value = match &self.local_slot[index] {
                        Some(cached) => cached.clone(),
                        None => {
                            let local = &self.bytecode.locals[index];
                            let value = ctx.get_var(&local.name).await?.ok_or_else(|| {
                                Error::NoSuchVar {
                                    var: local.name.clone(),
                                }
                            })?;
                            self.local_slot[index] = Some(value.clone());
                            value
                        }
                    };
                    self.stack.push(value);
                }

                bc::LOAD_STRING => {
                    let index = unsafe { self.read_u32() };
                    let value = Arc::clone(&self.bytecode.str_pool[index as usize]);
                    self.stack.push(Value::Str(value));
                }
                bc::LOAD_I64 => {
                    let value = unsafe { self.read_i64() };
                    self.stack.push(Value::I64(value));
                }
                bc::LOAD_F64 => {
                    let value = unsafe { self.read_f64() };
                    self.stack.push(Value::F64(value));
                }
                bc::LOAD_BOOL => {
                    let value = unsafe { self.read_bool() };
                    self.stack.push(Value::Bool(value));
                }
                bc::LOAD_NULL => {
                    self.stack.push(Value::Null);
                }

                bc::BUILD_ARRAY => {
                    let length = unsafe { self.read_u32() };
                    let elements = self
                        .stack
                        .split_off(self.stack.len() - length as usize);
                    self.stack.push(Value::Array(Arc::new(elements)));
                }

                bc::CALL_FUNCTION => {
                    unsafe { self.run_call_function().await? };
                }
                bc::CALL_METHOD => {
                    unsafe { self.run_call_method().await? };
                }

                bc::POP_JUMP_IF_FALSE => {
                    let offset = unsafe { self.read_u32() };
                    let value = unsafe { self.pop_stack_top_unchecked() };

                    if let Value::Bool(false) = value {
                        self.pc = offset as usize;
                    }
                }
                bc::JUMP_IF_FALSE => {
                    let offset = unsafe { self.read_u32() };
                    unsafe {
                        if matches!(self.get_stack_top_unchecked(), Value::Bool(false)) {
                            self.pc = offset as usize;
                        }
                    }
                }
                bc::POP_JUMP_IF_TRUE => {
                    let offset = unsafe { self.read_u32() };
                    let value = unsafe { self.pop_stack_top_unchecked() };

                    if let Value::Bool(true) = value {
                        self.pc = offset as usize;
                    }
                }
                bc::JUMP_IF_TRUE => {
                    let offset = unsafe { self.read_u32() };
                    if self.stack.is_empty() {
                        return Err(Error::Internal(
                            "Stack underflow: no value for DupAndJumpIfTrue".to_string(),
                        ));
                    }
                    unsafe {
                        if matches!(self.get_stack_top_unchecked(), Value::Bool(true)) {
                            self.pc = offset as usize;
                        }
                    }
                }
                bc::JUMP => {
                    let offset = unsafe { self.read_u32() };
                    self.pc = offset as usize;
                }

                bc::DUP => {
                    let value = unsafe { self.get_stack_top_unchecked().clone() };
                    self.stack.push(value);
                }

                bc::GT => {
                    let (left, right) = unsafe { self.pop_stack_top_two_unchecked() };
                    let result = left
                        .partial_cmp(&right)
                        .map(|ord| ord == std::cmp::Ordering::Greater)
                        .unwrap_or(false);
                    self.stack.push(Value::Bool(result));
                }
                bc::LT => {
                    let (left, right) = unsafe { self.pop_stack_top_two_unchecked() };
                    let result = left
                        .partial_cmp(&right)
                        .map(|ord| ord == std::cmp::Ordering::Less)
                        .unwrap_or(false);
                    self.stack.push(Value::Bool(result));
                }
                bc::GE => {
                    let (left, right) = unsafe { self.pop_stack_top_two_unchecked() };
                    let result = left
                        .partial_cmp(&right)
                        .map(|ord| {
                            ord == std::cmp::Ordering::Greater || ord == std::cmp::Ordering::Equal
                        })
                        .unwrap_or(false);
                    self.stack.push(Value::Bool(result));
                }
                bc::LE => {
                    let (left, right) = unsafe { self.pop_stack_top_two_unchecked() };
                    let result = left
                        .partial_cmp(&right)
                        .map(|ord| {
                            ord == std::cmp::Ordering::Less || ord == std::cmp::Ordering::Equal
                        })
                        .unwrap_or(false);
                    self.stack.push(Value::Bool(result));
                }
                bc::EQ => {
                    let (left, right) = unsafe { self.pop_stack_top_two_unchecked() };
                    self.stack.push(Value::Bool(left == right));
                }
                bc::NE => {
                    let (left, right) = unsafe { self.pop_stack_top_two_unchecked() };
                    self.stack.push(Value::Bool(left != right));
                }
                bc::IN => {
                    let right = unsafe { self.pop_stack_top_unchecked() };
                    let left = unsafe { self.pop_stack_top_unchecked() };

                    let result = match right {
                        Value::Array(arr) => arr.contains(&left),
                        _ => false,
                    };
                    self.stack.push(Value::Bool(result));
                }
                bc::NOT => {
                    let value = unsafe { self.pop_stack_top_unchecked() };
                    let result = match value {
                        Value::Bool(b) => !b,
                        _ => false,
                    };
                    self.stack.push(Value::Bool(result));
                }
                bc::RETURN => {
                    return Ok(unsafe { self.pop_stack_top_unchecked() });
                }

                _ => {
                    unreachable!("Invalid opcode: {}", opcode);
                }
            }
        }

        unreachable!("Execution ended without Return");
    }

    #[inline(always)]
    async unsafe fn run_call_function(&mut self) -> Result<(), Error> {
        let function_index = unsafe { self.read_u32() };
        let argument_count = unsafe { self.read_u32() };

        let function = unsafe {
            self.bytecode
                .functions
                .get(function_index as usize)
                .unwrap_unchecked()
        };

        let args = unsafe { self.pop_stack_top_n_unchecked(argument_count as usize) };

        let func = self.env.get_function(&function.name)?;
        let result = func
            .call(FunctionContext {
                env: &self.env,
                args: &args,
            })
            .await?;

        self.stack.push(result);

        Ok(())
    }

    #[inline(always)]
    async unsafe fn run_call_method(&mut self) -> Result<(), Error> {
        let method_index = unsafe { self.read_u32() };
        let argument_count = unsafe { self.read_u32() };

        // Get the method information.
        let method = unsafe {
            self.bytecode
                .methods
                .get(method_index as usize)
                .unwrap_unchecked()
        };

        // Get the arguments.
        let args = unsafe { self.pop_stack_top_n_unchecked(argument_count as usize) };

        // Get the object.
        let obj = unsafe { self.pop_stack_top_unchecked() };

        // Call method by the type of the object.
        let method = self.env.get_method(&method.name, obj.typ())?;
        let result = method
            .call(MethodContext {
                env: &self.env,
                obj: &obj,
                args: &args,
            })
            .await?;

        self.stack.push(result);

        Ok(())
    }

    /// Read a u32 from the bytecode.  And advance the pc by 4.
    ///
    /// # Safety
    ///
    /// Caller need make sure there are at least 4 bytes left.
    #[inline(always)]
    unsafe fn read_u32(&mut self) -> u32 {
        debug_assert!(self.pc + 4 <= self.bytecode.code.len(), "pc out of bounds");

        let bytes = unsafe { *((&self.bytecode.code[self.pc]) as *const u8 as *const [u8; 4]) };
        self.pc += 4;
        u32::from_le_bytes(bytes)
    }

    /// Read a i64 from the bytecode.  And advance the pc by 8.
    ///
    /// # Safety
    ///
    /// Caller need make sure there are at least 8 bytes left.
    #[inline(always)]
    unsafe fn read_i64(&mut self) -> i64 {
        debug_assert!(self.pc + 8 <= self.bytecode.code.len(), "pc out of bounds");

        let bytes = unsafe { *((&self.bytecode.code[self.pc]) as *const u8 as *const [u8; 8]) };
        self.pc += 8;
        i64::from_le_bytes(bytes)
    }

    /// Read a f64 from the bytecode.  And advance the pc by 8.
    ///
    /// # Safety
    ///
    /// Caller need make sure there are at least 8 bytes left.
    #[inline(always)]
    unsafe fn read_f64(&mut self) -> f64 {
        debug_assert!(self.pc + 8 <= self.bytecode.code.len(), "pc out of bounds");

        let bytes = unsafe { *((&self.bytecode.code[self.pc]) as *const u8 as *const [u8; 8]) };
        self.pc += 8;
        f64::from_le_bytes(bytes)
    }

    /// Read a bool from the bytecode.  And advance the pc by 1.
    ///
    /// # Safety
    ///
    /// Caller need make sure there are at least 1 byte left.
    #[inline(always)]
    unsafe fn read_bool(&mut self) -> bool {
        debug_assert!(self.pc < self.bytecode.code.len(), "pc out of bounds");

        let value = unsafe { *((&self.bytecode.code[self.pc]) as *const u8) };
        self.pc += 1;
        value != 0
    }

    /// Get the top value of the stack without checking the bounds.
    ///
    /// # Safety
    ///
    /// Caller need make sure the stack is not empty.
    #[inline(always)]
    unsafe fn get_stack_top_unchecked(&self) -> &Value {
        debug_assert!(!self.stack.is_empty(), "stack is empty");

        unsafe { self.stack.get_unchecked(self.stack.len() - 1) }
    }

    /// Pop the top value of the stack without checking the bounds.
    ///
    /// # Safety
    ///
    /// Caller need make sure the stack is not empty.
    #[inline(always)]
    unsafe fn pop_stack_top_unchecked(&mut self) -> Value {
        debug_assert!(!self.stack.is_empty(), "stack is empty");

        let len = self.stack.len();
        let value = unsafe { std::ptr::read(self.stack.get_unchecked(len - 1)) };
        unsafe { self.stack.set_len(len - 1) };
        value
    }

    /// Pop two values from the stack without checking the bounds.
    ///
    /// # Safety
    ///
    /// Caller need make sure the stack has at least 2 values.
    #[inline(always)]
    unsafe fn pop_stack_top_two_unchecked(&mut self) -> (Value, Value) {
        debug_assert!(self.stack.len() >= 2, "stack has less than 2 values");

        let len = self.stack.len();
        let top_first = unsafe { std::ptr::read(self.stack.get_unchecked(len - 1)) };
        let top_second = unsafe { std::ptr::read(self.stack.get_unchecked(len - 2)) };
        unsafe { self.stack.set_len(len - 2) };
        (top_second, top_first)
    }

    /// Pop n values from the stack without checking the bounds.
    ///
    /// # Safety
    ///
    /// Caller need make sure the stack has at least n values.
    #[inline(always)]
    unsafe fn pop_stack_top_n_unchecked(&mut self, n: usize) -> Vec<Value> {
        debug_assert!(self.stack.len() >= n, "stack has less than n values");

        let len = self.stack.len();
        self.stack.split_off(len - n)
    }
}
