use std::sync::{Arc, Mutex};

use crate::{
    FilterExprEvalerCache,
    bc::{self, Bytecode},
    ctx::Context,
    error::Error,
    value::{Value, ValueType},
};

/// A runner for executing bytecode.
pub(crate) struct BytecodeRunner<'a> {
    /// The bytecode to run.
    bytecode: &'a Bytecode,
    /// The evaler cache.
    evaler_cache: Arc<Mutex<FilterExprEvalerCache>>,

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
    pub(crate) fn new(
        bytecode: &'a Bytecode,
        evaler_cache: Arc<Mutex<FilterExprEvalerCache>>,
    ) -> Self {
        Self {
            bytecode,
            evaler_cache,

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

                    let elements: Vec<Value> = self
                        .stack
                        .drain(self.stack.len() - length as usize..)
                        .collect();
                    self.stack.push(Value::Array(Arc::new(elements)));
                }

                bc::CALL_FUNCTION => {
                    unsafe { self.run_call_function(ctx).await? };
                }
                bc::CALL_METHOD => {
                    unsafe { self.run_call_method()? };
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
    async unsafe fn run_call_function(&mut self, ctx: &dyn Context) -> Result<(), Error> {
        let function_index = unsafe { self.read_u32() };
        let argument_count = unsafe { self.read_u32() };

        let function = unsafe {
            self.bytecode
                .functions
                .get(function_index as usize)
                .unwrap_unchecked()
        };

        let args = unsafe { self.pop_stack_top_n_unchecked(argument_count as usize) };

        if let Some(expr_fn) = ctx.get_fn(&function.name) {
            let result = expr_fn.call(crate::ctx::ExprFnContext { args }).await?;
            self.stack.push(result);
        } else {
            match function.name.as_str() {
                "matches" => unsafe { self.run_call_function_matches(&args)? },
                "type" => unsafe { self.run_call_function_type(&args)? },
                _ => {
                    return Err(Error::NoSuchFunction {
                        function: function.name.clone(),
                    });
                }
            }
        }

        Ok(())
    }

    unsafe fn run_call_function_matches(&mut self, args: &[Value]) -> Result<(), Error> {
        if args.len() != 2 {
            return Err(Error::InvalidArgumentCountForFunction {
                function: "matches".to_string(),
                expected: 2,
                got: args.len(),
            });
        }

        let (text, pattern) = (&args[0], &args[1]);
        let text = match text {
            Value::Str(s) => s,
            _ => {
                return Err(Error::InvalidArgumentTypeForFunction {
                    function: "matches".to_string(),
                    index: 0,
                    expected: ValueType::Str,
                    got: text.typ(),
                });
            }
        };
        let pattern = match pattern {
            Value::Str(s) => s,
            _ => {
                return Err(Error::InvalidArgumentTypeForFunction {
                    function: "matches".to_string(),
                    index: 1,
                    expected: ValueType::Str,
                    got: pattern.typ(),
                });
            }
        };

        let cache = self
            .evaler_cache
            .lock()
            .map_err(|e| Error::Internal(format!("failed to lock evaler cache: {e}")))?;
        let pattern_regex = cache.cached_regex.get(pattern.as_str());
        drop(cache);
        let matches = match pattern_regex {
            Some(pattern) => pattern.is_match(text),
            None => {
                let pattern = regex::Regex::new(pattern.as_str());
                let pattern = match pattern {
                    Ok(pattern) => pattern,
                    Err(e) => {
                        return Err(Error::Internal(format!("failed to compile regex: {e}")));
                    }
                };
                let pattern_arc = Arc::new(pattern.clone());
                let cache = self
                    .evaler_cache
                    .lock()
                    .map_err(|e| Error::Internal(format!("failed to lock evaler cache: {e}")))?;
                cache
                    .cached_regex
                    .insert(pattern.as_str().to_string(), pattern_arc.clone());
                pattern_arc.is_match(text)
            }
        };

        self.stack.push(Value::Bool(matches));

        Ok(())
    }

    unsafe fn run_call_function_type(&mut self, args: &[Value]) -> Result<(), Error> {
        if args.len() != 1 {
            return Err(Error::InvalidArgumentCountForFunction {
                function: "type".to_string(),
                expected: 1,
                got: args.len(),
            });
        }

        let result = Value::str(match &args[0].typ() {
            ValueType::Str => "str",
            ValueType::I64 => "i64",
            ValueType::F64 => "f64",
            ValueType::Bool => "bool",
            ValueType::Null => "null",
            ValueType::Array => "array",
        });
        self.stack.push(result);
        Ok(())
    }

    #[inline(always)]
    unsafe fn run_call_method(&mut self) -> Result<(), Error> {
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
        let result = match obj {
            Value::Str(s) => self.run_call_method_str(s, &method.name, &args)?,
            _ => {
                return Err(Error::NoSuchMethod {
                    method: method.name.clone(),
                    obj_type: obj.typ(),
                });
            }
        };

        self.stack.push(result);

        Ok(())
    }

    #[inline(always)]
    fn run_call_method_str(
        &mut self,
        s: Arc<String>,
        method: &str,
        args: &[Value],
    ) -> Result<Value, Error> {
        match method {
            "to_uppercase" => {
                if !args.is_empty() {
                    return Err(Error::InvalidArgumentCountForMethod {
                        method: method.to_string(),
                        expected: 0,
                        got: args.len(),
                    });
                }
                Ok(Value::Str(Arc::new(s.to_uppercase())))
            }
            "to_lowercase" => {
                if !args.is_empty() {
                    return Err(Error::InvalidArgumentCountForMethod {
                        method: method.to_string(),
                        expected: 0,
                        got: args.len(),
                    });
                }
                Ok(Value::Str(Arc::new(s.to_lowercase())))
            }
            "contains" => {
                if args.len() != 1 {
                    return Err(Error::InvalidArgumentCountForMethod {
                        method: method.to_string(),
                        expected: 1,
                        got: args.len(),
                    });
                }

                let arg = match &args[0] {
                    Value::Str(s) => s,
                    _ => {
                        return Err(Error::InvalidArgumentTypeForMethod {
                            method: method.to_string(),
                            index: 0,
                            expected: ValueType::Str,
                            got: args[0].typ(),
                        });
                    }
                };
                Ok(Value::Bool(s.contains(arg.as_str())))
            }
            "starts_with" => {
                if args.len() != 1 {
                    return Err(Error::InvalidArgumentCountForMethod {
                        method: method.to_string(),
                        expected: 1,
                        got: args.len(),
                    });
                }
                let arg = match &args[0] {
                    Value::Str(s) => s,
                    _ => {
                        return Err(Error::InvalidArgumentTypeForMethod {
                            method: method.to_string(),
                            index: 0,
                            expected: ValueType::Str,
                            got: args[0].typ(),
                        });
                    }
                };
                Ok(Value::Bool(s.starts_with(arg.as_str())))
            }
            "ends_with" => {
                if args.len() != 1 {
                    return Err(Error::InvalidArgumentCountForMethod {
                        method: method.to_string(),
                        expected: 1,
                        got: args.len(),
                    });
                }
                let arg = match &args[0] {
                    Value::Str(s) => s,
                    _ => {
                        return Err(Error::InvalidArgumentTypeForMethod {
                            method: method.to_string(),
                            index: 0,
                            expected: ValueType::Str,
                            got: args[0].typ(),
                        });
                    }
                };
                Ok(Value::Bool(s.ends_with(arg.as_str())))
            }
            _ => {
                Err(Error::NoSuchMethod {
                    method: method.to_string(),
                    obj_type: ValueType::Str,
                })
            }
        }
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
        let value = unsafe { self.stack.get_unchecked(len - 1).clone() };
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
        let top_first = unsafe { self.stack.get_unchecked(len - 1).clone() };
        let top_second = unsafe { self.stack.get_unchecked(len - 2).clone() };
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
        let values = unsafe { self.stack.get_unchecked(len - n..) }.to_vec();
        unsafe { self.stack.set_len(len - n) };
        values
    }
}
