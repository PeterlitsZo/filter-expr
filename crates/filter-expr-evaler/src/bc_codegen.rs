use std::collections::HashMap;

use crate::{
    asm::{Asm, AsmInstruction},
    bc::{Bytecode, BytecodeContextVar, BytecodeFunction, BytecodeMethod, BytecodeOpcode},
};

pub(crate) struct BytecodeCodegen {
    code: Vec<u8>,

    labels_map: HashMap<u32, usize>,
}

impl BytecodeCodegen {
    pub(crate) fn new() -> Self {
        Self {
            code: Vec::new(),
            labels_map: HashMap::new(),
        }
    }

    pub(crate) fn codegen(mut self, asm: Asm) -> Bytecode {
        let context_vars = asm
            .context_vars
            .into_iter()
            .map(|context_var| BytecodeContextVar { name: context_var.name })
            .collect();
        let functions = asm
            .functions
            .into_iter()
            .map(|function| BytecodeFunction {
                name: function.name,
            })
            .collect();
        let methods = asm
            .methods
            .into_iter()
            .map(|method| BytecodeMethod { name: method.name })
            .collect();

        // Calculate the offset of each label.
        let mut ins_offset = 0;
        for ins in &asm.instructions {
            use AsmInstruction as Ins;
            use BytecodeOpcode as Op;

            match ins {
                Ins::Label(label_index) => {
                    self.labels_map.insert(*label_index, ins_offset);
                }

                Ins::LoadConst(..) => ins_offset += Op::LoadConst.op_len(),
                Ins::LoadContext(..) => ins_offset += Op::LoadContext.op_len(),
                Ins::LoadLocal(..) => ins_offset += Op::LoadLocal.op_len(),
                Ins::StoreLocal(..) => ins_offset += Op::StoreLocal.op_len(),
                Ins::GetField(..) => ins_offset += Op::GetField.op_len(),

                Ins::LoadI64(..) => ins_offset += Op::LoadI64.op_len(),
                Ins::LoadF64(..) => ins_offset += Op::LoadF64.op_len(),
                Ins::LoadBool(..) => ins_offset += Op::LoadBool.op_len(),
                Ins::LoadNull => ins_offset += Op::LoadNull.op_len(),

                Ins::BuildArray(..) => ins_offset += Op::BuildArray.op_len(),
                Ins::BuildObject(..) => ins_offset += Op::BuildObject.op_len(),

                Ins::CallFunction(..) => ins_offset += Op::CallFunction.op_len(),
                Ins::CallMethod(..) => ins_offset += Op::CallMethod.op_len(),

                Ins::PopJumpIfFalse(..) => ins_offset += Op::JumpIfFalse.op_len(),
                Ins::PopJumpIfTrue(..) => ins_offset += Op::JumpIfTrue.op_len(),
                Ins::Jump(..) => ins_offset += Op::Jump.op_len(),

                Ins::Dup => ins_offset += Op::Dup.op_len(),
                Ins::JumpIfFalse(..) => ins_offset += Op::JumpIfFalse.op_len(),
                Ins::JumpIfTrue(..) => ins_offset += Op::JumpIfTrue.op_len(),

                Ins::Add => ins_offset += Op::Add.op_len(),
                Ins::Sub => ins_offset += Op::Sub.op_len(),
                Ins::Mul => ins_offset += Op::Mul.op_len(),
                Ins::Div => ins_offset += Op::Div.op_len(),
                Ins::Rem => ins_offset += Op::Rem.op_len(),
                Ins::Pos => ins_offset += Op::Pos.op_len(),
                Ins::Neg => ins_offset += Op::Neg.op_len(),
                Ins::Gt => ins_offset += Op::Gt.op_len(),
                Ins::Lt => ins_offset += Op::Lt.op_len(),
                Ins::Ge => ins_offset += Op::Ge.op_len(),
                Ins::Le => ins_offset += Op::Le.op_len(),
                Ins::Eq => ins_offset += Op::Eq.op_len(),
                Ins::Ne => ins_offset += Op::Ne.op_len(),
                Ins::In => ins_offset += Op::In.op_len(),

                Ins::Not => ins_offset += Op::Not.op_len(),

                Ins::Return => ins_offset += Op::Return.op_len(),
            }
        }

        for ins in asm.instructions {
            self.codegen_instruction(ins);
        }

        Bytecode {
            local_count: asm.local_count,
            code: self.code,
            context_vars,
            functions,
            methods,
            const_pool: asm.const_pool,
        }
    }

    fn push_u8(&mut self, value: u8) {
        self.code.push(value);
    }

    fn push_u32(&mut self, value: u32) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }

    fn push_i64(&mut self, value: i64) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }

    fn push_f64(&mut self, value: f64) {
        self.code.extend_from_slice(&value.to_le_bytes());
    }

    fn push_bool(&mut self, value: bool) {
        self.code.push(value as u8);
    }

    fn codegen_instruction(&mut self, instruction: AsmInstruction) {
        match instruction {
            AsmInstruction::Label(_label_index) => {
                // Do nothing - label offset already recorded in first pass
            }

            AsmInstruction::GetField(index) => {
                self.push_u8(BytecodeOpcode::GetField.into_u8());
                self.push_u32(index);
            }
            AsmInstruction::LoadConst(index) => {
                self.push_u8(BytecodeOpcode::LoadConst.into_u8());
                self.push_u32(index);
            }
            AsmInstruction::LoadContext(index) => {
                self.push_u8(BytecodeOpcode::LoadContext.into_u8());
                self.push_u32(index);
            }
            AsmInstruction::LoadLocal(index) => {
                self.push_u8(BytecodeOpcode::LoadLocal.into_u8());
                self.push_u32(index);
            }
            AsmInstruction::StoreLocal(index) => {
                self.push_u8(BytecodeOpcode::StoreLocal.into_u8());
                self.push_u32(index);
            }

            AsmInstruction::LoadI64(value) => {
                self.push_u8(BytecodeOpcode::LoadI64.into_u8());
                self.push_i64(value);
            }
            AsmInstruction::LoadF64(value) => {
                self.push_u8(BytecodeOpcode::LoadF64.into_u8());
                self.push_f64(value);
            }
            AsmInstruction::LoadBool(value) => {
                self.push_u8(BytecodeOpcode::LoadBool.into_u8());
                self.push_bool(value);
            }
            AsmInstruction::LoadNull => {
                self.push_u8(BytecodeOpcode::LoadNull.into_u8());
            }

            AsmInstruction::BuildObject(length) => {
                self.push_u8(BytecodeOpcode::BuildObject.into_u8());
                self.push_u32(length);
            }
            AsmInstruction::BuildArray(length) => {
                self.push_u8(BytecodeOpcode::BuildArray.into_u8());
                self.push_u32(length);
            }

            AsmInstruction::CallFunction(function_index, argument_count) => {
                self.push_u8(BytecodeOpcode::CallFunction.into_u8());
                self.push_u32(function_index);
                self.push_u32(argument_count);
            }
            AsmInstruction::CallMethod(method_index, argument_count) => {
                self.push_u8(BytecodeOpcode::CallMethod.into_u8());
                self.push_u32(method_index);
                self.push_u32(argument_count);
            }

            AsmInstruction::PopJumpIfFalse(label_index) => {
                self.push_u8(BytecodeOpcode::PopJumpIfFalse.into_u8());
                let offset = *self.labels_map.get(&label_index).expect("Label not found");
                self.push_u32(offset as u32);
            }
            AsmInstruction::JumpIfFalse(label_index) => {
                self.push_u8(BytecodeOpcode::JumpIfFalse.into_u8());
                let offset = *self.labels_map.get(&label_index).expect("Label not found");
                self.push_u32(offset as u32);
            }
            AsmInstruction::PopJumpIfTrue(label_index) => {
                self.push_u8(BytecodeOpcode::PopJumpIfTrue.into_u8());
                let offset = *self.labels_map.get(&label_index).expect("Label not found");
                self.push_u32(offset as u32);
            }
            AsmInstruction::JumpIfTrue(label_index) => {
                self.push_u8(BytecodeOpcode::JumpIfTrue.into_u8());
                let offset = *self.labels_map.get(&label_index).expect("Label not found");
                self.push_u32(offset as u32);
            }
            AsmInstruction::Jump(label_index) => {
                self.push_u8(BytecodeOpcode::Jump.into_u8());
                let offset = *self.labels_map.get(&label_index).expect("Label not found");
                self.push_u32(offset as u32);
            }

            AsmInstruction::Dup => {
                self.push_u8(BytecodeOpcode::Dup.into_u8());
            }

            AsmInstruction::Add => {
                self.push_u8(BytecodeOpcode::Add.into_u8());
            }
            AsmInstruction::Sub => {
                self.push_u8(BytecodeOpcode::Sub.into_u8());
            }
            AsmInstruction::Mul => {
                self.push_u8(BytecodeOpcode::Mul.into_u8());
            }
            AsmInstruction::Div => {
                self.push_u8(BytecodeOpcode::Div.into_u8());
            }
            AsmInstruction::Rem => {
                self.push_u8(BytecodeOpcode::Rem.into_u8());
            }
            AsmInstruction::Pos => {
                self.push_u8(BytecodeOpcode::Pos.into_u8());
            }
            AsmInstruction::Neg => {
                self.push_u8(BytecodeOpcode::Neg.into_u8());
            }
            AsmInstruction::Gt => {
                self.push_u8(BytecodeOpcode::Gt.into_u8());
            }
            AsmInstruction::Lt => {
                self.push_u8(BytecodeOpcode::Lt.into_u8());
            }
            AsmInstruction::Ge => {
                self.push_u8(BytecodeOpcode::Ge.into_u8());
            }
            AsmInstruction::Le => {
                self.push_u8(BytecodeOpcode::Le.into_u8());
            }
            AsmInstruction::Eq => {
                self.push_u8(BytecodeOpcode::Eq.into_u8());
            }
            AsmInstruction::Ne => {
                self.push_u8(BytecodeOpcode::Ne.into_u8());
            }
            AsmInstruction::In => {
                self.push_u8(BytecodeOpcode::In.into_u8());
            }

            AsmInstruction::Not => {
                self.push_u8(BytecodeOpcode::Not.into_u8());
            }

            AsmInstruction::Return => {
                self.push_u8(BytecodeOpcode::Return.into_u8());
            }
        }
    }
}
