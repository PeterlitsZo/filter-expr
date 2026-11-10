use std::sync::Arc;

use filter_expr::{FilterExpr, LanguageVersion};

use crate::{asm_codegen::AsmCodegen, bc::Bytecode, bc_codegen::BytecodeCodegen};

/// Compiled bytecode, reusable across evaluators.
#[derive(Clone)]
pub struct CompiledFilterExpr {
    bytecode: Option<Arc<Bytecode>>,
}

impl CompiledFilterExpr {
    /// Compile an expression for repeated execution.
    pub fn compile(filter: &FilterExpr) -> Self {
        match filter.language_version() {
            LanguageVersion::V1 => {}
        }
        let bytecode = filter.expr().map(|_| {
            let asm = AsmCodegen::new(filter).codegen();
            Arc::new(BytecodeCodegen::new().codegen(asm))
        });
        Self { bytecode }
    }

    pub(super) fn bytecode(&self) -> Option<&Bytecode> {
        self.bytecode.as_deref()
    }
}
