use std::{collections::HashMap, sync::Arc};

use filter_expr::Expr;

use crate::asm::{Asm, AsmFunction, AsmInstruction, AsmLabel, AsmLocal, AsmMethod};

pub(crate) struct AsmCodegen {
    locals: Vec<AsmLocal>,
    functions: Vec<AsmFunction>,
    methods: Vec<AsmMethod>,
    labels: Vec<AsmLabel>,
    str_pool: Vec<Arc<String>>,
    instructions: Vec<AsmInstruction>,

    locals_map: HashMap<String, u32>,
    str_pool_map: HashMap<String, u32>,
    functions_map: HashMap<String, u32>,
    methods_map: HashMap<String, u32>,
    label_counter: u32,
}

impl AsmCodegen {
    pub(crate) fn new() -> Self {
        Self {
            locals: Vec::new(),
            functions: Vec::new(),
            methods: Vec::new(),
            labels: Vec::new(),
            str_pool: Vec::new(),
            instructions: Vec::new(),

            locals_map: HashMap::new(),
            str_pool_map: HashMap::new(),
            functions_map: HashMap::new(),
            methods_map: HashMap::new(),
            label_counter: 0,
        }
    }

    fn new_label(&mut self) -> u32 {
        let label_id = self.label_counter;
        self.label_counter += 1;
        self.labels.push(AsmLabel::new(format!("label_{label_id}")));
        label_id
    }

    pub(crate) fn codegen(mut self, expr: &Expr) -> Asm {
        self.codegen_expr(expr);

        self.instructions.push(AsmInstruction::Return);

        Asm {
            locals: self.locals,
            functions: self.functions,
            methods: self.methods,
            labels: self.labels,
            str_pool: self.str_pool,
            instructions: self.instructions,
        }
    }

    fn codegen_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Field(field) => self.codegen_field(field),
            Expr::FieldAccess(obj, field) => todo!(),
            Expr::Str(value) => self.codegen_str(value),
            Expr::I64(value) => self.codegen_i64(*value),
            Expr::F64(value) => self.codegen_f64(*value),
            Expr::Bool(value) => self.codegen_bool(*value),
            Expr::Null => self.codegen_null(),
            Expr::Array(elements) => self.codegen_array(elements),
            Expr::Gt(left, right) => self.codegen_gt(left, right),
            Expr::Lt(left, right) => self.codegen_lt(left, right),
            Expr::Ge(left, right) => self.codegen_ge(left, right),
            Expr::Le(left, right) => self.codegen_le(left, right),
            Expr::Eq(left, right) => self.codegen_eq(left, right),
            Expr::Ne(left, right) => self.codegen_ne(left, right),
            Expr::In(left, right) => self.codegen_in(left, right),
            Expr::And(exprs) => self.codegen_and(exprs),
            Expr::Or(exprs) => self.codegen_or(exprs),
            Expr::Not(expr) => self.codegen_not(expr),
            Expr::FuncCall(func_name, args) => self.codegen_func_call(func_name, args),
            Expr::MethodCall(method_name, obj, args) => {
                self.codegen_method_call(method_name, obj, args)
            }
        }
    }

    pub(crate) fn codegen_field(&mut self, field: &str) {
        if let Some(local) = self.locals_map.get(field) {
            self.instructions.push(AsmInstruction::LoadLocal(*local));
        } else {
            let local = AsmLocal::new(field.to_string());
            self.locals_map
                .insert(field.to_string(), self.locals.len() as u32);
            self.instructions
                .push(AsmInstruction::LoadLocal(self.locals.len() as u32));
            self.locals.push(local);
        }
    }

    pub(crate) fn codegen_str(&mut self, value: &str) {
        if let Some(constant) = self.str_pool_map.get(value) {
            self.instructions
                .push(AsmInstruction::LoadString(*constant));
        } else {
            let s = value.to_string();
            self.str_pool_map
                .insert(s.clone(), self.str_pool.len() as u32);
            self.instructions
                .push(AsmInstruction::LoadString(self.str_pool.len() as u32));
            self.str_pool.push(Arc::new(s));
        }
    }

    pub(crate) fn codegen_i64(&mut self, value: i64) {
        self.instructions.push(AsmInstruction::LoadI64(value));
    }

    pub(crate) fn codegen_f64(&mut self, value: f64) {
        self.instructions.push(AsmInstruction::LoadF64(value));
    }

    pub(crate) fn codegen_bool(&mut self, value: bool) {
        self.instructions.push(AsmInstruction::LoadBool(value));
    }

    pub(crate) fn codegen_null(&mut self) {
        self.instructions.push(AsmInstruction::LoadNull);
    }

    pub(crate) fn codegen_array(&mut self, elements: &[Expr]) {
        // Codegen all elements first
        for element in elements {
            self.codegen_expr(element);
        }
        // Then build the array
        self.instructions
            .push(AsmInstruction::BuildArray(elements.len() as u32));
    }

    pub(crate) fn codegen_gt(&mut self, left: &Expr, right: &Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Gt);
    }

    pub(crate) fn codegen_lt(&mut self, left: &Expr, right: &Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Lt);
    }

    pub(crate) fn codegen_ge(&mut self, left: &Expr, right: &Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Ge);
    }

    pub(crate) fn codegen_le(&mut self, left: &Expr, right: &Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Le);
    }

    pub(crate) fn codegen_eq(&mut self, left: &Expr, right: &Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Eq);
    }

    pub(crate) fn codegen_ne(&mut self, left: &Expr, right: &Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Ne);
    }

    pub(crate) fn codegen_in(&mut self, left: &Expr, right: &Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::In);
    }

    pub(crate) fn codegen_and(&mut self, exprs: &[Expr]) {
        if exprs.is_empty() {
            self.codegen_bool(true);
            return;
        }

        if exprs.len() == 1 {
            self.codegen_expr(&exprs[0]);
            return;
        }

        let end_label = self.new_label();

        // Evaluate each expression.
        for (i, expr) in exprs.iter().enumerate() {
            self.codegen_expr(expr);

            // If not the last expression, check if false and jump to end.
            if i < exprs.len() - 1 {
                self.instructions
                    .push(AsmInstruction::JumpIfFalse(end_label));
            }
        }

        // Insert Label instruction at the end target.
        self.instructions.push(AsmInstruction::Label(end_label));
    }

    pub(crate) fn codegen_or(&mut self, exprs: &[Expr]) {
        if exprs.is_empty() {
            self.codegen_bool(false);
            return;
        }

        if exprs.len() == 1 {
            self.codegen_expr(&exprs[0]);
            return;
        }

        let end_label = self.new_label();

        // Evaluate each expression.
        for (i, expr) in exprs.iter().enumerate() {
            self.codegen_expr(expr);

            // If not the last expression, check if true and jump to end.
            if i < exprs.len() - 1 {
                self.instructions
                    .push(AsmInstruction::JumpIfTrue(end_label));
            }
        }

        // Insert Label instruction at the end target.
        self.instructions.push(AsmInstruction::Label(end_label));
    }

    pub(crate) fn codegen_not(&mut self, expr: &Expr) {
        self.codegen_expr(expr);
        self.instructions.push(AsmInstruction::Not);
    }

    pub(crate) fn codegen_func_call(&mut self, func_name: &str, args: &[Expr]) {
        for arg in args {
            self.codegen_expr(arg);
        }

        // Get or create function index.
        let func_index = if let Some(&index) = self.functions_map.get(func_name) {
            index
        } else {
            let index = self.functions.len() as u32;
            self.functions_map.insert(func_name.to_string(), index);
            self.functions.push(AsmFunction::new(func_name.to_string()));
            index
        };

        // Call the function.
        self.instructions
            .push(AsmInstruction::CallFunction(func_index, args.len() as u32));
    }

    pub(crate) fn codegen_method_call(&mut self, method_name: &str, obj: &Expr, args: &[Expr]) {
        // Codegen the object first.
        self.codegen_expr(obj);

        // Codegen all arguments.
        for arg in args {
            self.codegen_expr(arg);
        }

        // Get or create method index.
        let method_index = if let Some(&index) = self.methods_map.get(method_name) {
            index
        } else {
            let index = self.methods.len() as u32;
            self.methods_map.insert(method_name.to_string(), index);
            self.methods.push(AsmMethod::new(method_name.to_string()));
            index
        };

        // Call the method.
        self.instructions
            .push(AsmInstruction::CallMethod(method_index, args.len() as u32));
    }
}

#[cfg(test)]
mod tests {
    use core::f64;
    use std::sync::Arc;

    use super::*;

    #[test]
    fn test_codegen_field() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Field("field".to_string()));
        assert_eq!(
            asm,
            Asm {
                locals: vec![AsmLocal::new("field")],
                instructions: vec![AsmInstruction::LoadLocal(0), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_str() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Str("str".to_string()));
        assert_eq!(
            asm,
            Asm {
                str_pool: vec![Arc::new("str".to_string())],
                instructions: vec![AsmInstruction::LoadString(0), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_i64() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::I64(42));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::LoadI64(42), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_f64() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::F64(f64::consts::PI));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![
                    AsmInstruction::LoadF64(f64::consts::PI),
                    AsmInstruction::Return
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_bool() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Bool(true));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::LoadBool(true), AsmInstruction::Return],
                ..Default::default()
            }
        );

        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Bool(false));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::LoadBool(false), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_null() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Null);
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::LoadNull, AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_array() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Array(vec![Expr::I64(1), Expr::I64(2), Expr::I64(3)]));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![
                    AsmInstruction::LoadI64(1),
                    AsmInstruction::LoadI64(2),
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::BuildArray(3),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_empty_array() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Array(vec![]));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::BuildArray(0), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_gt() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Gt(Box::new(Expr::I64(5)), Box::new(Expr::I64(3))));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![
                    AsmInstruction::LoadI64(5),
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::Gt,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_lt() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Lt(Box::new(Expr::I64(3)), Box::new(Expr::I64(5))));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::LoadI64(5),
                    AsmInstruction::Lt,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_ge() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Ge(Box::new(Expr::I64(5)), Box::new(Expr::I64(5))));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![
                    AsmInstruction::LoadI64(5),
                    AsmInstruction::LoadI64(5),
                    AsmInstruction::Ge,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_le() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Le(Box::new(Expr::I64(3)), Box::new(Expr::I64(5))));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::LoadI64(5),
                    AsmInstruction::Le,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_eq() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Eq(
            Box::new(Expr::Str("hello".to_string())),
            Box::new(Expr::Str("hello".to_string())),
        ));
        // Same string constants are deduplicated
        assert_eq!(
            asm,
            Asm {
                str_pool: vec![Arc::new("hello".to_string())],
                instructions: vec![
                    AsmInstruction::LoadString(0),
                    AsmInstruction::LoadString(0),
                    AsmInstruction::Eq,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_eq_different_strings() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Eq(
            Box::new(Expr::Str("hello".to_string())),
            Box::new(Expr::Str("world".to_string())),
        ));
        assert_eq!(
            asm,
            Asm {
                str_pool: vec![Arc::new("hello".to_string()), Arc::new("world".to_string())],
                instructions: vec![
                    AsmInstruction::LoadString(0),
                    AsmInstruction::LoadString(1),
                    AsmInstruction::Eq,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_ne() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Ne(Box::new(Expr::I64(1)), Box::new(Expr::I64(2))));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![
                    AsmInstruction::LoadI64(1),
                    AsmInstruction::LoadI64(2),
                    AsmInstruction::Ne,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_in() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::In(
            Box::new(Expr::I64(2)),
            Box::new(Expr::Array(vec![Expr::I64(1), Expr::I64(2), Expr::I64(3)])),
        ));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![
                    AsmInstruction::LoadI64(2),
                    AsmInstruction::LoadI64(1),
                    AsmInstruction::LoadI64(2),
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::BuildArray(3),
                    AsmInstruction::In,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_and() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::And(vec![
            Expr::Bool(true),
            Expr::Bool(false),
            Expr::Bool(true),
        ]));
        assert_eq!(
            asm,
            Asm {
                labels: vec![AsmLabel::new("label_0")],
                instructions: vec![
                    AsmInstruction::LoadBool(true),
                    AsmInstruction::JumpIfFalse(0),
                    AsmInstruction::LoadBool(false),
                    AsmInstruction::JumpIfFalse(0),
                    AsmInstruction::LoadBool(true),
                    AsmInstruction::Label(0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_and_single() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::And(vec![Expr::Bool(true)]));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::LoadBool(true), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_and_empty() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::And(vec![]));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::LoadBool(true), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_or() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Or(vec![
            Expr::Bool(false),
            Expr::Bool(true),
            Expr::Bool(false),
        ]));
        assert_eq!(
            asm,
            Asm {
                labels: vec![AsmLabel::new("label_0")],
                instructions: vec![
                    AsmInstruction::LoadBool(false),
                    AsmInstruction::JumpIfTrue(0),
                    AsmInstruction::LoadBool(true),
                    AsmInstruction::JumpIfTrue(0),
                    AsmInstruction::LoadBool(false),
                    AsmInstruction::Label(0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_or_single() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Or(vec![Expr::Bool(false)]));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::LoadBool(false), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_or_empty() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Or(vec![]));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::LoadBool(false), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_not() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Not(Box::new(Expr::Bool(true))));
        assert_eq!(
            asm,
            Asm {
                instructions: vec![
                    AsmInstruction::LoadBool(true),
                    AsmInstruction::Not,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_complex() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::And(vec![
            Expr::Gt(Box::new(Expr::I64(1)), Box::new(Expr::I64(0))),
            Expr::Lt(Box::new(Expr::I64(2)), Box::new(Expr::I64(3))),
        ]));
        assert_eq!(
            asm,
            Asm {
                labels: vec![AsmLabel::new("label_0")],
                instructions: vec![
                    AsmInstruction::LoadI64(1),
                    AsmInstruction::LoadI64(0),
                    AsmInstruction::Gt,
                    AsmInstruction::JumpIfFalse(0),
                    AsmInstruction::LoadI64(2),
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::Lt,
                    AsmInstruction::Label(0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_and_short_circuit() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::And(vec![Expr::Bool(false), Expr::Bool(true)]));
        assert_eq!(
            asm,
            Asm {
                labels: vec![AsmLabel::new("label_0")],
                instructions: vec![
                    AsmInstruction::LoadBool(false),
                    AsmInstruction::JumpIfFalse(0),
                    AsmInstruction::LoadBool(true),
                    AsmInstruction::Label(0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_or_short_circuit() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Or(vec![Expr::Bool(true), Expr::Bool(false)]));
        assert_eq!(
            asm,
            Asm {
                labels: vec![AsmLabel::new("label_0")],
                instructions: vec![
                    AsmInstruction::LoadBool(true),
                    AsmInstruction::JumpIfTrue(0),
                    AsmInstruction::LoadBool(false),
                    AsmInstruction::Label(0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_func_call() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::FuncCall(
            "len".to_string(),
            vec![Expr::Str("hello".to_string())],
        ));
        assert_eq!(
            asm,
            Asm {
                functions: vec![AsmFunction::new("len")],
                str_pool: vec![Arc::new("hello".to_string())],
                instructions: vec![
                    AsmInstruction::LoadString(0),
                    AsmInstruction::CallFunction(0, 1),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_func_call_multiple_args() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::FuncCall(
            "substr".to_string(),
            vec![Expr::Str("hello".to_string()), Expr::I64(0), Expr::I64(3)],
        ));
        assert_eq!(
            asm,
            Asm {
                functions: vec![AsmFunction::new("substr")],
                str_pool: vec![Arc::new("hello".to_string())],
                instructions: vec![
                    AsmInstruction::LoadString(0),
                    AsmInstruction::LoadI64(0),
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::CallFunction(0, 3),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_func_call_no_args() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::FuncCall("now".to_string(), vec![]));
        assert_eq!(
            asm,
            Asm {
                functions: vec![AsmFunction::new("now")],
                instructions: vec![AsmInstruction::CallFunction(0, 0), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_func_call_same_function_twice() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::FuncCall(
            "len".to_string(),
            vec![Expr::FuncCall(
                "len".to_string(),
                vec![Expr::Str("test".to_string())],
            )],
        ));
        assert_eq!(
            asm,
            Asm {
                functions: vec![AsmFunction::new("len")],
                str_pool: vec![Arc::new("test".to_string())],
                instructions: vec![
                    AsmInstruction::LoadString(0),
                    AsmInstruction::CallFunction(0, 1),
                    AsmInstruction::CallFunction(0, 1),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_method_call() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::MethodCall(
            "toUpper".to_string(),
            Box::new(Expr::Str("hello".to_string())),
            vec![],
        ));
        assert_eq!(
            asm,
            Asm {
                methods: vec![AsmMethod::new("toUpper")],
                str_pool: vec![Arc::new("hello".to_string())],
                instructions: vec![
                    AsmInstruction::LoadString(0),
                    AsmInstruction::CallMethod(0, 0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_method_call_with_args() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::MethodCall(
            "substring".to_string(),
            Box::new(Expr::Str("hello".to_string())),
            vec![Expr::I64(0), Expr::I64(3)],
        ));
        assert_eq!(
            asm,
            Asm {
                methods: vec![AsmMethod::new("substring")],
                str_pool: vec![Arc::new("hello".to_string())],
                instructions: vec![
                    AsmInstruction::LoadString(0),
                    AsmInstruction::LoadI64(0),
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::CallMethod(0, 2),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_method_call_chained() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::MethodCall(
            "method2".to_string(),
            Box::new(Expr::MethodCall(
                "method1".to_string(),
                Box::new(Expr::Str("test".to_string())),
                vec![],
            )),
            vec![],
        ));
        assert_eq!(
            asm,
            Asm {
                methods: vec![AsmMethod::new("method1"), AsmMethod::new("method2")],
                str_pool: vec![Arc::new("test".to_string())],
                instructions: vec![
                    AsmInstruction::LoadString(0),
                    AsmInstruction::CallMethod(0, 0),
                    AsmInstruction::CallMethod(1, 0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_method_call_on_field() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::MethodCall(
            "toUpper".to_string(),
            Box::new(Expr::Field("name".to_string())),
            vec![],
        ));
        assert_eq!(
            asm,
            Asm {
                locals: vec![AsmLocal::new("name")],
                methods: vec![AsmMethod::new("toUpper")],
                instructions: vec![
                    AsmInstruction::LoadLocal(0),
                    AsmInstruction::CallMethod(0, 0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_func_call_in_comparison() {
        let asm_codegen = AsmCodegen::new();
        let asm = asm_codegen.codegen(&Expr::Gt(
            Box::new(Expr::FuncCall(
                "len".to_string(),
                vec![Expr::Str("hello".to_string())],
            )),
            Box::new(Expr::I64(3)),
        ));
        assert_eq!(
            asm,
            Asm {
                functions: vec![AsmFunction::new("len")],
                str_pool: vec![Arc::new("hello".to_string())],
                instructions: vec![
                    AsmInstruction::LoadString(0),
                    AsmInstruction::CallFunction(0, 1),
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::Gt,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }
}
