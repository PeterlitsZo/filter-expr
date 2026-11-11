use std::{
    collections::{BTreeMap, HashMap},
    sync::Arc,
};

use filter_expr::{Expr, ExprId, FilterExpr};

use crate::Value;
use crate::asm::{Asm, AsmContextVar, AsmFunction, AsmInstruction, AsmMethod};

pub(crate) struct AsmCodegen<'a> {
    filter: &'a FilterExpr,
    local_count: u32,
    context_vars: Vec<AsmContextVar>,
    functions: Vec<AsmFunction>,
    methods: Vec<AsmMethod>,
    const_pool: Vec<Value>,
    instructions: Vec<AsmInstruction>,

    locals_map: HashMap<&'a str, u32>,
    context_vars_map: HashMap<&'a str, u32>,
    str_const_map: HashMap<&'a str, u32>,
    functions_map: HashMap<&'a str, u32>,
    methods_map: HashMap<&'a str, u32>,
    label_counter: u32,
}

impl<'a> AsmCodegen<'a> {
    pub(crate) fn new(filter: &'a FilterExpr) -> Self {
        Self {
            filter,
            local_count: 0,
            context_vars: Vec::new(),
            functions: Vec::new(),
            methods: Vec::new(),
            const_pool: Vec::new(),
            instructions: Vec::new(),

            locals_map: HashMap::new(),
            context_vars_map: HashMap::new(),
            str_const_map: HashMap::new(),
            functions_map: HashMap::new(),
            methods_map: HashMap::new(),
            label_counter: 0,
        }
    }

    fn new_label(&mut self) -> u32 {
        let label_id = self.label_counter;
        self.label_counter += 1;
        label_id
    }

    pub(crate) fn codegen(mut self) -> Asm {
        if let Some(expr) = self.filter.expr() {
            self.codegen_expr(expr);
        }

        self.instructions.push(AsmInstruction::Return);

        Asm {
            local_count: self.local_count,
            context_vars: self.context_vars,
            functions: self.functions,
            methods: self.methods,
            const_pool: self.const_pool,
            instructions: self.instructions,
        }
    }

    fn node(&self, id: ExprId) -> &'a Expr {
        self.filter.node(id).unwrap()
    }

    fn codegen_expr(&mut self, expr: &'a Expr) {
        match expr {
            Expr::Field(field) => self.codegen_field(field),
            Expr::FieldAccess(obj, field) => self.codegen_field_access(self.node(*obj), field),
            Expr::Str(value) => self.codegen_str(value),
            Expr::I64(value) => self.codegen_i64(*value),
            Expr::BigInt(value) => {
                let index = self.const_pool.len() as u32;
                self.const_pool.push(Value::BigInt(value.clone()));
                self.instructions.push(AsmInstruction::LoadConst(index));
            }
            Expr::F64(value) => self.codegen_f64(*value),
            Expr::Bool(value) => self.codegen_bool(*value),
            Expr::Null => self.codegen_null(),
            Expr::Array(elements) => self.codegen_array(elements),
            Expr::Object(fields) => self.codegen_object(fields),
            Expr::Add(left, right) => {
                self.codegen_expr(self.node(*left));
                self.codegen_expr(self.node(*right));
                self.instructions.push(AsmInstruction::Add);
            }
            Expr::Sub(left, right) => {
                self.codegen_expr(self.node(*left));
                self.codegen_expr(self.node(*right));
                self.instructions.push(AsmInstruction::Sub);
            }
            Expr::Mul(left, right) => {
                self.codegen_expr(self.node(*left));
                self.codegen_expr(self.node(*right));
                self.instructions.push(AsmInstruction::Mul);
            }
            Expr::Div(left, right) => {
                self.codegen_expr(self.node(*left));
                self.codegen_expr(self.node(*right));
                self.instructions.push(AsmInstruction::Div);
            }
            Expr::Rem(left, right) => {
                self.codegen_expr(self.node(*left));
                self.codegen_expr(self.node(*right));
                self.instructions.push(AsmInstruction::Rem);
            }
            Expr::Pos(expr) => {
                self.codegen_expr(self.node(*expr));
                self.instructions.push(AsmInstruction::Pos);
            }
            Expr::Neg(expr) => {
                self.codegen_expr(self.node(*expr));
                self.instructions.push(AsmInstruction::Neg);
            }
            Expr::Gt(left, right) => self.codegen_gt(self.node(*left), self.node(*right)),
            Expr::Lt(left, right) => self.codegen_lt(self.node(*left), self.node(*right)),
            Expr::Ge(left, right) => self.codegen_ge(self.node(*left), self.node(*right)),
            Expr::Le(left, right) => self.codegen_le(self.node(*left), self.node(*right)),
            Expr::Eq(left, right) => self.codegen_eq(self.node(*left), self.node(*right)),
            Expr::Ne(left, right) => self.codegen_ne(self.node(*left), self.node(*right)),
            Expr::In(left, right) => self.codegen_in(self.node(*left), self.node(*right)),
            Expr::And(exprs) => self.codegen_and(exprs),
            Expr::Or(exprs) => self.codegen_or(exprs),
            Expr::Not(expr) => self.codegen_not(self.node(*expr)),
            Expr::FuncCall(func_name, args) => self.codegen_func_call(func_name, args),
            Expr::MethodCall(method_name, obj, args) => {
                self.codegen_method_call(method_name, self.node(*obj), args)
            }

            Expr::Let(name, value, body) => {
                self.codegen_expr(self.node(*value));
                let slot = self.local_count;
                self.local_count += 1;
                self.instructions.push(AsmInstruction::StoreLocal(slot));
                let outer = self.locals_map.insert(name, slot);
                self.codegen_expr(self.node(*body));
                if let Some(outer) = outer {
                    self.locals_map.insert(name, outer);
                } else {
                    self.locals_map.remove(name.as_str());
                }
            }
        }
    }

    pub(crate) fn codegen_field(&mut self, field: &'a str) {
        if let Some(&slot) = self.locals_map.get(field) {
            self.instructions.push(AsmInstruction::LoadLocal(slot));
            return;
        }
        let index = *self.context_vars_map.entry(field).or_insert_with(|| {
            let index = self.context_vars.len() as u32;
            self.context_vars.push(AsmContextVar::new(field));
            index
        });
        self.instructions.push(AsmInstruction::LoadContext(index));
    }

    pub(crate) fn codegen_field_access(&mut self, obj: &'a Expr, field: &'a str) {
        self.codegen_expr(obj);
        let index = self.intern_str(field);
        self.instructions.push(AsmInstruction::GetField(index));
    }

    fn intern_str(&mut self, value: &'a str) -> u32 {
        *self.str_const_map.entry(value).or_insert_with(|| {
            let index = self.const_pool.len() as u32;
            self.const_pool.push(Value::Str(Arc::new(value.to_string())));
            index
        })
    }

    pub(crate) fn codegen_str(&mut self, value: &'a str) {
        let index = self.intern_str(value);
        self.instructions.push(AsmInstruction::LoadConst(index));
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

    pub(crate) fn codegen_array(&mut self, elements: &'a [Expr]) {
        // Codegen all elements first
        for element in elements {
            self.codegen_expr(element);
        }
        // Then build the array
        self.instructions
            .push(AsmInstruction::BuildArray(elements.len() as u32));
    }

    pub(crate) fn codegen_object(&mut self, fields: &'a BTreeMap<filter_expr::CompactString, Expr>) {
        for (key, value) in fields {
            self.codegen_str(key);
            self.codegen_expr(value);
        }
        self.instructions
            .push(AsmInstruction::BuildObject(fields.len() as u32));
    }

    pub(crate) fn codegen_gt(&mut self, left: &'a Expr, right: &'a Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Gt);
    }

    pub(crate) fn codegen_lt(&mut self, left: &'a Expr, right: &'a Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Lt);
    }

    pub(crate) fn codegen_ge(&mut self, left: &'a Expr, right: &'a Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Ge);
    }

    pub(crate) fn codegen_le(&mut self, left: &'a Expr, right: &'a Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Le);
    }

    pub(crate) fn codegen_eq(&mut self, left: &'a Expr, right: &'a Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Eq);
    }

    pub(crate) fn codegen_ne(&mut self, left: &'a Expr, right: &'a Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::Ne);
    }

    pub(crate) fn codegen_in(&mut self, left: &'a Expr, right: &'a Expr) {
        self.codegen_expr(left);
        self.codegen_expr(right);
        self.instructions.push(AsmInstruction::In);
    }

    pub(crate) fn codegen_and(&mut self, exprs: &'a [Expr]) {
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

    pub(crate) fn codegen_or(&mut self, exprs: &'a [Expr]) {
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

    pub(crate) fn codegen_not(&mut self, expr: &'a Expr) {
        self.codegen_expr(expr);
        self.instructions.push(AsmInstruction::Not);
    }

    pub(crate) fn codegen_func_call(&mut self, func_name: &'a str, args: &'a [Expr]) {
        for arg in args {
            self.codegen_expr(arg);
        }

        // Get or create function index.
        let func_index = *self.functions_map.entry(func_name).or_insert_with(|| {
            let index = self.functions.len() as u32;
            self.functions.push(AsmFunction::new(func_name));
            index
        });

        // Call the function.
        self.instructions
            .push(AsmInstruction::CallFunction(func_index, args.len() as u32));
    }

    pub(crate) fn codegen_method_call(
        &mut self,
        method_name: &'a str,
        obj: &'a Expr,
        args: &'a [Expr],
    ) {
        // Codegen the object first.
        self.codegen_expr(obj);

        // Codegen all arguments.
        for arg in args {
            self.codegen_expr(arg);
        }

        // Get or create method index.
        let method_index = *self.methods_map.entry(method_name).or_insert_with(|| {
            let index = self.methods.len() as u32;
            self.methods.push(AsmMethod::new(method_name));
            index
        });

        // Call the method.
        self.instructions
            .push(AsmInstruction::CallMethod(method_index, args.len() as u32));
    }
}

#[cfg(test)]
mod tests {
    use crate::Value;
    macro_rules! alloc {
        ($arena:expr, $expr:expr) => {{
            let expr = $expr;
            $arena.alloc(expr)
        }};
    }

    use core::f64;
    use std::sync::Arc;

    use super::*;
    use filter_expr::{Expr, ExprArena};

    fn codegen(expr: &Expr, arena: &ExprArena) -> Asm {
        let filter = FilterExpr::new(Some(expr.clone()), arena.clone());
        AsmCodegen::new(&filter).codegen()
    }

    #[test]
    fn test_codegen_field() {
        let arena = ExprArena::default();
        let asm = codegen(&Expr::Field("field".into()), &arena);
        assert_eq!(
            asm,
            Asm {
                context_vars: vec![AsmContextVar::new("field")],
                instructions: vec![AsmInstruction::LoadContext(0), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_str() {
        let arena = ExprArena::default();
        let asm = codegen(&Expr::Str("str".into()), &arena);
        assert_eq!(
            asm,
            Asm {
                const_pool: vec![Value::Str(Arc::new("str".to_string()))],
                instructions: vec![AsmInstruction::LoadConst(0), AsmInstruction::Return],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_i64() {
        let arena = ExprArena::default();
        let asm = codegen(&Expr::I64(42), &arena);
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::F64(f64::consts::PI), &arena);
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::Bool(true), &arena);
        assert_eq!(
            asm,
            Asm {
                instructions: vec![AsmInstruction::LoadBool(true), AsmInstruction::Return],
                ..Default::default()
            }
        );

        let asm = codegen(&Expr::Bool(false), &arena);
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::Null, &arena);
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
        let arena = ExprArena::default();
        let asm = codegen(
            &Expr::Array(vec![Expr::I64(1), Expr::I64(2), Expr::I64(3)]),
            &arena,
        );
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::Array(vec![]), &arena);
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::Gt(alloc!(arena, Expr::I64(5)), alloc!(arena, Expr::I64(3))),
            &arena,
        );
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::Lt(alloc!(arena, Expr::I64(3)), alloc!(arena, Expr::I64(5))),
            &arena,
        );
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::Ge(alloc!(arena, Expr::I64(5)), alloc!(arena, Expr::I64(5))),
            &arena,
        );
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::Le(alloc!(arena, Expr::I64(3)), alloc!(arena, Expr::I64(5))),
            &arena,
        );
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::Eq(
                alloc!(arena, Expr::Str("hello".into())),
                alloc!(arena, Expr::Str("hello".into())),
            ),
            &arena,
        );
        // Same string constants are deduplicated
        assert_eq!(
            asm,
            Asm {
                const_pool: vec![Value::Str(Arc::new("hello".to_string()))],
                instructions: vec![
                    AsmInstruction::LoadConst(0),
                    AsmInstruction::LoadConst(0),
                    AsmInstruction::Eq,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_eq_different_strings() {
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::Eq(
                alloc!(arena, Expr::Str("hello".into())),
                alloc!(arena, Expr::Str("world".into())),
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                const_pool: vec![
                    Value::Str(Arc::new("hello".to_string())),
                    Value::Str(Arc::new("world".to_string())),
                ],
                instructions: vec![
                    AsmInstruction::LoadConst(0),
                    AsmInstruction::LoadConst(1),
                    AsmInstruction::Eq,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_ne() {
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::Ne(alloc!(arena, Expr::I64(1)), alloc!(arena, Expr::I64(2))),
            &arena,
        );
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::In(
                alloc!(arena, Expr::I64(2)),
                alloc!(
                    arena,
                    Expr::Array(vec![Expr::I64(1), Expr::I64(2), Expr::I64(3)])
                ),
            ),
            &arena,
        );
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
        let arena = ExprArena::default();
        let asm = codegen(
            &Expr::And(vec![Expr::Bool(true), Expr::Bool(false), Expr::Bool(true)]),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::And(vec![Expr::Bool(true)]), &arena);
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::And(vec![]), &arena);
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
        let arena = ExprArena::default();
        let asm = codegen(
            &Expr::Or(vec![Expr::Bool(false), Expr::Bool(true), Expr::Bool(false)]),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::Or(vec![Expr::Bool(false)]), &arena);
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::Or(vec![]), &arena);
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
        let mut arena = ExprArena::default();
        let asm = codegen(&Expr::Not(alloc!(arena, Expr::Bool(true))), &arena);
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::And(vec![
                Expr::Gt(alloc!(arena, Expr::I64(1)), alloc!(arena, Expr::I64(0))),
                Expr::Lt(alloc!(arena, Expr::I64(2)), alloc!(arena, Expr::I64(3))),
            ]),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
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
        let arena = ExprArena::default();
        let asm = codegen(
            &Expr::And(vec![Expr::Bool(false), Expr::Bool(true)]),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::Or(vec![Expr::Bool(true), Expr::Bool(false)]), &arena);
        assert_eq!(
            asm,
            Asm {
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
        let arena = ExprArena::default();
        let asm = codegen(
            &Expr::FuncCall("len".into(), vec![Expr::Str("hello".into())]),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                functions: vec![AsmFunction::new("len")],
                const_pool: vec![Value::Str(Arc::new("hello".to_string()))],
                instructions: vec![
                    AsmInstruction::LoadConst(0),
                    AsmInstruction::CallFunction(0, 1),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_func_call_multiple_args() {
        let arena = ExprArena::default();
        let asm = codegen(
            &Expr::FuncCall(
                "substr".into(),
                vec![Expr::Str("hello".into()), Expr::I64(0), Expr::I64(3)],
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                functions: vec![AsmFunction::new("substr")],
                const_pool: vec![Value::Str(Arc::new("hello".to_string()))],
                instructions: vec![
                    AsmInstruction::LoadConst(0),
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
        let arena = ExprArena::default();
        let asm = codegen(&Expr::FuncCall("now".into(), vec![]), &arena);
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
        let arena = ExprArena::default();
        let asm = codegen(
            &Expr::FuncCall(
                "len".into(),
                vec![Expr::FuncCall("len".into(), vec![Expr::Str("test".into())])],
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                functions: vec![AsmFunction::new("len")],
                const_pool: vec![Value::Str(Arc::new("test".to_string()))],
                instructions: vec![
                    AsmInstruction::LoadConst(0),
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::MethodCall(
                "toUpper".into(),
                alloc!(arena, Expr::Str("hello".into())),
                vec![],
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                methods: vec![AsmMethod::new("toUpper")],
                const_pool: vec![Value::Str(Arc::new("hello".to_string()))],
                instructions: vec![
                    AsmInstruction::LoadConst(0),
                    AsmInstruction::CallMethod(0, 0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_method_call_with_args() {
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::MethodCall(
                "substring".into(),
                alloc!(arena, Expr::Str("hello".into())),
                vec![Expr::I64(0), Expr::I64(3)],
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                methods: vec![AsmMethod::new("substring")],
                const_pool: vec![Value::Str(Arc::new("hello".to_string()))],
                instructions: vec![
                    AsmInstruction::LoadConst(0),
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::MethodCall(
                "method2".into(),
                alloc!(
                    arena,
                    Expr::MethodCall(
                        "method1".into(),
                        alloc!(arena, Expr::Str("test".into())),
                        vec![],
                    )
                ),
                vec![],
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                methods: vec![AsmMethod::new("method1"), AsmMethod::new("method2")],
                const_pool: vec![Value::Str(Arc::new("test".to_string()))],
                instructions: vec![
                    AsmInstruction::LoadConst(0),
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
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::MethodCall(
                "toUpper".into(),
                alloc!(arena, Expr::Field("name".into())),
                vec![],
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                context_vars: vec![AsmContextVar::new("name")],
                methods: vec![AsmMethod::new("toUpper")],
                instructions: vec![
                    AsmInstruction::LoadContext(0),
                    AsmInstruction::CallMethod(0, 0),
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_func_call_in_comparison() {
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::Gt(
                alloc!(
                    arena,
                    Expr::FuncCall("len".into(), vec![Expr::Str("hello".into())],)
                ),
                alloc!(arena, Expr::I64(3)),
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                functions: vec![AsmFunction::new("len")],
                const_pool: vec![Value::Str(Arc::new("hello".to_string()))],
                instructions: vec![
                    AsmInstruction::LoadConst(0),
                    AsmInstruction::CallFunction(0, 1),
                    AsmInstruction::LoadI64(3),
                    AsmInstruction::Gt,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_let() {
        let mut arena = ExprArena::default();
        let asm = codegen(
            &Expr::let_(
                "x",
                alloc!(arena, Expr::func_call_("value", vec![])),
                alloc!(
                    arena,
                    Expr::add_(
                        alloc!(arena, Expr::field_("x")),
                        alloc!(arena, Expr::field_("x"))
                    )
                ),
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                local_count: 1,
                functions: vec![AsmFunction::new("value")],
                instructions: vec![
                    AsmInstruction::CallFunction(0, 0),
                    AsmInstruction::StoreLocal(0),
                    AsmInstruction::LoadLocal(0),
                    AsmInstruction::LoadLocal(0),
                    AsmInstruction::Add,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }

    #[test]
    fn test_codegen_let_shadowing() {
        let mut arena = ExprArena::default();
        // let x := x then (let x := x + 1 then x) + x
        let asm = codegen(
            &Expr::let_(
                "x",
                alloc!(arena, Expr::field_("x")),
                alloc!(
                    arena,
                    Expr::add_(
                        alloc!(
                            arena,
                            Expr::let_(
                                "x",
                                alloc!(
                                    arena,
                                    Expr::add_(
                                        alloc!(arena, Expr::field_("x")),
                                        alloc!(arena, Expr::i64_(1))
                                    )
                                ),
                                alloc!(arena, Expr::field_("x"))
                            )
                        ),
                        alloc!(arena, Expr::field_("x"))
                    )
                ),
            ),
            &arena,
        );
        assert_eq!(
            asm,
            Asm {
                local_count: 2,
                context_vars: vec![AsmContextVar::new("x")],
                instructions: vec![
                    AsmInstruction::LoadContext(0),
                    AsmInstruction::StoreLocal(0),
                    AsmInstruction::LoadLocal(0),
                    AsmInstruction::LoadI64(1),
                    AsmInstruction::Add,
                    AsmInstruction::StoreLocal(1),
                    AsmInstruction::LoadLocal(1),
                    AsmInstruction::LoadLocal(0),
                    AsmInstruction::Add,
                    AsmInstruction::Return,
                ],
                ..Default::default()
            }
        );
    }
}
