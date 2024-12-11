use std::ops::RangeFrom;

use crate::instruction::{Constant, Instruction, Label, Opcode, Type, Value, VirtualRegister};

#[derive(Debug, Clone)]
pub enum Statement {
    Instruction(Instruction),
    Label(Label),
}

#[derive(Debug, Clone)]
pub struct FunctionBuilder {
    pub(crate) name: String,
    pub(crate) stmts: Vec<Statement>,

    reg_id_generator: RangeFrom<usize>,
    lbl_id_generator: RangeFrom<usize>,
}

impl FunctionBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            stmts: Vec::new(),
            reg_id_generator: 0..,
            lbl_id_generator: 0..,
        }
    }

    fn new_register(&mut self, ty: Type) -> VirtualRegister {
        VirtualRegister {
            id: self
                .reg_id_generator
                .next()
                .expect("ran out of unique register IDs"),
            ty,
        }
    }

    pub fn constant(&mut self, value: Constant) -> Value {
        Value::Constant(value)
    }

    fn alloc_stmt(&self, target: VirtualRegister, ty: Type, count: Option<usize>) -> Statement {
        Statement::Instruction(Instruction {
            opcode: Opcode::Alloc,
            ty: Some(ty),
            target: Some(target),
            arg1: Some(Value::Constant(Constant::I32(count.unwrap_or(1) as i32))),
            ..Default::default()
        })
    }

    pub fn prealloc_n(&mut self, ty: Type, count: Option<usize>) -> Value {
        let target = self.new_register(ty);
        self.stmts.insert(0, self.alloc_stmt(target, ty, count));
        Value::Register(target)
    }

    pub fn prealloc(&mut self, ty: Type) -> Value {
        self.prealloc_n(ty, None)
    }

    pub fn alloc_n(&mut self, ty: Type, count: Option<usize>) -> Value {
        let target = self.new_register(ty);
        self.stmts.push(self.alloc_stmt(target, ty, count));
        Value::Register(target)
    }

    pub fn alloc(&mut self, ty: Type) -> Value {
        self.alloc_n(ty, None)
    }

    pub fn load(&mut self, ty: Type, ptr: Value) -> Value {
        let target = self.new_register(ty);
        self.stmts.push(Statement::Instruction(Instruction {
            opcode: Opcode::Load,
            ty: Some(ty),
            target: Some(target),
            arg1: Some(ptr),
            ..Default::default()
        }));
        Value::Register(target)
    }

    pub fn store(&mut self, ptr: Value, value: Value) {
        self.stmts.push(Statement::Instruction(Instruction {
            opcode: Opcode::Store,
            arg1: Some(ptr),
            arg2: Some(value),
            ..Default::default()
        }));
    }

    pub fn add(&mut self, ty: Type, lhs: Value, rhs: Value) -> Value {
        let target = self.new_register(ty);
        self.push_3ac(Opcode::Add, ty, target, lhs, rhs)
    }

    pub fn sub(&mut self, ty: Type, lhs: Value, rhs: Value) -> Value {
        let target = self.new_register(ty);
        self.push_3ac(Opcode::Sub, ty, target, lhs, rhs)
    }

    pub fn mul(&mut self, ty: Type, lhs: Value, rhs: Value) -> Value {
        let target = self.new_register(ty);
        self.push_3ac(Opcode::Mul, ty, target, lhs, rhs)
    }

    pub fn div(&mut self, ty: Type, lhs: Value, rhs: Value) -> Value {
        let target = self.new_register(ty);
        self.push_3ac(Opcode::Div, ty, target, lhs, rhs)
    }

    pub fn ret(&mut self, value: Value) {
        self.stmts.push(Statement::Instruction(Instruction {
            opcode: Opcode::Ret,
            arg1: Some(value),
            ..Default::default()
        }));
    }

    fn push_3ac(
        &mut self,
        opcode: Opcode,
        ty: Type,
        target: VirtualRegister,
        arg1: Value,
        arg2: Value,
    ) -> Value {
        self.stmts.push(Statement::Instruction(Instruction {
            opcode,
            ty: Some(ty),
            target: Some(target),
            arg1: Some(arg1),
            arg2: Some(arg2),
            ..Default::default()
        }));
        Value::Register(target)
    }
}
