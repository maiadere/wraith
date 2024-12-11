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
    mem_id_generator: RangeFrom<usize>,
    lbl_id_generator: RangeFrom<usize>,
}

impl FunctionBuilder {
    pub fn new(name: String) -> Self {
        Self {
            name,
            stmts: Vec::new(),
            reg_id_generator: 0..,
            mem_id_generator: 0..,
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
