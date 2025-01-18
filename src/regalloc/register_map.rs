use std::collections::HashMap;

use crate::{
    function::Function,
    instruction::{Instruction, Pointer, Register, Value},
};

#[derive(Debug, Clone)]
pub struct RegisterMap {
    map: HashMap<Register, Register>,
}

impl RegisterMap {
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
        }
    }

    pub fn insert(&mut self, virt_reg: Register, phys_reg: Register) {
        self.map.insert(virt_reg, phys_reg);
    }

    pub fn get(&self, virt_reg: Register) -> Option<Register> {
        self.map.get(&virt_reg).cloned()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&Register, &Register)> {
        self.map.iter()
    }

    pub fn rewrite(&self, function: &mut Function) {
        for instr in function.instrs.iter_mut() {
            self.apply(instr);
        }
    }

    pub fn apply(&self, instr: &mut Instruction) {
        match instr {
            Instruction::Load(register, pointer) => {
                self.replace_register(register);
                self.replace_pointer(pointer);
            }
            Instruction::LoadParam(register, _) => {
                self.replace_register(register);
            }
            Instruction::Store(_, pointer, value) => {
                self.replace_pointer(pointer);
                self.replace_value(value);
            }
            Instruction::Lea(register, pointer) => {
                self.replace_register(register);
                self.replace_pointer(pointer);
            }
            Instruction::Mov(register, value) => {
                self.replace_register(register);
                self.replace_value(value);
            }
            Instruction::Add(register, lhs, rhs)
            | Instruction::Sub(register, lhs, rhs)
            | Instruction::Mul(register, lhs, rhs)
            | Instruction::Sdiv(register, lhs, rhs)
            | Instruction::Udiv(register, lhs, rhs) => {
                self.replace_register(register);
                self.replace_value(lhs);
                self.replace_value(rhs);
            }
            Instruction::Branch(register, _, _) => {
                self.replace_register(register);
            }
            Instruction::Ret(Some(Value::Register(register))) => {
                self.replace_register(register);
            }
            Instruction::Push(register) => {
                self.replace_register(register);
            }
            Instruction::Pop(register) => {
                self.replace_register(register);
            }
            Instruction::Call(register, _, vec, _) => {
                self.replace_register(register);
                for reg in vec {
                    self.replace_register(reg);
                }
            }
            Instruction::Alloc(..)
            | Instruction::Label(..)
            | Instruction::Jump(..)
            | Instruction::Ret(None)
            | Instruction::Ret(Some(Value::Constant(_))) => {}
        }
    }

    fn replace_register(&self, register: &mut Register) {
        if let Some(phys_reg) = self.get(*register) {
            *register = phys_reg;
        }
    }

    fn replace_pointer(&self, pointer: &mut Pointer) {
        if let Pointer::Register(register) = pointer {
            self.replace_register(register);
        }
    }

    fn replace_value(&self, value: &mut Value) {
        if let Value::Register(register) = value {
            self.replace_register(register);
        }
    }
}
