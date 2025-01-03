use std::ops::RangeFrom;

use crate::instruction::{Instruction, Label, MemorySlot, Pointer, Register, Type, Value};

#[derive(Debug, Clone)]
pub struct Function {
    pub name: String,
    pub ty: Type,
    pub instrs: Vec<Instruction>,
    pub(crate) reg_id: RangeFrom<usize>,
    pub(crate) lbl_id: RangeFrom<usize>,
}

impl Function {
    pub fn new(name: String, ty: Type) -> Self {
        Self {
            name,
            ty,
            instrs: Vec::new(),
            reg_id: 0..,
            lbl_id: 0..,
        }
    }

    pub(crate) fn new_register(&mut self, ty: Type) -> Register {
        Register::new(self.reg_id.next().expect("ran out of register IDs"), ty)
    }

    pub(crate) fn new_memory_slot(&mut self) -> MemorySlot {
        MemorySlot::new(self.reg_id.next().expect("ran out of memory slot IDs"))
    }

    pub fn alloc(&mut self, ty: Type, count: usize) -> MemorySlot {
        let memory_slot = self.new_memory_slot();
        self.instrs.push(Instruction::Alloc(memory_slot, ty, count));
        memory_slot
    }

    pub fn load(&mut self, ty: Type, ptr: impl Into<Pointer>) -> Register {
        let reg = self.new_register(ty);
        self.instrs.push(Instruction::Load(reg, ptr.into()));
        reg
    }

    pub fn store(&mut self, ty: Type, ptr: impl Into<Pointer>, value: impl Into<Value>) {
        self.instrs
            .push(Instruction::Store(ty, ptr.into(), value.into()));
    }

    pub fn mov(&mut self, ty: Type, value: impl Into<Value>) -> Register {
        let reg = self.new_register(ty);
        self.instrs.push(Instruction::Mov(reg, value.into()));
        reg
    }

    pub fn add(&mut self, ty: Type, lhs: impl Into<Value>, rhs: impl Into<Value>) -> Register {
        let reg = self.new_register(ty);
        self.instrs
            .push(Instruction::Add(reg, lhs.into(), rhs.into()));
        reg
    }

    pub fn sub(&mut self, ty: Type, lhs: impl Into<Value>, rhs: impl Into<Value>) -> Register {
        let reg = self.new_register(ty);
        self.instrs
            .push(Instruction::Sub(reg, lhs.into(), rhs.into()));
        reg
    }

    pub fn mul(&mut self, ty: Type, lhs: impl Into<Value>, rhs: impl Into<Value>) -> Register {
        let reg = self.new_register(ty);
        self.instrs
            .push(Instruction::Mul(reg, lhs.into(), rhs.into()));
        reg
    }

    pub fn div(&mut self, ty: Type, lhs: impl Into<Value>, rhs: impl Into<Value>) -> Register {
        let reg = self.new_register(ty);
        self.instrs
            .push(Instruction::Div(reg, lhs.into(), rhs.into()));
        reg
    }

    pub fn new_label(&mut self) -> Label {
        Label {
            id: self.lbl_id.next().expect("ran out of label IDs"),
        }
    }

    pub fn add_label(&mut self, label: Label) {
        self.instrs.push(Instruction::Label(label));
    }

    pub fn label(&mut self) -> Label {
        let label = self.new_label();
        self.add_label(label);
        label
    }

    pub fn jump(&mut self, target: Label) {
        self.instrs.push(Instruction::Jump(target));
    }

    pub fn branch(&mut self, cond: Register, if_true: Label, if_false: Option<Label>) {
        self.instrs
            .push(Instruction::Branch(cond, if_true, if_false));
    }

    pub fn ret(&mut self, value: Option<impl Into<Value>>) {
        self.instrs.push(Instruction::Ret(value.map(Into::into)));
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "fn {}() -> {} {{", self.name, self.ty)?;
        for instr in &self.instrs {
            if let Instruction::Label(label) = instr {
                writeln!(f, "{}:", label)?;
            } else {
                writeln!(f, "    {}", instr)?;
            }
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}
