use std::ops::RangeFrom;

use crate::instruction::{Instruction, Label, MemorySlot, Pointer, Register, Type, Value};

#[derive(Debug, Clone)]
pub struct FunctionDecl {
    pub name: String,
    pub ty: Type,
    pub params: Vec<Register>,
    pub variadic: bool,
}

#[derive(Debug, Clone)]
pub struct ExternalFunction {
    pub decl: FunctionDecl,
}

impl ExternalFunction {
    pub fn new(name: String, ty: Type) -> Self {
        Self {
            decl: FunctionDecl {
                name,
                ty,
                params: Vec::new(),
                variadic: false,
            },
        }
    }

    pub fn add_param(&mut self, ty: Type) {
        let i = self.decl.params.len();
        self.decl.params.push(Register::new(i, ty));
    }

    pub fn set_variadic(&mut self, variadic: bool) {
        self.decl.variadic = variadic;
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub decl: FunctionDecl,
    pub instrs: Vec<Instruction>,
    pub(crate) reg_id: RangeFrom<usize>,
    pub(crate) lbl_id: RangeFrom<usize>,
}

impl Function {
    pub fn new(name: String, ty: Type) -> Self {
        Self {
            decl: FunctionDecl {
                name,
                ty,
                params: Vec::new(),
                variadic: false,
            },
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

    pub fn add_param(&mut self, ty: Type) -> Register {
        let param = self.new_register(ty);
        self.decl.params.push(param);
        param
    }

    pub fn set_variadic(&mut self, variadic: bool) {
        self.decl.variadic = variadic;
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

    pub fn lea(&mut self, ty: Type, ptr: impl Into<Pointer>) -> Register {
        let reg = self.new_register(ty);
        self.instrs.push(Instruction::Lea(reg, ptr.into()));
        reg
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

    pub fn sdiv(&mut self, ty: Type, lhs: impl Into<Value>, rhs: impl Into<Value>) -> Register {
        let reg = self.new_register(ty);
        self.instrs
            .push(Instruction::Sdiv(reg, lhs.into(), rhs.into()));
        reg
    }

    pub fn udiv(&mut self, ty: Type, lhs: impl Into<Value>, rhs: impl Into<Value>) -> Register {
        let reg = self.new_register(ty);
        self.instrs
            .push(Instruction::Udiv(reg, lhs.into(), rhs.into()));
        reg
    }

    pub fn call(&mut self, ty: Type, name: impl Into<String>, args: Vec<Register>) -> Register {
        let reg = self.new_register(ty);
        self.instrs.push(Instruction::Call(
            reg,
            name.into(),
            args.into_iter().map(Into::into).collect(),
            false,
        ));
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

impl std::fmt::Display for FunctionDecl {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "fn {}(", self.name)?;
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", param, param.ty)?;
        }
        if self.variadic {
            write!(f, ", ...")?;
        }
        write!(f, ") -> {}", self.ty)
    }
}

impl std::fmt::Display for ExternalFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern {}", self.decl)?;
        Ok(())
    }
}

impl std::fmt::Display for Function {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {{", self.decl)?;
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
