use crate::instruction::Instruction;
use crate::value::{Type, Value};

#[derive(Debug, Clone)]
pub struct Label {
    pub(crate) id: usize,
}

#[derive(Debug, Clone)]
pub enum Statement {
    Instruction(Instruction),
    Label(Label),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub(crate) stmts: Vec<Statement>,
    pub(crate) reg: usize,
    pub(crate) lbl: usize,
}

impl Function {
    pub fn new() -> Self {
        Self {
            stmts: Vec::new(),
            reg: 0,
            lbl: 0,
        }
    }

    pub fn new_label(&mut self) -> Label {
        self.lbl += 1;
        Label { id: self.lbl }
    }

    pub fn label(&mut self, lbl: Label) {
        self.stmts.push(Statement::Label(lbl));
    }

    fn reg(&mut self, instr: Instruction) -> Value {
        let reg = self.reg;
        self.reg += 1;
        self.stmts.push(Statement::Instruction(instr));
        Value::Register(reg)
    }

    fn add_instr(&mut self, instr: Instruction) {
        self.stmts.push(Statement::Instruction(instr));
    }

    pub fn alloc(&mut self, ty: Type, size: usize) -> Value {
        self.reg(Instruction::Alloc(self.reg, ty, size))
    }

    pub fn load(&mut self, ty: Type, ptr: Value) -> Value {
        self.reg(Instruction::Load(self.reg, ty, ptr))
    }

    pub fn store(&mut self, ty: Type, ptr: Value, value: Value) {
        self.add_instr(Instruction::Store(ty, ptr, value));
    }

    pub fn add(&mut self, ty: Type, a: Value, b: Value) -> Value {
        self.reg(Instruction::Add(self.reg, ty, a, b))
    }

    pub fn sub(&mut self, ty: Type, a: Value, b: Value) -> Value {
        self.reg(Instruction::Sub(self.reg, ty, a, b))
    }

    pub fn mul(&mut self, ty: Type, a: Value, b: Value) -> Value {
        self.reg(Instruction::Mul(self.reg, ty, a, b))
    }

    pub fn div(&mut self, ty: Type, a: Value, b: Value) -> Value {
        self.reg(Instruction::Div(self.reg, ty, a, b))
    }

    pub fn jump(&mut self, lbl: Label) {
        self.add_instr(Instruction::Jump(lbl));
    }

    pub fn branch(&mut self, cond: Value, if_true: Label, if_false: Option<Label>) {
        self.add_instr(Instruction::Branch(cond, if_true, if_false));
    }

    pub fn ret(&mut self, value: Value) {
        self.add_instr(Instruction::Ret(value));
    }
}
