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

    pub fn ret(&mut self, value: Value) {
        let instr = Instruction::Ret(value);
        self.stmts.push(Statement::Instruction(instr));
    }
}
