use crate::value::{Type, Value};

#[derive(Debug, Clone)]
pub enum Instruction {
    Add(usize, Type, Value, Value),
    Sub(usize, Type, Value, Value),
    Mul(usize, Type, Value, Value),
    Div(usize, Type, Value, Value),
    Ret(Value),
}

impl Instruction {
    pub fn has_target(&self) -> bool {
        match self {
            Instruction::Ret(_) => false,
            _ => true,
        }
    }
}
