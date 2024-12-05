use crate::{
    function::Label,
    value::{Type, Value},
};

#[derive(Debug, Clone)]
pub enum Instruction {
    Alloc(usize, Type, usize),
    Load(usize, Type, Value),
    Store(Type, Value, Value),
    Add(usize, Type, Value, Value),
    Sub(usize, Type, Value, Value),
    Mul(usize, Type, Value, Value),
    Div(usize, Type, Value, Value),
    Jump(Label),
    Branch(Value, Label, Option<Label>),
    Ret(Value),
}

impl Instruction {
    pub fn has_target(&self) -> bool {
        match self {
            Instruction::Store(_, _, _) => false,
            Instruction::Jump(_) => false,
            Instruction::Branch(_, _, _) => false,
            Instruction::Ret(_) => false,
            _ => true,
        }
    }
}
