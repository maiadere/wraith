#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum Type {
    Void = 0,
    I8 = 1,
    I16 = 2,
    I32 = 4,
    I64 = 8,
    F32 = 16,
    F64 = 32,
}

impl Type {
    pub fn size(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::I8 => 1,
            Type::I16 => 2,
            Type::I32 | Type::F32 => 4,
            Type::I64 | Type::F64 => 8,
        }
    }
}

pub type RegisterId = usize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Register {
    pub id: RegisterId,
    pub ty: Type,
}

impl Register {
    pub fn new(id: RegisterId, ty: Type) -> Self {
        Self { id, ty }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct MemorySlot {
    pub id: usize,
}

impl MemorySlot {
    pub fn new(id: usize) -> Self {
        Self { id }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Constant {
    Int(i128),
    Float(f64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Value {
    Constant(Constant),
    Register(Register),
}

impl Into<Value> for Constant {
    fn into(self) -> Value {
        Value::Constant(self)
    }
}

impl Into<Value> for Register {
    fn into(self) -> Value {
        Value::Register(self)
    }
}

impl Into<Value> for i32 {
    fn into(self) -> Value {
        Value::Constant(Constant::Int(self.into()))
    }
}

impl Into<Value> for i128 {
    fn into(self) -> Value {
        Value::Constant(Constant::Int(self.into()))
    }
}

impl Into<Value> for f64 {
    fn into(self) -> Value {
        Value::Constant(Constant::Float(self))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Pointer {
    MemorySlot(MemorySlot),
    Register(Register),
}

impl Into<Pointer> for MemorySlot {
    fn into(self) -> Pointer {
        Pointer::MemorySlot(self)
    }
}

impl Into<Pointer> for Register {
    fn into(self) -> Pointer {
        Pointer::Register(self)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Label {
    pub id: usize,
}

#[derive(Debug, Clone, Copy)]
pub enum Instruction {
    Alloc(MemorySlot, Type, usize),
    Load(Register, Pointer),
    Store(Type, Pointer, Value),
    Mov(Register, Value),
    Add(Register, Value, Value),
    Sub(Register, Value, Value),
    Mul(Register, Value, Value),
    Sdiv(Register, Value, Value),
    Udiv(Register, Value, Value),
    Label(Label),
    Jump(Label),
    Branch(Register, Label, Option<Label>),
    Ret(Option<Value>),
}

impl Instruction {
    pub fn uses(&self, register: Register) -> bool {
        self.used_regs().contains(&register)
    }

    pub fn used_regs(&self) -> Vec<Register> {
        let mut regs = Vec::new();
        match self {
            Instruction::Branch(reg, _, _) => regs.push(*reg),
            Instruction::Ret(Some(Value::Register(reg))) => regs.push(*reg),
            Instruction::Mov(_, Value::Register(reg)) => regs.push(*reg),
            Instruction::Load(_, Pointer::Register(reg)) => regs.push(*reg),
            Instruction::Store(_, ptr, val) => {
                if let Pointer::Register(reg) = ptr {
                    regs.push(*reg);
                }
                if let Value::Register(reg) = val {
                    regs.push(*reg);
                }
            }
            Instruction::Add(_, lhs, rhs)
            | Instruction::Sub(_, lhs, rhs)
            | Instruction::Mul(_, lhs, rhs)
            | Instruction::Sdiv(_, lhs, rhs)
            | Instruction::Udiv(_, lhs, rhs) => {
                if let Value::Register(reg) = lhs {
                    regs.push(*reg);
                }
                if let Value::Register(reg) = rhs {
                    regs.push(*reg);
                }
            }
            _ => {}
        }
        regs
    }

    pub fn regs(&self) -> Vec<Register> {
        self.defs()
            .into_iter()
            .chain(self.used_regs().into_iter())
            .collect()
    }

    pub fn defs(&self) -> Option<Register> {
        match self {
            Instruction::Load(reg, _) => Some(*reg),
            Instruction::Mov(reg, _) => Some(*reg),
            Instruction::Add(reg, _, _)
            | Instruction::Sub(reg, _, _)
            | Instruction::Mul(reg, _, _)
            | Instruction::Sdiv(reg, _, _)
            | Instruction::Udiv(reg, _, _) => Some(*reg),
            _ => None,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::I8 => write!(f, "i8"),
            Type::I16 => write!(f, "i16"),
            Type::I32 => write!(f, "i32"),
            Type::I64 => write!(f, "i64"),
            Type::F32 => write!(f, "f32"),
            Type::F64 => write!(f, "f64"),
            Type::Void => write!(f, "()"),
        }
    }
}

impl std::fmt::Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.id)
    }
}

impl std::fmt::Display for MemorySlot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.id)
    }
}

impl std::fmt::Display for Constant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Constant::Int(value) => write!(f, "{}", value),
            Constant::Float(value) => write!(f, "{}", value),
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Constant(constant) => write!(f, "{}", constant),
            Value::Register(register) => write!(f, "{}", register),
        }
    }
}

impl std::fmt::Display for Pointer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Pointer::MemorySlot(slot) => write!(f, "{}", slot),
            Pointer::Register(register) => write!(f, "{}", register),
        }
    }
}

impl std::fmt::Display for Label {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, ".L{}", self.id)
    }
}

impl std::fmt::Display for Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Instruction::Alloc(slot, ty, 1) => write!(f, "{} = alloc {}", slot, ty),
            Instruction::Alloc(slot, ty, count) => write!(f, "{} = alloc {} {}", slot, ty, count),
            Instruction::Load(reg, ptr) => write!(f, "{} = load {} {}", reg, reg.ty, ptr),
            Instruction::Store(ty, ptr, value) => write!(f, "store {} {}, {}", ty, ptr, value),
            Instruction::Mov(reg, value) if reg.ty == Type::I32 => write!(f, "{} = {}", reg, value),
            Instruction::Mov(reg, value) => write!(f, "{} = {}{}", reg, value, reg.ty),
            Instruction::Add(reg, lhs, rhs) => write!(f, "{} = add {}, {}", reg, lhs, rhs),
            Instruction::Sub(reg, lhs, rhs) => write!(f, "{} = sub {}, {}", reg, lhs, rhs),
            Instruction::Mul(reg, lhs, rhs) => write!(f, "{} = mul {}, {}", reg, lhs, rhs),
            Instruction::Sdiv(reg, lhs, rhs) => write!(f, "{} = sdiv {}, {}", reg, lhs, rhs),
            Instruction::Udiv(reg, lhs, rhs) => write!(f, "{} = udiv {}, {}", reg, lhs, rhs),
            Instruction::Label(label) => write!(f, "{}:", label),
            Instruction::Jump(label) => write!(f, "jmp {}", label),
            Instruction::Branch(cond, l1, None) => write!(f, "br {}, {}", cond, l1),
            Instruction::Branch(cond, l1, Some(l2)) => write!(f, "br {}, {}, {}", cond, l1, l2),
            Instruction::Ret(None) => write!(f, "ret"),
            Instruction::Ret(Some(value)) => write!(f, "ret {}", value),
        }
    }
}
