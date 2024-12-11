#[derive(Debug, Clone, Copy)]
pub enum Type {
    I8,
    I16,
    I32,
}

#[derive(Debug, Clone, Copy)]
pub struct VirtualRegister {
    pub id: usize,
    pub ty: Type,
}

#[derive(Debug, Clone, Copy)]
pub enum Constant {
    I8(i8),
    I16(i16),
    I32(i32),
}

#[derive(Debug, Clone, Copy)]
pub enum Value {
    Constant(Constant),
    Register(VirtualRegister),
}

#[derive(Debug, Clone)]
pub struct Label {
    pub id: usize,
}

#[derive(Debug, Clone)]
pub enum Opcode {
    Nop,
    Alloc,
    Load,
    Store,
    Add,
    Sub,
    Mul,
    Div,
    Jump,
    Branch,
    Ret,
}

#[derive(Debug, Clone)]
pub struct Instruction {
    /// The opcode of the instruction.
    pub opcode: Opcode,

    /// The type of the result of the instruction.
    pub ty: Option<Type>,

    /// The register that contains the result of the instruction, if applicable.
    pub target: Option<VirtualRegister>,

    /// The first argument of the instruction, if it has one.
    pub arg1: Option<Value>,

    /// The second argument of the instruction, if it has one.
    pub arg2: Option<Value>,

    /// The label that the instruction jumps to.
    ///
    /// In the case of a `Jump` instruction, this is the label that is jumped to.
    ///
    /// In the case of a `Branch` instruction, this is the label that is jumped to if the condition is true.
    pub true_branch: Option<Label>,

    /// The label that the instruction jumps to if the condition is false.
    ///
    /// This is only applicable to `Branch` instructions.
    pub false_branch: Option<Label>,
}

impl Default for Instruction {
    fn default() -> Self {
        Self {
            opcode: Opcode::Nop,
            ty: None,
            target: None,
            arg1: None,
            arg2: None,
            true_branch: None,
            false_branch: None,
        }
    }
}
