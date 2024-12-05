#[derive(Debug, Clone, Copy)]
pub enum Type {
    I8,
    I16,
    I32,
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
    Register(usize),
}
