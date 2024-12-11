use crate::instruction::{Constant, Label, Type};

pub mod x86;

#[derive(Debug, Clone)]
pub struct PhysicalRegister {
    pub id: usize,
    pub name: &'static str,
    pub ty: Type,
}

pub type TargetRegisters = &'static [&'static [(&'static str, Type)]];

pub trait Target {
    const REGISTERS: TargetRegisters;
}

pub(crate) fn physical_registers<T: Target>(_: T) -> Vec<PhysicalRegister> {
    T::REGISTERS
        .iter()
        .enumerate()
        .flat_map(|(id, regs)| {
            regs.iter()
                .map(|(name, ty)| PhysicalRegister { id, name, ty: *ty })
                .collect::<Vec<_>>()
        })
        .collect()
}
