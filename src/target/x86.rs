use crate::instruction::Type;

use super::{Target, TargetRegisters};

pub struct X86Target;

impl Target for X86Target {
    const REGISTERS: TargetRegisters = &[
        &[("eax", Type::I32), ("ax", Type::I16), ("al", Type::I8)],
        &[("ebx", Type::I32), ("bx", Type::I16), ("bl", Type::I8)],
        &[("ecx", Type::I32), ("cx", Type::I16), ("cl", Type::I8)],
        &[("edx", Type::I32), ("dx", Type::I16), ("dl", Type::I8)],
        &[("esi", Type::I32), ("si", Type::I16), ("sil", Type::I8)],
        &[("edi", Type::I32), ("di", Type::I16), ("dil", Type::I8)],
    ];
}
