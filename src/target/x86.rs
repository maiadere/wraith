use crate::instruction::{RegisterId, Type::*};

use super::{RegisterKind::*, Target, TargetRegisterInfo};

pub struct X86;

const EAX: RegisterId = 0;
const EBX: RegisterId = 1;
const ECX: RegisterId = 2;
const EDX: RegisterId = 3;
const ESI: RegisterId = 4;
const EDI: RegisterId = 5;

impl Target for X86 {
    fn registers(&self) -> TargetRegisterInfo {
        let mut regs = TargetRegisterInfo::new();

        for i in [EAX, ECX, EDX] {
            regs.add(i, I8 | I16 | I32, CallerSaved);
        }

        for i in [EBX, ESI, EDI] {
            regs.add(i, I8 | I16 | I32, CalleeSaved);
        }

        // allow i64, f32, or f64 virtual regs to stay virtual
        // for spilling without IR's load/store instructions
        regs.keep_virtual(I64 | F32 | F64);
        regs
    }
}
