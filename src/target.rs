use crate::{
    instruction::{Instruction, RegisterId, Type},
    module::Module,
};

pub mod x86;

pub trait Target {
    fn registers(&self) -> TargetRegisterInfo;
    fn compile(&self, module: &Module);
    fn get_clobbered_registers(&self, instr: Instruction) -> &[RegisterId];
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct PhysicalRegisterType(usize);

impl std::ops::BitOr<Type> for Type {
    type Output = PhysicalRegisterType;

    fn bitor(self, rhs: Type) -> PhysicalRegisterType {
        PhysicalRegisterType(self as usize | rhs as usize)
    }
}

impl std::ops::BitOr<Type> for PhysicalRegisterType {
    type Output = PhysicalRegisterType;

    fn bitor(self, rhs: Type) -> PhysicalRegisterType {
        PhysicalRegisterType(self.0 | rhs as usize)
    }
}

impl std::ops::BitOr<PhysicalRegisterType> for PhysicalRegisterType {
    type Output = PhysicalRegisterType;

    fn bitor(self, rhs: PhysicalRegisterType) -> PhysicalRegisterType {
        PhysicalRegisterType(self.0 | rhs.0)
    }
}

impl PhysicalRegisterType {
    pub fn void() -> Self {
        Type::Void.into()
    }

    pub fn contains(&self, ty: Type) -> bool {
        (self.0 & ty as usize) != 0
    }
}

impl Into<PhysicalRegisterType> for Type {
    fn into(self) -> PhysicalRegisterType {
        PhysicalRegisterType(self as usize)
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegisterKind {
    /// Caller-saved (call-clobbered) register
    CallerSaved,
    /// Callee-saved (call-preserved) register
    CalleeSaved,
}

#[derive(Debug, Clone, Copy)]
pub struct PhysicalRegister {
    /// The unique id of the register
    ///
    /// Multiple registers can have the same id
    /// if they are aliases (e.g. `eax`, `ax` on x86)
    pub id: RegisterId,

    /// The types of values that can be stored in this register
    pub ty: PhysicalRegisterType,

    /// The kind of register (caller-saved or callee-saved)
    pub kind: RegisterKind,
}

#[derive(Debug, Clone)]
pub struct TargetRegisterInfo {
    pub(crate) regs: Vec<PhysicalRegister>,
    pub(crate) vreg_ty: PhysicalRegisterType,
}

impl TargetRegisterInfo {
    pub fn new() -> Self {
        Self {
            regs: Vec::new(),
            vreg_ty: Type::Void.into(),
        }
    }

    pub fn add(&mut self, id: RegisterId, ty: impl Into<PhysicalRegisterType>, kind: RegisterKind) {
        self.regs.push(PhysicalRegister {
            id,
            ty: ty.into(),
            kind,
        });
    }

    pub fn keep_virtual(&mut self, ty: impl Into<PhysicalRegisterType>) {
        self.vreg_ty = ty.into();
    }

    pub fn should_stay_virtual(&self, ty: Type) -> bool {
        self.vreg_ty.contains(ty)
    }

    pub fn find(&self, id: RegisterId) -> Option<&PhysicalRegister> {
        self.regs.iter().find(|r| r.id == id)
    }

    pub fn is_caller_saved(&self, id: RegisterId) -> bool {
        self.find(id)
            .map_or(false, |r| r.kind == RegisterKind::CallerSaved)
    }

    pub fn is_callee_saved(&self, id: RegisterId) -> bool {
        self.find(id)
            .map_or(false, |r| r.kind == RegisterKind::CalleeSaved)
    }
}
