use std::collections::{HashMap, HashSet};

use crate::{
    instruction::{RegisterId, Type},
    target::{RegisterKind, Target, TargetRegisterInfo},
};

use super::live_interval::LiveInterval;

#[derive(Debug)]
pub struct RegisterPool {
    registers: TargetRegisterInfo,
    liveness: HashMap<RegisterId, HashSet<LiveInterval>>,
}

impl RegisterPool {
    pub fn new(target: &dyn Target) -> Self {
        Self {
            registers: target.registers(),
            liveness: HashMap::new(),
        }
    }

    /// Selects a register for the given type and interval,
    /// then updates the liveness information.
    ///
    /// Returns `None` if no register is available.
    pub fn select(&mut self, ty: Type, interval: LiveInterval) -> Option<RegisterId> {
        let mut reg_id = None;

        for register in self.registers.regs.iter() {
            if !register.ty.contains(ty) {
                continue;
            }

            if let Some(intervals) = self.liveness.get(&register.id) {
                if intervals.iter().any(|i| i.overlaps(&interval)) {
                    continue;
                }
            }

            reg_id = Some(register.id);

            if register.kind == RegisterKind::CallerSaved {
                break;
            }
        }

        if let Some(id) = reg_id {
            self.liveness
                .entry(id)
                .or_insert_with(HashSet::new)
                .insert(interval);
        }

        reg_id
    }

    /// Reserves `n` physical registers of each type.
    ///
    /// Register types that are marked as virtual are skipped.
    pub fn reserve(&mut self, n: usize) -> Vec<RegisterId> {
        use Type::*;
        let err = format!("each type should have at least {} registers available", n);
        let mut reserved = Vec::new();

        for ty in [I8, I16, I32, I64, F32, F64] {
            if self.registers.should_stay_virtual(ty) {
                continue;
            }

            let f = |&&id: &&RegisterId| self.registers.find(id).unwrap().ty.contains(ty);
            let count = n.saturating_sub(reserved.iter().filter(f).count());

            for _ in 0..count {
                let inf = LiveInterval::new(0, usize::MAX);
                let id = self.select(ty, inf).expect(&err);
                reserved.push(id);
            }
        }

        reserved
    }

    /// Clears the liveness information for the given register.
    pub fn clear(&mut self, id: RegisterId) {
        self.liveness.remove(&id);
    }

    /// Returns `true` if a register of given type should stay virtual.
    pub fn should_stay_virtual(&self, ty: Type) -> bool {
        self.registers.should_stay_virtual(ty)
    }
}
