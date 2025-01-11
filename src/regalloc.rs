use live_interval::calculate_live_intervals;
use register_map::RegisterMap;
use register_pool::RegisterPool;
use spill_cost::regs_by_cost;
use spiller::Spiller;

use crate::{function::Function, instruction::Register, target::Target};

pub mod live_interval;
pub mod register_map;
pub mod register_pool;
pub mod spill_cost;
pub mod spiller;

#[derive(Debug, Clone)]
struct RegisterAllocator {
    function: Function,
    reg_pool: RegisterPool,
    reg_map: RegisterMap,
    spiller: Spiller,
}

impl RegisterAllocator {
    fn new(func: &Function, target: &dyn Target) -> Self {
        Self {
            function: func.clone(),
            reg_pool: RegisterPool::new(func, target),
            reg_map: RegisterMap::new(),
            spiller: Spiller::new(),
        }
    }

    /// Selects physical registers for each virtual register in the given list.
    ///
    /// Returns Err(()) if a register couldn't be allocated, otherwise Ok(()).
    fn try_select(&mut self, function: &Function, regs: Vec<Register>) -> Result<(), ()> {
        let live_intervals = calculate_live_intervals(function);

        for virt_reg in regs {
            if self.reg_pool.should_stay_virtual(virt_reg.ty) {
                continue;
            }

            let interval = live_intervals[&virt_reg.id];

            if let Some(phys_reg) = self.reg_pool.select(virt_reg.ty, interval) {
                let phys_reg = Register::new(phys_reg, virt_reg.ty);
                self.reg_map.insert(virt_reg, phys_reg);
            } else {
                return Err(());
            }
        }

        Ok(())
    }

    fn try_alloc(&mut self, n: usize, regs: &Vec<Register>) -> Option<Function> {
        let mut func = self.function.clone();
        let regs_to_spill = regs.iter().skip(regs.len() - n).copied();
        self.spiller.rewrite(&mut func, regs_to_spill.collect());

        let spilled = self.spiller.spilled_regs();
        let regs = regs.iter().take(regs.len() - n);
        let regs = spilled.iter().chain(regs).copied();

        if self.try_select(&func, regs.collect()).is_err() {
            return None;
        }

        self.reg_map.rewrite(&mut func);
        Some(func)
    }

    fn alloc(self) -> Function {
        let regs = regs_by_cost(&self.function);

        for i in 0..=regs.len() {
            let new_func = self.clone().try_alloc(i, &regs);

            if let Some(new_func) = new_func {
                return new_func;
            }
        }

        panic!("failed to allocate registers");
    }
}

pub fn allocate_registers(function: &mut Function, target: &dyn Target) {
    *function = RegisterAllocator::new(function, target).alloc();
}
