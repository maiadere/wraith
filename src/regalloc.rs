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

struct RegAllocBasic {
    reg_pool: RegisterPool,
    reg_map: RegisterMap,
    spiller: Spiller,
}

impl RegAllocBasic {
    fn new(target: &dyn Target) -> Self {
        Self {
            reg_pool: RegisterPool::new(target),
            reg_map: RegisterMap::new(),
            spiller: Spiller::new(),
        }
    }

    /// Selects physical registers for each virtual register in the given list.
    ///
    /// Returns Err(()) if a register couldn't be allocated, otherwise Ok(()).
    fn select(&mut self, function: &Function, regs: Vec<Register>) -> Result<(), ()> {
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

    fn allocate(
        &mut self,
        function: &Function,
        n: usize,
        regs: &Vec<Register>,
    ) -> Result<Function, ()> {
        let mut function = function.clone();
        let regs_to_spill: Vec<Register> = regs.iter().rev().take(n).cloned().collect();
        self.spiller.rewrite(&mut function, regs_to_spill);

        let regs = regs
            .iter()
            .rev()
            .skip(n)
            .rev()
            .chain(self.spiller.spilled_regs())
            .cloned()
            .collect::<Vec<_>>();

        if self.select(&function, regs).is_err() {
            return Err(());
        }

        self.reg_map.rewrite(&mut function);
        Ok(function)
    }
}

pub fn allocate_registers(function: &mut Function, target: &dyn Target) {
    let regs = regs_by_cost(function);

    for i in 0..=regs.len() {
        let mut allocator = RegAllocBasic::new(target);

        if let Ok(f) = allocator.allocate(function, i, &regs) {
            *function = f;
            return;
        }
    }

    panic!("failed to allocate registers");
}
