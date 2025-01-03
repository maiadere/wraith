use std::collections::{BTreeMap, HashSet};

use crate::{
    function::Function,
    instruction::{Instruction, Register},
};

struct RegSpillCostInfo {
    use_count: usize,
    def_count: usize,
    max_depth: usize,
}

impl RegSpillCostInfo {
    fn new() -> Self {
        Self {
            use_count: 0,
            def_count: 0,
            max_depth: 0,
        }
    }

    fn spill_cost(&self) -> usize {
        self.use_count + self.def_count + 10_usize.pow(self.max_depth as u32)
    }
}

pub fn calculate_spill_costs(function: &Function) -> BTreeMap<Register, usize> {
    let mut spill_costs = BTreeMap::new();
    let mut labels = HashSet::new();
    let mut current_depth = 0;

    for instr in function.instrs.iter().rev() {
        if let Some(reg) = instr.defs() {
            let reg = spill_costs.entry(reg).or_insert_with(RegSpillCostInfo::new);
            reg.def_count += 1;
            reg.max_depth = reg.max_depth.max(current_depth);
        }

        for reg in instr.used_regs() {
            let reg = spill_costs.entry(reg).or_insert_with(RegSpillCostInfo::new);
            reg.use_count += 1;
            reg.max_depth = reg.max_depth.max(current_depth);
        }

        match instr {
            Instruction::Branch(_, _, Some(label))
            | Instruction::Branch(_, label, _)
            | Instruction::Jump(label) => {
                if !labels.contains(&label.id) {
                    current_depth += 1;
                }
                labels.insert(label.id);
            }
            Instruction::Label(label) => {
                if labels.contains(&label.id) {
                    current_depth -= 1;
                }
                labels.insert(label.id);
            }
            _ => {}
        }
    }

    spill_costs
        .iter()
        .map(|(reg_id, cost)| (*reg_id, cost.spill_cost()))
        .collect()
}

pub fn regs_by_cost(function: &Function) -> Vec<Register> {
    let mut spill_costs = calculate_spill_costs(function)
        .into_iter()
        .collect::<Vec<_>>();
    spill_costs.sort_by_key(|(_, cost)| usize::MAX - *cost);
    spill_costs.iter().map(|(reg, _)| *reg).collect()
}
