use std::collections::HashMap;

use crate::{
    function::Function,
    instruction::{Instruction, Register},
};

use super::register_map::RegisterMap;

pub struct Spiller {
    spilled_regs: Vec<Register>,
}

impl Spiller {
    pub fn new() -> Self {
        Self {
            spilled_regs: Vec::new(),
        }
    }

    /// Inserts the necessary load and store instructions using new virtual registers.
    /// Those registers will be added to `spilled_regs`.
    pub fn rewrite(&mut self, function: &mut Function, regs_to_spill: Vec<Register>) {
        let instrs = function.instrs.clone();
        let mut new_instrs = Vec::new();
        let mut memory_slots = HashMap::new();

        for instr in instrs {
            // Maps the old vregs to the new vregs used to load and store the values.
            let mut new_regs = RegisterMap::new();

            for &reg in regs_to_spill.iter() {
                let uses_or_defs = instr.uses(reg) || instr.defs() == Some(reg);

                // If the register is used in the instruction and
                // it doesn't have a memory slot, allocate a new one.
                if uses_or_defs && !memory_slots.contains_key(&reg) {
                    let memory_slot = function.new_memory_slot();
                    new_instrs.insert(0, Instruction::Alloc(memory_slot, reg.ty, 1));
                    memory_slots.insert(reg, memory_slot);
                }

                // Create new virtual register to hold temporary value.
                let new_reg = function.new_register(reg.ty);
                new_regs.insert(reg, new_reg);

                if uses_or_defs {
                    self.spilled_regs.push(new_reg);
                }

                // If the instruction uses the register, load the value from the memory slot.
                if instr.uses(reg) {
                    new_instrs.push(Instruction::Load(new_reg, memory_slots[&reg].into()));
                }
            }

            // Replace the old vregs with the new vregs in the instruction.
            let mut new_instr = instr.clone();
            new_regs.apply(&mut new_instr);
            new_instrs.push(new_instr);

            // If the instruction defines a register that was spilled,
            // store the value in the memory slot.
            for (old_reg, new_reg) in new_regs.iter() {
                if instr.defs() == Some(*old_reg) {
                    new_instrs.push(Instruction::Store(
                        old_reg.ty,
                        memory_slots[&old_reg].into(),
                        (*new_reg).into(),
                    ));
                }
            }
        }

        // Update the function with the new instructions.
        function.instrs = new_instrs;
    }

    pub fn is_spilled(&self, register: Register) -> bool {
        self.spilled_regs.contains(&register)
    }

    pub fn spilled_regs(&self) -> &Vec<Register> {
        &self.spilled_regs
    }
}
