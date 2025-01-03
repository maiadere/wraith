use std::collections::HashMap;

use crate::{function::Function, instruction::RegisterId};

/// A live interval `[start, end)`.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LiveInterval {
    pub start: usize,
    pub end: usize,
}

impl LiveInterval {
    pub fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    pub fn overlaps(&self, other: &LiveInterval) -> bool {
        self.start < other.end && other.start < self.end
    }

    pub fn len(&self) -> usize {
        self.end - self.start
    }
}

pub fn calculate_live_intervals(function: &Function) -> HashMap<RegisterId, LiveInterval> {
    let mut intervals = HashMap::new();

    for (i, instr) in function.instrs.iter().enumerate() {
        for reg in instr.regs() {
            let interval = intervals
                .entry(reg.id)
                .or_insert_with(|| LiveInterval::new(i, i));

            interval.end = i;
        }
    }

    intervals
}
