use typed_index_collections::TiVec;

use crate::ir::{Function, InstId, Opcode};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Location {
    Stack(usize),
    Register(String),
    Const(i32),
}

impl Location {
    pub fn is_stack(&self) -> bool {
        match self {
            Location::Stack(_) => true,
            _ => false,
        }
    }

    pub fn is_reg(&self) -> bool {
        match self {
            Location::Register(_) => true,
            _ => false,
        }
    }

    pub fn is_const(&self) -> bool {
        match self {
            Location::Const(_) => true,
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct RegAlloc {
    local: TiVec<InstId, bool>,
    use_count: TiVec<InstId, usize>,
    locations: TiVec<InstId, Option<Location>>,
    pool: Vec<String>,
    stack_size: usize,
}

impl RegAlloc {
    pub fn new(func: &Function, pool: Vec<String>) -> Self {
        let min_pool = pool.len() / 2;

        let mut regalloc = Self {
            local: vec![true; func.insts.len()].into(),
            use_count: vec![0; func.insts.len()].into(),
            locations: vec![None; func.insts.len()].into(),
            pool,
            stack_size: 0,
        };

        for (inst_id, inst) in func.insts.iter_enumerated() {
            for &arg_id in &inst.args {
                let arg = &func.insts[arg_id];

                regalloc.use_count[arg_id] += 1;

                if arg.block_id != inst.block_id
                    || match arg.opcode {
                        Opcode::Alloca | Opcode::Const(_) | Opcode::Phi => true,
                        _ => false,
                    }
                {
                    regalloc.local[arg_id] = false;
                }
            }

            match inst.opcode {
                Opcode::Phi => {
                    if inst.opcode == Opcode::Phi && regalloc.pool.len() > min_pool {
                        regalloc.locations[inst_id] = Some(regalloc.alloc());
                    }
                }
                Opcode::Const(value) => regalloc.locations[inst_id] = Some(Location::Const(value)),
                _ => {}
            }
        }

        regalloc
    }

    pub fn get(&mut self, id: InstId) -> Location {
        if let Some(location) = &self.locations[id] {
            return location.clone();
        }

        let location = if self.is_local(id) {
            self.alloc()
        } else {
            Location::Stack(self.alloc_stack())
        };

        self.locations[id] = Some(location.clone());
        location
    }

    pub fn free(&mut self, id: InstId) {
        if let Some(Location::Register(reg)) = &self.locations[id] {
            self.pool.push(reg.clone());
        }
    }

    pub fn is_local(&self, id: InstId) -> bool {
        self.local[id]
    }

    pub fn get_use_count(&self, id: InstId) -> usize {
        self.use_count[id]
    }

    pub fn stack_size(&self) -> usize {
        self.stack_size
    }

    fn alloc_stack(&mut self) -> usize {
        self.stack_size += 8;
        self.stack_size
    }

    fn alloc(&mut self) -> Location {
        if let Some(reg) = self.pool.pop() {
            Location::Register(reg)
        } else {
            Location::Stack(self.alloc_stack())
        }
    }
}
