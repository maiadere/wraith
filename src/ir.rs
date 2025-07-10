use derive_more::{Display, From, Into};
use tinyvec::{TinyVec, tiny_vec};
use typed_index_collections::TiVec;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Opcode {
    Nop,
    Identity,
    Alloca,
    Load,
    Store,
    Const(i32),
    Add,
    Eq,
    Ret,
    Jmp(BasicBlockId),
    Br(BasicBlockId, BasicBlockId),
    Phi,
    Upsilon(InstId),
}

#[derive(Clone, Copy, Display, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into, Default)]
pub struct InstId(usize);

impl std::fmt::Debug for InstId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Clone, Copy, Display, PartialEq, Eq, PartialOrd, Ord, Hash, From, Into, Default)]
pub struct BasicBlockId(usize);

impl std::fmt::Debug for BasicBlockId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "bb{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct Inst {
    pub opcode: Opcode,
    pub args: TinyVec<[InstId; 2]>,
    pub block_id: BasicBlockId,
    pub prev: Option<InstId>,
    pub next: Option<InstId>,
}

impl Inst {
    pub fn new(opcode: Opcode, args: &[InstId], block_id: BasicBlockId) -> Self {
        Self {
            opcode,
            args: args.into(),
            block_id,
            prev: None,
            next: None,
        }
    }
}

impl std::fmt::Display for Inst {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let opcode = format!("{:?}", self.opcode);
        let args = self
            .args
            .iter()
            .map(|arg| format!("%{}", arg))
            .collect::<Vec<String>>()
            .join(", ");
        if opcode.ends_with(")") && !args.is_empty() {
            write!(f, "{}, {})", &opcode[..opcode.len() - 1], args)
        } else if !args.is_empty() {
            write!(f, "{}({})", opcode, args)
        } else {
            write!(f, "{}", opcode)
        }
    }
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub(crate) start_end: Option<(InstId, InstId)>,
}

impl BasicBlock {
    pub fn start(&self) -> Option<InstId> {
        self.start_end.map(|x| x.0)
    }

    pub fn end(&self) -> Option<InstId> {
        self.start_end.map(|x| x.1)
    }
}

pub enum InsertPosition {
    Start,
    End,
    Before(InstId),
    After(InstId),
}

#[derive(Debug, Clone)]
pub struct Function {
    pub insts: TiVec<InstId, Inst>,
    pub blocks: TiVec<BasicBlockId, BasicBlock>,
}

impl Function {
    pub fn predecessors(&self, id: BasicBlockId) -> TinyVec<[BasicBlockId; 4]> {
        let mut preds = tiny_vec!();
        for (block_id, block) in self.blocks.iter_enumerated() {
            let Some(end) = block.end() else {
                continue;
            };
            if match self.insts[end].opcode {
                Opcode::Jmp(target) => id == target,
                Opcode::Br(ift, iff) => id == ift || id == iff,
                _ => false,
            } {
                preds.push(block_id);
            }
        }
        preds
    }

    pub fn successors(&self, block_id: BasicBlockId) -> TinyVec<[BasicBlockId; 2]> {
        match self.blocks[block_id]
            .end()
            .map(|end| &self.insts[end].opcode)
        {
            Some(Opcode::Ret) => tiny_vec!(),
            Some(Opcode::Jmp(target)) => tiny_vec!([BasicBlockId; 2] => *target),
            Some(Opcode::Br(ift, iff)) => tiny_vec!([BasicBlockId; 2] => *ift, *iff),
            _ => unreachable!("basic block should end with a branch or a return"),
        }
    }

    pub fn append(&mut self, inst: Inst) -> InstId {
        self.insert(inst, InsertPosition::End)
    }

    pub fn insert(&mut self, inst: Inst, pos: InsertPosition) -> InstId {
        let block = self
            .blocks
            .get_mut(inst.block_id)
            .expect("inst.block_id should contain valid block_id");

        let inst_id = InstId::from(self.insts.len());
        self.insts.push(inst);

        match (block.start_end, pos) {
            (Some((start, end)), InsertPosition::Start) => {
                self.insts[inst_id].prev = None;
                self.insts[inst_id].next = Some(start);
                block.start_end = Some((inst_id, end));
            }
            (Some((start, end)), InsertPosition::End) => {
                self.insts[end].next = Some(inst_id);
                self.insts[inst_id].prev = Some(end);
                self.insts[inst_id].next = None;
                block.start_end = Some((start, inst_id));
            }
            (None, InsertPosition::Start) | (None, InsertPosition::End) => {
                self.insts[inst_id].prev = None;
                self.insts[inst_id].next = None;
                block.start_end = Some((inst_id, inst_id))
            }
            (Some((start, end)), InsertPosition::Before(next_id)) => {
                let old_prev = self.insts[next_id].prev;
                self.insts[next_id].prev = Some(inst_id);
                self.insts[inst_id].next = Some(next_id);
                self.insts[inst_id].prev = old_prev;
                if let Some(prev_id) = old_prev {
                    self.insts[prev_id].next = Some(inst_id);
                }
                if next_id == start {
                    block.start_end = Some((inst_id, end));
                }
            }
            (Some((start, end)), InsertPosition::After(prev_id)) => {
                let old_next = self.insts[prev_id].next;
                self.insts[prev_id].next = Some(inst_id);
                self.insts[inst_id].prev = Some(prev_id);
                self.insts[inst_id].next = old_next;

                if let Some(next) = old_next {
                    self.insts[next].prev = Some(inst_id);
                }

                if prev_id == end {
                    block.start_end = Some((start, inst_id));
                }
            }
            (None, InsertPosition::Before(_)) | (None, InsertPosition::After(_)) => {
                panic!("pos should not contain instruction id if the block is empty")
            }
        }

        inst_id
    }

    pub fn iter_block_ids(&self) -> impl std::iter::Iterator<Item = BasicBlockId> + use<> {
        (0..self.blocks.len()).map(|id| BasicBlockId::from(id))
    }

    pub fn iter_inst_ids(&self) -> impl std::iter::Iterator<Item = InstId> + use<> {
        (0..self.insts.len()).map(|id| InstId::from(id))
    }

    pub fn dump(&self) {
        for block_id in self.iter_block_ids() {
            println!("{:?}:", block_id);
            self.dump_block(block_id);
        }
    }

    pub fn dump_block(&self, block_id: BasicBlockId) {
        let mut next = self.blocks[block_id].start();

        while let Some(id) = next {
            let inst = &self.insts[id];
            next = inst.next;
            println!("  %{} = {}", id, inst);
        }
    }
}

#[derive(Debug, Clone)]
pub struct FunctionBuilder {
    function: Function,
    current_block: Option<BasicBlockId>,
}

impl FunctionBuilder {
    pub fn new() -> Self {
        Self {
            function: Function {
                insts: TiVec::new(),
                blocks: TiVec::new(),
            },
            current_block: None,
        }
    }

    pub fn build(self) -> Function {
        self.function
    }

    pub fn create_block(&mut self) -> BasicBlockId {
        self.function.blocks.push(BasicBlock { start_end: None });
        BasicBlockId::from(self.function.blocks.len() - 1)
    }

    pub fn switch_to_block(&mut self, block_id: BasicBlockId) {
        self.current_block = Some(block_id);
    }

    pub fn current_block(&self) -> Option<BasicBlockId> {
        self.current_block
    }

    fn expect_current_block(&self) -> BasicBlockId {
        self.current_block
            .expect("set current block with .switch_to_block(id)")
    }

    pub fn alloca(&mut self) -> InstId {
        self.function
            .append(Inst::new(Opcode::Alloca, &[], self.expect_current_block()))
    }

    pub fn load(&mut self, slot: InstId) -> InstId {
        self.function.append(Inst::new(
            Opcode::Load,
            &[slot],
            self.expect_current_block(),
        ))
    }

    pub fn store(&mut self, slot: InstId, value: InstId) -> InstId {
        self.function.append(Inst::new(
            Opcode::Store,
            &[slot, value],
            self.expect_current_block(),
        ))
    }

    pub fn constant(&mut self, value: i32) -> InstId {
        self.function.append(Inst::new(
            Opcode::Const(value),
            &[],
            self.expect_current_block(),
        ))
    }

    pub fn add(&mut self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.append(Inst::new(
            Opcode::Add,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn eq(&mut self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.append(Inst::new(
            Opcode::Eq,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn ret(&mut self, value: InstId) -> InstId {
        self.function.append(Inst::new(
            Opcode::Ret,
            &[value],
            self.expect_current_block(),
        ))
    }

    pub fn jmp(&mut self, target: BasicBlockId) -> InstId {
        self.function.append(Inst::new(
            Opcode::Jmp(target),
            &[],
            self.expect_current_block(),
        ))
    }

    pub fn br(&mut self, cond: InstId, ift: BasicBlockId, iff: BasicBlockId) -> InstId {
        self.function.append(Inst::new(
            Opcode::Br(ift, iff),
            &[cond],
            self.expect_current_block(),
        ))
    }
}
