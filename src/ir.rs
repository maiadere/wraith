use std::cell::RefCell;

use derive_more::{Display, From, Into};
use tinyvec::{TinyVec, tiny_vec};
use typed_index_collections::TiVec;

#[derive(Debug, Clone, Copy, Display, PartialEq, Eq)]
pub enum Type {
    Void,
    Ptr,
    I8,
    I16,
    I32,
    I64,
    U8,
    U16,
    U32,
    U64,
    F32,
    F64,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Opcode {
    Nop,
    Identity,
    Alloca {
        ty: Type,
        count: usize,
        alignment: usize,
    },
    Load(Type),
    Store,
    I8(i8),
    I16(i16),
    I32(i32),
    I64(i64),
    U8(u8),
    U16(u16),
    U32(u32),
    U64(u64),
    F32(f32),
    F64(f64),
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    And,
    Or,
    Xor,
    Shl,
    Shr,
    Eq,
    Ne,
    Gt,
    Lt,
    Ge,
    Le,
    Ret,
    Jmp(BasicBlockId),
    Br(BasicBlockId, BasicBlockId),
    Phi,
    Upsilon(InstId),
}

pub trait ToConstOpcode {
    fn to_const_opcode(self) -> Opcode;
}

macro_rules! impl_to_const_opcode {
    ($ty:tt, $opcode:expr) => {
        impl ToConstOpcode for $ty {
            fn to_const_opcode(self) -> Opcode {
                $opcode(self)
            }
        }
    };
}

impl_to_const_opcode!(i8, Opcode::I8);
impl_to_const_opcode!(i16, Opcode::I16);
impl_to_const_opcode!(i32, Opcode::I32);
impl_to_const_opcode!(i64, Opcode::I64);
impl_to_const_opcode!(u8, Opcode::U8);
impl_to_const_opcode!(u16, Opcode::U16);
impl_to_const_opcode!(u32, Opcode::U32);
impl_to_const_opcode!(u64, Opcode::U64);
impl_to_const_opcode!(f32, Opcode::F32);
impl_to_const_opcode!(f64, Opcode::F64);

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

    pub fn inst_type(&self, id: InstId) -> Type {
        let inst = &self.insts[id];

        match inst.opcode {
            Opcode::Nop
            | Opcode::Store
            | Opcode::Ret
            | Opcode::Jmp(..)
            | Opcode::Br(..)
            | Opcode::Upsilon(..) => Type::Void,

            Opcode::Alloca { .. } => Type::Ptr,

            Opcode::Load(ty) => ty,

            Opcode::I8(_) => Type::I8,
            Opcode::I16(_) => Type::I16,
            Opcode::I32(_) => Type::I32,
            Opcode::I64(_) => Type::I64,
            Opcode::U8(_) => Type::U8,
            Opcode::U16(_) => Type::U16,
            Opcode::U32(_) => Type::U32,
            Opcode::U64(_) => Type::U64,
            Opcode::F32(_) => Type::F32,
            Opcode::F64(_) => Type::F64,

            Opcode::Add
            | Opcode::Sub
            | Opcode::Mul
            | Opcode::Div
            | Opcode::Rem
            | Opcode::And
            | Opcode::Or
            | Opcode::Xor
            | Opcode::Shl
            | Opcode::Shr
            | Opcode::Eq
            | Opcode::Ne
            | Opcode::Gt
            | Opcode::Lt
            | Opcode::Ge
            | Opcode::Le
            | Opcode::Identity => {
                let mut types = inst.args.iter().map(|&id| self.inst_type(id));
                if let Some(first) = types.next() {
                    assert!(types.all(|t| t == first));
                    first
                } else {
                    panic!("opcode {:?} requires >= 1 argument(s)", inst.opcode);
                }
            }

            Opcode::Phi => self
                .insts
                .iter_enumerated()
                .find(|(_, inst)| match inst.opcode {
                    Opcode::Upsilon(phi_id) if phi_id == id => true,
                    _ => false,
                })
                .map(|(inst_id, _)| self.inst_type(inst_id))
                .unwrap_or(Type::Void),
        }
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
    function: RefCell<Function>,
    current_block: Option<BasicBlockId>,
}

impl FunctionBuilder {
    pub fn new() -> Self {
        Self {
            function: RefCell::new(Function {
                insts: TiVec::new(),
                blocks: TiVec::new(),
            }),
            current_block: None,
        }
    }

    pub fn build(self) -> Function {
        self.function.into_inner()
    }

    pub fn create_block(&self) -> BasicBlockId {
        let mut func = self.function.borrow_mut();
        func.blocks.push(BasicBlock { start_end: None });
        BasicBlockId::from(func.blocks.len() - 1)
    }

    pub fn switch_to_block(&mut self, block_id: BasicBlockId) {
        self.current_block = Some(block_id);
    }

    pub fn current_block(&self) -> Option<BasicBlockId> {
        self.current_block
    }

    fn expect_current_block(&self) -> BasicBlockId {
        self.current_block
            .expect("set current block with switch_to_block(id)")
    }

    pub fn alloca(&self, ty: Type) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Alloca {
                ty,
                count: 1,
                alignment: 1,
            },
            &[],
            self.expect_current_block(),
        ))
    }

    pub fn load(&self, ty: Type, src: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Load(ty),
            &[src],
            self.expect_current_block(),
        ))
    }

    pub fn store(&self, dst: InstId, value: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Store,
            &[dst, value],
            self.expect_current_block(),
        ))
    }

    pub fn constant(&self, value: impl ToConstOpcode) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            value.to_const_opcode(),
            &[],
            self.expect_current_block(),
        ))
    }

    pub fn add(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Add,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn sub(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Sub,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn mul(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Mul,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn div(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Div,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn rem(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Rem,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn and(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::And,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn or(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Or,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn xor(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Xor,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn shl(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Shl,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn shr(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Shr,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn eq(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Eq,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn ne(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Ne,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn gt(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Gt,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn lt(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Lt,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn ge(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Ge,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn le(&self, lhs: InstId, rhs: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Le,
            &[lhs, rhs],
            self.expect_current_block(),
        ))
    }

    pub fn ret(&self, value: InstId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Ret,
            &[value],
            self.expect_current_block(),
        ))
    }

    pub fn jmp(&self, target: BasicBlockId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Jmp(target),
            &[],
            self.expect_current_block(),
        ))
    }

    pub fn br(&self, cond: InstId, ift: BasicBlockId, iff: BasicBlockId) -> InstId {
        self.function.borrow_mut().append(Inst::new(
            Opcode::Br(ift, iff),
            &[cond],
            self.expect_current_block(),
        ))
    }
}
