use std::collections::HashSet;

use crate::{
    ir::{BasicBlockId, Function, InstId, Opcode::*},
    regalloc::{Location, RegAlloc},
};

pub struct Amd64Target {
    func: Function,
    regalloc: RegAlloc,
    out: String,
    compiled_blocks: HashSet<BasicBlockId>,
    emitted_insts: HashSet<InstId>,
}

fn fmt_loc(loc: &Location) -> String {
    match loc {
        Location::Stack(offset) => format!("-{}(%rbp)", offset),
        Location::Register(reg) => format!("%{}", reg),
        Location::Const(value) => format!("${}", value),
    }
}

macro_rules! emit {
    ($target:expr, $($t:tt)*) => {{
        $target.out += &format!($($t)*);
    }};
}

impl Amd64Target {
    pub fn new(func: Function) -> Self {
        Self {
            regalloc: RegAlloc::new(
                &func,
                vec![
                    "r11".into(),
                    "r10".into(),
                    "r9".into(),
                    "r8".into(),
                    "rsi".into(),
                    "rdi".into(),
                    "rdx".into(),
                    "rcx".into(),
                ],
            ),
            func,
            out: String::new(),
            compiled_blocks: HashSet::new(),
            emitted_insts: HashSet::new(),
        }
    }

    pub fn compile(mut self) -> String {
        let output = self.compile_block(BasicBlockId::from(0));
        let stack_size = self.regalloc.stack_size();
        if stack_size == 0 {
            output
        } else {
            format!(
                "\tpush %rbp\n\tmov %rsp, %rbp\n\tsub ${}, %rsp\n{}",
                ((stack_size - 1) | 15) + 1,
                output.replace("\tret\n", "\tmov %rbp, %rsp\n\tpop %rbp\n\tret\n")
            )
        }
    }

    fn compile_block(&mut self, block_id: BasicBlockId) -> String {
        if !self.compiled_blocks.insert(block_id) {
            return String::new();
        }

        let mut prev = self.func.blocks[block_id].end();
        let mut block_output = Vec::new();

        while let Some(id) = prev {
            prev = self.func.insts[id].prev;
            self.out.clear();
            self.emit_inst(id);
            block_output.push(self.out.clone());
        }

        let mut output = format!(
            ".bb{}:\n{}",
            block_id,
            block_output.into_iter().rev().collect::<String>()
        );

        for successor in self.func.successors(block_id) {
            output += &self.compile_block(successor);
        }

        output
    }

    fn emit_inst(&mut self, id: InstId) {
        if self.emitted_insts.remove(&id) {
            return;
        }

        let inst = &self.func.insts[id];

        if self.regalloc.is_local(id) {
            self.regalloc.free(id);
        }

        match inst.opcode {
            Nop | Alloca | Const(_) => {}
            Phi => {}
            Upsilon(phi_id) => {
                let dst = self.regalloc.get(phi_id);
                let src = self.regalloc.get(inst.args[0]);
                self.emit_op("mov", &src, &dst);
            }
            Load | Identity => {
                let dst = self.regalloc.get(id);
                let src = self.regalloc.get(inst.args[0]);
                self.emit_op("mov", &src, &dst);
            }
            Store => {
                let dst = self.regalloc.get(inst.args[0]);
                let src = self.regalloc.get(inst.args[1]);
                self.emit_op("mov", &src, &dst);
            }
            Add => {
                let dst = self.regalloc.get(id);
                let lhs = self.regalloc.get(inst.args[0]);
                let rhs = self.regalloc.get(inst.args[1]);
                self.emit_op("mov", &lhs, &dst);
                self.emit_op("add", &rhs, &dst);
            }
            Eq => {
                let dst = self.regalloc.get(id);
                let lhs = self.regalloc.get(inst.args[0]);
                let rhs = self.regalloc.get(inst.args[1]);
                self.emit_op("cmp", &lhs, &rhs);
                emit!(self, "\tsete %al\n");
                emit!(self, "\tmovzx %al, {}\n", fmt_loc(&dst));
            }
            Ret => {
                let src = self.regalloc.get(inst.args[0]);
                let rax = Location::Register("rax".to_string());
                self.emit_op("mov", &src, &rax);
                emit!(self, "\tret\n");
            }
            Jmp(target) => {
                if self.compiled_blocks.contains(&target) {
                    emit!(self, "\tjmp .bb{}\n", target)
                }
            }
            Br(ift, iff) => {
                let cond_id = inst.args[0];
                let cond_inst = &self.func.insts[cond_id];
                let use_count = self.regalloc.get_use_count(cond_id);

                if use_count == 1 && inst.prev == Some(cond_id) && cond_inst.opcode == Eq {
                    let lhs = self.regalloc.get(cond_inst.args[0]);
                    let rhs = self.regalloc.get(cond_inst.args[1]);
                    self.emitted_insts.insert(cond_id);
                    self.emit_op("cmp", &lhs, &rhs);
                    emit!(self, "\tjne .bb{}\n", iff);
                } else {
                    let cond = self.regalloc.get(cond_id);
                    self.emit_op("test", &cond, &cond);
                    emit!(self, "\tjz .bb{}\n", iff);
                }

                if self.compiled_blocks.contains(&ift) {
                    emit!(self, "\tjmp .bb{}\n", ift);
                }
            }
        }
    }

    fn emit_op(&mut self, op: &str, src: &Location, dst: &Location) {
        if op == "mov" && src == dst && src.is_reg() {
            return;
        }

        let mut src = src.clone();
        let mut dst = dst.clone();

        if dst.is_const() {
            (src, dst) = (dst, src);
        }

        if dst.is_const() {
            panic!("dst should not be constant");
        }

        let (s, d) = (fmt_loc(&src), fmt_loc(&dst));

        if src.is_stack() && dst.is_stack() {
            emit!(self, "\t{} {}, %rax\n", op, s);
            emit!(self, "\t{} %rax, {}\n", op, d);
        } else {
            emit!(self, "\t{} {}, {}\n", op, s, d);
        }
    }
}
