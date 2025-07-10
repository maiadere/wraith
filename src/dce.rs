use std::collections::{HashMap, HashSet};

use typed_index_collections::TiVec;

use crate::ir::{Function, Inst, InstId, Opcode};

pub fn simple_dce(func: &mut Function) {
    let mut insts: TiVec<InstId, Inst> = TiVec::with_capacity(func.insts.len());
    let mut id_map = HashMap::new();

    let mut used: HashSet<InstId> = func.iter_inst_ids().collect();

    loop {
        let mut changed = false;

        let mut new_used = HashSet::new();

        for inst_id in func.iter_inst_ids() {
            if !used.contains(&inst_id) {
                continue;
            }

            for i in 0..func.insts[inst_id].args.len() {
                let mut arg = func.insts[inst_id].args[i];

                while func.insts[arg].opcode == Opcode::Identity {
                    arg = func.insts[arg].args[0];
                }

                func.insts[inst_id].args[i] = arg;
                new_used.insert(arg);
            }

            match func.insts[inst_id].opcode {
                Opcode::Upsilon(phi_id) => {
                    if used.contains(&phi_id) {
                        new_used.insert(inst_id);
                    }
                }
                Opcode::Load | Opcode::Store | Opcode::Ret | Opcode::Jmp(_) | Opcode::Br(_, _) => {
                    new_used.insert(inst_id);
                }
                _ => {}
            }
        }

        if new_used != used {
            changed = true;
            used = new_used;
        }

        if !changed {
            break;
        }
    }

    for block in func.blocks.iter_mut() {
        let mut next = block.start();
        block.start_end = None;

        while let Some(id) = next {
            let mut inst = func.insts[id].clone();
            next = inst.next;

            if !used.contains(&id) {
                continue;
            }

            let new_id = InstId::from(insts.len());
            id_map.insert(id, new_id);
            inst.prev = block.start().map(|_| InstId::from(usize::from(new_id) - 1));
            inst.next = inst.next.map(|_| InstId::from(usize::from(new_id) + 1));
            match block.start_end {
                Some((start, _)) => block.start_end = Some((start, new_id)),
                None => block.start_end = Some((new_id, new_id)),
            }
            insts.push(inst);
        }
    }

    for inst in insts.iter_mut() {
        for arg in inst.args.iter_mut() {
            *arg = id_map[arg];
        }

        if let Opcode::Upsilon(id) = inst.opcode {
            inst.opcode = Opcode::Upsilon(id_map[&id]);
        }
    }

    func.insts = insts;
}
