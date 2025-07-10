use std::collections::{HashMap, HashSet};

use typed_index_collections::TiVec;

use crate::{
    dom::{dominance_frontiers, dominators, immediate_dominators},
    ir::{BasicBlockId, Function, InsertPosition, Inst, InstId, Opcode},
};

pub fn convert_to_ssa(func: &mut Function) {
    let dom = dominators(&func);
    let idom = immediate_dominators(&dom);
    let df = dominance_frontiers(&func, &idom);

    let mut phi_map = HashMap::new();

    for (alloca_id, mut defs) in func
        .insts
        .iter()
        .filter(|inst| inst.opcode == Opcode::Store)
        .map(|inst| (inst.args[0], inst.block_id))
        .fold(HashMap::new(), |mut map, (a, b)| {
            map.entry(a).or_insert_with(HashSet::new).insert(b);
            map
        })
    {
        let mut blocks_with_phi = HashSet::new();

        while !defs.is_empty() {
            let def = *defs.iter().next().expect("defs should not be empty");
            let def = defs.take(&def).expect("defs should not be empty");
            let Some(blocks) = df.get(def) else {
                continue;
            };

            for &block_id in blocks {
                if blocks_with_phi.contains(&block_id) {
                    continue;
                }

                let phi = Inst::new(Opcode::Phi, &[], block_id);
                let phi_id = func.insert(phi, InsertPosition::Start);
                phi_map.insert(phi_id, alloca_id);
                blocks_with_phi.insert(block_id);
                defs.insert(block_id);
            }
        }
    }

    rename(BasicBlockId::from(0), func, HashMap::new(), &phi_map, &idom);
}

fn rename(
    block_id: BasicBlockId,
    func: &mut Function,
    mut stack: HashMap<InstId, Vec<InstId>>,
    phi_map: &HashMap<InstId, InstId>,
    idom: &TiVec<BasicBlockId, Option<BasicBlockId>>,
) {
    let mut next = func.blocks[block_id].start();

    while let Some(id) = next {
        let inst = &mut func.insts[id];
        next = inst.next;

        match inst.opcode {
            Opcode::Alloca => {
                stack.insert(id, vec![]);

                inst.opcode = Opcode::Nop;
                inst.args.clear();
            }
            Opcode::Load => {
                let ids = stack
                    .get(&inst.args[0])
                    .expect("load should be preceded by alloca");

                inst.opcode = Opcode::Identity;
                inst.args[0] = *ids.iter().last().expect("load should be preceded by store");
            }
            Opcode::Store => {
                let ids = stack
                    .get_mut(&inst.args[0])
                    .expect("store should be preceded by alloca");
                ids.push(id);

                inst.opcode = Opcode::Identity;
                inst.args[0] = inst.args[1];
                inst.args.truncate(1);
            }
            Opcode::Phi => {
                let Some(alloca_id) = phi_map.get(&id) else {
                    continue;
                };
                let ids = stack
                    .get_mut(alloca_id)
                    .expect("phi should be preceded by alloca and store");
                ids.push(id);
            }
            _ => {}
        }
    }

    for s in func.successors(block_id) {
        let mut next = func.blocks[s].start();

        while let Some(id) = next {
            let inst = &func.insts[id];
            next = inst.next;

            if inst.opcode == Opcode::Phi {
                let Some(alloca_id) = phi_map.get(&id) else {
                    continue;
                };

                let def_id = *stack[alloca_id]
                    .iter()
                    .last()
                    .expect("phi should be preceded by store");

                func.insert(
                    Inst::new(Opcode::Upsilon(id), &[def_id], func.insts[def_id].block_id),
                    InsertPosition::After(def_id),
                );
            }
        }
    }

    for (b, _) in idom
        .iter_enumerated()
        .filter(|&(_, &d)| d == Some(block_id))
    {
        rename(b, func, stack.clone(), phi_map, idom);
    }
}
