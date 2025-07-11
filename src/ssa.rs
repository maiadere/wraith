use std::collections::{HashMap, HashSet};

use typed_index_collections::TiVec;

use crate::{
    dom::{dominance_frontiers, dominators, immediate_dominators},
    ir::{BasicBlockId, Function, InsertPosition, Inst, InstId, Opcode},
};

pub fn find_allocas(func: &Function) -> HashMap<InstId, HashSet<BasicBlockId>> {
    let mut defs = HashMap::new();

    let mut next = func.blocks[BasicBlockId::from(0)].start();

    while let Some(id) = next {
        let inst = &func.insts[id];
        next = inst.next;

        if let Opcode::Alloca { count: 1, .. } = inst.opcode {
            defs.insert(id, HashSet::new());
        }
    }

    for inst in func.insts.iter() {
        match inst.opcode {
            Opcode::Store => {
                let dst = inst.args[0];
                let src = inst.args[1];
                let src_ty = func.inst_type(src);

                match func.insts[dst].opcode {
                    Opcode::Alloca { ty, count: 1, .. } if ty == src_ty => {
                        if let Some(alloca_defs) = defs.get_mut(&dst) {
                            alloca_defs.insert(inst.block_id);
                        }
                    }
                    Opcode::Alloca { .. } => {
                        defs.remove(&dst);
                    }
                    _ => {}
                }
            }
            Opcode::Load(load_ty) => {
                let src = inst.args[0];

                match func.insts[src].opcode {
                    Opcode::Alloca { ty, count: 1, .. } if ty != load_ty => {
                        defs.remove(&src);
                    }
                    _ => {}
                }
            }
            _ => {}
        }
    }

    defs
}

pub fn convert_to_ssa(func: &mut Function) {
    let dom = dominators(&func);
    let idom = immediate_dominators(&dom);
    let df = dominance_frontiers(&func, &idom);

    let mut allocas = HashSet::new();
    let mut phi_map = HashMap::new();

    for (alloca_id, mut defs) in find_allocas(func) {
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

        allocas.insert(alloca_id);
    }

    rename(
        BasicBlockId::from(0),
        func,
        &allocas,
        HashMap::new(),
        &phi_map,
        &idom,
    );
}

fn rename(
    block_id: BasicBlockId,
    func: &mut Function,
    allocas: &HashSet<InstId>,
    mut stack: HashMap<InstId, Vec<InstId>>,
    phi_map: &HashMap<InstId, InstId>,
    idom: &TiVec<BasicBlockId, Option<BasicBlockId>>,
) {
    let mut next = func.blocks[block_id].start();

    while let Some(id) = next {
        let inst = &mut func.insts[id];
        next = inst.next;

        match inst.opcode {
            Opcode::Alloca { .. } if allocas.contains(&id) => {
                stack.insert(id, vec![]);

                inst.opcode = Opcode::Nop;
                inst.args.clear();
            }
            Opcode::Load(..) => {
                let Some(ids) = stack.get(&inst.args[0]) else {
                    continue;
                };
                if let Some(&last) = ids.iter().last() {
                    inst.opcode = Opcode::Identity;
                    inst.args[0] = last;
                } else {
                    todo!("ub: load before store")
                }
            }
            Opcode::Store => {
                let Some(ids) = stack.get_mut(&inst.args[0]) else {
                    continue;
                };
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
        rename(b, func, allocas, stack.clone(), phi_map, idom);
    }
}
