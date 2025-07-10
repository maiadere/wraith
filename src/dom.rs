use std::collections::HashSet;

use typed_index_collections::TiVec;

use crate::ir::{BasicBlockId, Function};

pub fn dominators(func: &Function) -> TiVec<BasicBlockId, HashSet<BasicBlockId>> {
    let n = func.blocks.len();
    let mut dom = TiVec::with_capacity(n);
    dom.push(HashSet::from([BasicBlockId::from(0)]));

    for _ in 1..n {
        dom.push(func.iter_block_ids().collect());
    }

    loop {
        let mut changed = false;

        for block_id in func.iter_block_ids().skip(1) {
            let pred_dom = func
                .predecessors(block_id)
                .split_first()
                .map(|(&first, rest)| {
                    rest.iter().fold(dom[first].clone(), |mut acc, &p| {
                        acc.retain(|d| dom[p].contains(d));
                        acc
                    })
                })
                .unwrap_or_default();

            let mut new_dom = HashSet::new();
            new_dom.insert(block_id);
            new_dom.extend(pred_dom);

            if new_dom != dom[block_id] {
                dom[block_id] = new_dom;
                changed = true;
            }
        }

        if !changed {
            break;
        }
    }

    dom
}

pub fn immediate_dominators(
    dom: &TiVec<BasicBlockId, HashSet<BasicBlockId>>,
) -> TiVec<BasicBlockId, Option<BasicBlockId>> {
    let n = dom.len();
    let mut idom = TiVec::with_capacity(n);

    for i in (0..n).map(|x| BasicBlockId::from(x)) {
        if dom[i].len() <= 1 {
            idom.push(None);
            continue;
        }

        let id = dom[i]
            .iter()
            .filter(|&&d| d != i)
            .max_by_key(|&&d| dom[d].len())
            .copied();

        idom.push(id);
    }

    idom
}

pub fn dominance_frontiers(
    func: &Function,
    idom: &TiVec<BasicBlockId, Option<BasicBlockId>>,
) -> TiVec<BasicBlockId, HashSet<BasicBlockId>> {
    let n = func.blocks.len();
    let mut df = TiVec::with_capacity(n);

    for _ in 0..n {
        df.push(HashSet::new());
    }

    for i in func.iter_block_ids() {
        let preds = func.predecessors(i);

        if preds.len() < 2 {
            continue;
        }

        for p in preds {
            let mut runner = p;

            while runner != idom[i].expect("block must have immediate dominator") {
                df[runner].insert(i);
                runner = idom[runner].expect("block must have immediate dominator");
            }
        }
    }

    df
}
