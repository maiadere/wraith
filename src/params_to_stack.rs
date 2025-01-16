use crate::{function::Function, instruction::Instruction};

pub fn params_to_stack(function: &mut Function) {
    let mut new_instrs = Vec::new();

    for instr in &function.instrs {
        match instr {
            Instruction::Call(register, name, params, _) => {
                for &param in params.iter().rev() {
                    new_instrs.push(Instruction::Push(param));
                }
                new_instrs.push(Instruction::Call(
                    register.clone(),
                    name.clone(),
                    params.clone(),
                    true,
                ));
            }
            _ => new_instrs.push(instr.clone()),
        }
    }

    function.instrs = new_instrs;
}
