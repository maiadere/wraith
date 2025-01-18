use crate::{
    function::{Function, FunctionDecl},
    instruction::{Instruction, Type},
};

pub fn params_to_stack(function_decls: &[FunctionDecl], function: &mut Function) {
    let mut new_instrs = Vec::new();

    for instr in &function.instrs {
        match instr {
            Instruction::Call(register, name, params, _) => {
                for (i, &param) in params.iter().enumerate().rev() {
                    let decl = function_decls.iter().find(|d| &d.name == name).unwrap();
                    if i >= decl.params.len() && decl.variadic && param.ty == Type::F32 {
                        todo!("cast f32 to f64 for variadic functions");
                    }
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
