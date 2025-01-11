use wraith::{
    instruction::Type,
    module::Module,
    regalloc::allocate_registers,
    target::{x86::X86, Target},
};

fn main() {
    let mut module = Module::new();

    let main = module.function("main", Type::I32);
    let x = main.mov(Type::I32, 5);
    let y = main.mov(Type::I32, 3);
    let a = main.add(Type::I32, x, y);
    let b = main.sub(Type::I32, x, y);
    let c = main.mul(Type::I32, a, b);
    let d = main.sub(Type::I32, c, x);
    let e = main.mul(Type::I32, d, d);
    main.ret(Some(e));

    allocate_registers(main, &X86);
    X86.compile(&module);
}
