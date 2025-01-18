use wraith::{
    global_var::GlobalVarKind,
    instruction::{Constant, Type},
    module::Module,
    params_to_stack::params_to_stack,
    regalloc::allocate_registers,
    target::{x86::X86, Target},
};

fn main() {
    let mut module = Module::new();

    let printf = module.external_function("printf", Type::I32);
    printf.add_param(Type::I8);
    printf.set_variadic(true);

    let fmt = module.global_var("fmt", GlobalVarKind::Const);
    for &b in b"%f\n\0" {
        fmt.add_data(Type::I8, Constant::Int(b as i128));
    }
    let fmt = fmt.clone();

    let main = module.function("main", Type::I32);
    let x = main.mov(Type::F64, 5.0);
    let y = main.mov(Type::F64, 3.0);
    let a = main.add(Type::F64, x, y);
    let b = main.sub(Type::F64, x, y);
    let c = main.mul(Type::F64, a, b);
    let d = main.sub(Type::F64, c, x);
    let e = main.mul(Type::F64, d, d);
    let f = main.lea(Type::I32, fmt);
    main.call(Type::I32, "printf", vec![f, e]);
    main.ret(Some(Constant::Int(0)));

    println!("{}", module);

    let function_decls = module.function_decls().cloned().collect::<Vec<_>>();
    for function in module.functions.iter_mut() {
        params_to_stack(&function_decls, function);
        allocate_registers(function, &X86);
    }
    X86.compile(&module);
}
