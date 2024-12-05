use wraith::{module::Module, value::Constant::*, value::Type, value::Value::*};

fn main() {
    let mut module = Module::new();

    let main = module.function("main");
    let r0 = main.add(Type::I32, Constant(I32(5)), Constant(I32(8)));
    let r1 = main.add(Type::I32, r0, Constant(I32(7)));
    main.ret(r1);

    println!("{:?}", module);
}
