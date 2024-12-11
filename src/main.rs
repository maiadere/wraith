use wraith::{
    instruction::{Constant::*, Type, Value::Constant},
    module::ModuleBuilder,
};

fn main() {
    let mut module = ModuleBuilder::new();

    let main = module.function("main");
    let r0 = main.add(Type::I32, Constant(I32(5)), Constant(I32(8)));
    let r1 = main.add(Type::I32, r0, Constant(I32(7)));
    let r2 = main.add(Type::I32, r0, r1);
    main.ret(r2);

    dbg!(module);
}
