use wraith::{amd64::Amd64Target, dce::simple_dce, ir::FunctionBuilder, ssa::convert_to_ssa};

fn main() {
    let mut builder = FunctionBuilder::new();
    let entry = builder.create_block();
    let loop_start = builder.create_block();
    let loop_body = builder.create_block();
    let loop_end = builder.create_block();

    builder.switch_to_block(entry);

    let i = builder.alloca();
    let x = builder.alloca();
    let z = builder.constant(0);
    let _ = builder.store(i, z);
    let _ = builder.store(x, z);

    let _ = builder.jmp(loop_start);

    builder.switch_to_block(loop_start);

    let n = builder.constant(10);
    let v = builder.load(i);
    let c = builder.eq(v, n);
    let _ = builder.br(c, loop_end, loop_body);

    builder.switch_to_block(loop_body);

    let v = builder.load(i);
    let o = builder.constant(1);
    let v = builder.add(v, o);
    let _ = builder.store(i, v);

    let v = builder.load(x);
    let t = builder.constant(2);
    let v = builder.add(v, t);
    let _ = builder.store(x, v);

    let _ = builder.jmp(loop_start);

    builder.switch_to_block(loop_end);

    let v = builder.load(x);
    let _ = builder.ret(v);

    let mut func = builder.build();
    func.dump();

    convert_to_ssa(&mut func);
    println!("\nConversion to SSA:\n");
    func.dump();

    simple_dce(&mut func);
    println!("\nSimple DCE:\n");
    func.dump();

    let output = Amd64Target::new(func).compile();
    println!("\nCode generation:\n\n{}", output);
}
