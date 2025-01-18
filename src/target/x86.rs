use crate::{
    function::Function,
    global_var::GlobalVarKind,
    instruction::{
        Constant, Instruction, Label, MemorySlot, Pointer, Register, RegisterId,
        Type::{self, *},
        Value,
    },
    module::Module,
};

use std::fmt::Write;
use std::{collections::HashMap, ops::RangeFrom};

use super::{RegisterKind::*, Target, TargetRegisterInfo};

pub struct X86;

const EAX: RegisterId = 0;
const ECX: RegisterId = 1;
const EDX: RegisterId = 2;
const EBX: RegisterId = 3;
const ESP: RegisterId = 4;
const EBP: RegisterId = 5;
const ESI: RegisterId = 6;
const EDI: RegisterId = 7;

impl Target for X86 {
    fn registers(&self) -> TargetRegisterInfo {
        let mut regs = TargetRegisterInfo::new();

        // caller saved
        regs.add(EAX, I8 | I16 | I32, CallerSaved);
        regs.add(ECX, I8 | I16 | I32, CallerSaved);
        regs.add(EDX, I8 | I16 | I32, CallerSaved);

        // callee saved
        regs.add(EBX, I8 | I16 | I32, CalleeSaved);
        regs.add(ESI, I16 | I32, CalleeSaved);
        regs.add(EDI, I16 | I32, CalleeSaved);

        // allow i64, f32, or f64 virtual regs to stay virtual
        // for spilling without IR's load/store instructions
        regs.keep_virtual(I64 | F32 | F64);
        regs
    }

    fn compile(&self, module: &Module) {
        X86Module::new().compile(module);
    }

    fn get_clobbered_registers(&self, instr: &Instruction) -> &[RegisterId] {
        match instr {
            Instruction::Sdiv(dst, ..) | Instruction::Udiv(dst, ..) => match dst.ty {
                Void => unreachable!(),
                I8 => &[EAX],
                I16 | I32 => &[EAX, EDX],
                I64 | F32 | F64 => todo!(),
            },
            Instruction::Call(..) => &[EAX, ECX, EDX],
            _ => &[],
        }
    }
}

#[derive(Debug, Clone)]
enum X86Instruction {
    LoadReg(Register, Register),
    LoadMem(Register, i128),
    LoadGlobal(Register, String),
    StoreRegReg(Register, Register),
    StoreRegImm(Type, Register, i128),
    StoreMemReg(i128, Register),
    StoreMemImm(Type, i128, i128),
    StoreGlobalReg(String, Register),
    StoreGlobalImm(Type, String, i128),
    LeaMem(Register, i128),
    LeaGlobal(Register, String),
    PushImm(i128),
    MovImm(Register, i128),
    AddImm(Register, i128),
    SubImm(Register, i128),
    Push(Register),
    Pop(Register),
    Neg(Register),
    Mov(Register, Register),
    Add(Register, Register),
    Sub(Register, Register),
    Xor(Register, Register),
    Imul(Register, Register),
    ImulImm(Register, Register, i128),
    Cbw,
    Cwd,
    Cdq,
    Idiv(Register),
    Div(Register),
    Test(Register, Register),
    Label(Label),
    Jmp(Label),
    Jcc(X86Condition, Label),
    Call(String),
    Ret,
    Leave,
}

#[derive(Debug, Clone, Copy)]
enum X86Condition {
    Zero,
    NotZero,
    Less,
    LessOrEqual,
    Greater,
    GreaterOrEqual,
}

impl std::fmt::Display for X86Condition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            X86Condition::Zero => write!(f, "z"),
            X86Condition::NotZero => write!(f, "nz"),
            X86Condition::Less => write!(f, "l"),
            X86Condition::LessOrEqual => write!(f, "le"),
            X86Condition::Greater => write!(f, "g"),
            X86Condition::GreaterOrEqual => write!(f, "ge"),
        }
    }
}

fn type_ptr(ty: Type) -> &'static str {
    match ty {
        I8 => "byte ptr",
        I16 => "word ptr",
        I32 => "dword ptr",
        I64 => "qword ptr",
        F32 => "dword ptr",
        F64 => "qword ptr",
        Void => unreachable!(),
    }
}

impl std::fmt::Display for X86Instruction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.clone() {
            X86Instruction::LoadReg(r1, r2) => {
                write!(f, "mov {}, [{}]", reg(r1), reg(r2))
            }
            X86Instruction::LoadMem(register, offset) => {
                let sign = if offset < 0 { "-" } else { "+" };
                write!(f, "mov {}, [ebp {} {}]", reg(register), sign, offset.abs())
            }
            X86Instruction::LoadGlobal(register, name) => {
                write!(f, "mov {}, [{}]", reg(register), name)
            }
            X86Instruction::StoreRegReg(r1, r2) => {
                write!(f, "mov [{}], {}", reg(r1), reg(r2))
            }
            X86Instruction::StoreRegImm(ty, register, imm) => {
                write!(f, "mov {} [{}], {}", type_ptr(ty), reg(register), imm)
            }
            X86Instruction::StoreMemReg(offset, register) => {
                let sign = if offset < 0 { "-" } else { "+" };
                write!(f, "mov [ebp {} {}], {}", sign, offset.abs(), reg(register))
            }
            X86Instruction::StoreMemImm(ty, offset, imm) => {
                let sign = if offset < 0 { "-" } else { "+" };
                let offset = offset.abs();
                write!(f, "mov {} [ebp {} {}], {}", type_ptr(ty), sign, offset, imm)
            }
            X86Instruction::StoreGlobalReg(name, register) => {
                write!(f, "mov [{}], {}", name, reg(register))
            }
            X86Instruction::StoreGlobalImm(ty, name, imm) => {
                write!(f, "mov {} [{}], {}", type_ptr(ty), name, imm)
            }
            X86Instruction::LeaMem(register, offset) => {
                let sign = if offset < 0 { "-" } else { "+" };
                write!(f, "lea {}, [ebp {} {}]", reg(register), sign, offset.abs())
            }
            X86Instruction::LeaGlobal(register, name) => {
                write!(f, "lea {}, [{}]", reg(register), name)
            }
            X86Instruction::PushImm(imm) => write!(f, "push {}", imm),
            X86Instruction::MovImm(register, imm) => write!(f, "mov {}, {}", reg(register), imm),
            X86Instruction::AddImm(register, imm) => write!(f, "add {}, {}", reg(register), imm),
            X86Instruction::SubImm(register, imm) => write!(f, "sub {}, {}", reg(register), imm),
            X86Instruction::Push(register) => write!(f, "push {}", reg(register)),
            X86Instruction::Pop(register) => write!(f, "pop {}", reg(register)),
            X86Instruction::Neg(register) => write!(f, "neg {}", reg(register)),
            X86Instruction::Mov(r1, r2) => write!(f, "mov {}, {}", reg(r1), reg(r2)),
            X86Instruction::Add(r1, r2) => write!(f, "add {}, {}", reg(r1), reg(r2)),
            X86Instruction::Sub(r1, r2) => write!(f, "sub {}, {}", reg(r1), reg(r2)),
            X86Instruction::Xor(r1, r2) => write!(f, "xor {}, {}", reg(r1), reg(r2)),
            X86Instruction::Imul(r1, r2) => write!(f, "imul {}, {}", reg(r1), reg(r2)),
            X86Instruction::ImulImm(r1, r2, imm) => {
                write!(f, "imul {}, {}, {}", reg(r1), reg(r2), imm)
            }
            X86Instruction::Idiv(register) => write!(f, "idiv {}", reg(register)),
            X86Instruction::Div(register) => write!(f, "div {}", reg(register)),
            X86Instruction::Test(r1, r2) => write!(f, "test {}, {}", reg(r1), reg(r2)),
            X86Instruction::Label(label) => write!(f, ".L{}:", label.id),
            X86Instruction::Jmp(label) => write!(f, "jmp .L{}", label.id),
            X86Instruction::Jcc(x86_cc, label) => write!(f, "j{} .L{}", x86_cc, label.id),
            X86Instruction::Ret => write!(f, "ret"),
            X86Instruction::Leave => write!(f, "leave"),
            X86Instruction::Cbw => write!(f, "cbw"),
            X86Instruction::Cwd => write!(f, "cwd"),
            X86Instruction::Cdq => write!(f, "cdq"),
            X86Instruction::Call(name) => write!(f, "call {}", name),
        }
    }
}

struct X86Function {
    name: String,
    instrs: Vec<X86Instruction>,
    memory_slots: HashMap<MemorySlot, i128>,
    memory_slot_offset: i128,
    uses_params: bool,
}

impl X86Function {
    fn new(name: String) -> Self {
        Self {
            name,
            instrs: Vec::new(),
            memory_slots: HashMap::new(),
            memory_slot_offset: 0,
            uses_params: false,
        }
    }

    fn push(&mut self, instr: X86Instruction) {
        self.instrs.push(instr);
    }
}

struct X86Module {
    functions: Vec<X86Function>,
    lbl_id: RangeFrom<usize>,
    local_labels: HashMap<Label, Label>,
    global_var_symbols: HashMap<String, String>,
}

impl X86Module {
    fn new() -> Self {
        Self {
            functions: Vec::new(),
            lbl_id: 0..,
            local_labels: HashMap::new(),
            global_var_symbols: HashMap::new(),
        }
    }

    fn compile(&mut self, module: &Module) {
        for var in &module.global_vars {
            let symbol = match var.kind {
                GlobalVarKind::Public => var.name.clone(),
                GlobalVarKind::Private => format!(".L{}", var.name),
                GlobalVarKind::Const => format!(".LC{}", var.name),
            };
            self.global_var_symbols.insert(var.name.clone(), symbol);
        }

        for var in &module.external_vars {
            self.global_var_symbols
                .insert(var.name.clone(), var.name.clone());
        }

        for func in &module.functions {
            self.compile_function(func);
        }

        for func in self.functions.iter_mut() {
            func.instrs = peephole_optimize(&func.instrs);
        }

        println!("{}", self.emit_asm(&module).unwrap());
    }

    fn label(&mut self, label: Label) -> Label {
        *self.local_labels.entry(label).or_insert_with(|| Label {
            id: self.lbl_id.next().expect("ran out of label IDs"),
        })
    }

    fn compile_function(&mut self, func: &Function) {
        let mut used_regs = vec![];
        let mut x86_func = X86Function::new(func.decl.name.clone());

        self.local_labels.clear();

        for instr in &func.instrs {
            for reg in instr.used_regs() {
                if used_regs.contains(&reg.id) {
                    continue;
                }
                if reg.ty == I8 || reg.ty == I16 || reg.ty == I32 {
                    used_regs.push(reg.id);
                }
            }

            match instr.clone() {
                Instruction::Alloc(slot, ty, count) => {
                    let size = ty.size() * count;
                    x86_func.memory_slot_offset -= size as i128;
                    let offset = x86_func.memory_slot_offset;
                    x86_func.memory_slots.insert(slot, offset);
                }
                Instruction::Load(register, Pointer::MemorySlot(slot)) => {
                    let offset = *x86_func.memory_slots.get(&slot).unwrap();
                    x86_func.push(X86Instruction::LoadMem(register, offset));
                }
                Instruction::Load(register, Pointer::Register(ptr)) => {
                    x86_func.push(X86Instruction::LoadReg(register, ptr));
                }
                Instruction::Load(register, Pointer::GlobalVar(name)) => {
                    let symbol = self.global_var_symbols.get(&name).unwrap().clone();
                    x86_func.push(X86Instruction::LoadGlobal(register, symbol));
                }
                Instruction::LoadParam(register, param) => {
                    let mut offset = 8;

                    for p in &func.decl.params {
                        if p.id == param {
                            break;
                        }

                        offset += param_stack_size(p.ty);
                    }

                    x86_func.uses_params = true;
                    x86_func.push(X86Instruction::LoadMem(register, offset));
                }
                Instruction::Store(ty, Pointer::MemorySlot(ptr), Value::Constant(imm)) => {
                    let imm = const_to_bits(ty, imm);
                    let mem = *x86_func.memory_slots.get(&ptr).unwrap();
                    x86_func.push(X86Instruction::StoreMemImm(ty, mem, imm));
                }
                Instruction::Store(_, Pointer::MemorySlot(ptr), Value::Register(register)) => {
                    let mem = *x86_func.memory_slots.get(&ptr).unwrap();
                    x86_func.push(X86Instruction::StoreMemReg(mem, register));
                }
                Instruction::Store(ty, Pointer::Register(ptr), Value::Constant(imm)) => {
                    let imm = const_to_bits(ty, imm);
                    x86_func.push(X86Instruction::StoreRegImm(ty, ptr, imm));
                }
                Instruction::Store(_, Pointer::Register(ptr), Value::Register(register)) => {
                    x86_func.push(X86Instruction::StoreRegReg(ptr, register));
                }
                Instruction::Store(ty, Pointer::GlobalVar(name), Value::Constant(imm)) => {
                    let symbol = self.global_var_symbols.get(&name).unwrap().clone();
                    let imm = const_to_bits(ty, imm);
                    x86_func.push(X86Instruction::StoreGlobalImm(ty, symbol, imm));
                }
                Instruction::Store(_, Pointer::GlobalVar(name), Value::Register(register)) => {
                    let symbol = self.global_var_symbols.get(&name).unwrap().clone();
                    x86_func.push(X86Instruction::StoreGlobalReg(symbol, register));
                }

                Instruction::Lea(register, Pointer::MemorySlot(slot)) => {
                    let offset = *x86_func.memory_slots.get(&slot).unwrap();
                    x86_func.push(X86Instruction::LeaMem(register, offset));
                }

                Instruction::Lea(register, Pointer::Register(ptr)) => {
                    x86_func.push(X86Instruction::Mov(register, ptr));
                }

                Instruction::Lea(register, Pointer::GlobalVar(name)) => {
                    let symbol = self.global_var_symbols.get(&name).unwrap().clone();
                    x86_func.push(X86Instruction::LeaGlobal(register, symbol));
                }

                Instruction::Mov(r1, Value::Register(r2)) => {
                    x86_func.push(X86Instruction::Mov(r1, r2));
                }
                Instruction::Mov(register, Value::Constant(imm)) => {
                    x86_func.push(X86Instruction::MovImm(
                        register,
                        const_to_bits(register.ty, imm),
                    ));
                }

                Instruction::Add(d, ..) if d.ty == I64 || d.ty == F32 || d.ty == F64 => todo!(),
                Instruction::Sub(d, ..) if d.ty == I64 || d.ty == F32 || d.ty == F64 => todo!(),
                Instruction::Mul(d, ..) if d.ty == I64 || d.ty == F32 || d.ty == F64 => todo!(),
                Instruction::Sdiv(d, ..) if d.ty == I64 || d.ty == F32 || d.ty == F64 => todo!(),
                Instruction::Udiv(d, ..) if d.ty == I64 || d.ty == F32 || d.ty == F64 => todo!(),

                Instruction::Add(dst, Value::Register(r1), Value::Register(r2)) => {
                    if dst != r2 {
                        x86_func.push(X86Instruction::Mov(dst, r1));
                        x86_func.push(X86Instruction::Add(dst, r2));
                    } else {
                        x86_func.push(X86Instruction::Mov(dst, r2));
                        x86_func.push(X86Instruction::Add(dst, r1));
                    }
                }
                Instruction::Add(dst, Value::Register(register), Value::Constant(imm))
                | Instruction::Add(dst, Value::Constant(imm), Value::Register(register)) => {
                    let imm = const_to_bits(dst.ty, imm);
                    x86_func.push(X86Instruction::Mov(dst, register));
                    x86_func.push(X86Instruction::AddImm(dst, imm));
                }

                Instruction::Sub(dst, Value::Register(r1), Value::Register(r2)) => {
                    if dst != r2 {
                        x86_func.push(X86Instruction::Mov(dst, r1));
                        x86_func.push(X86Instruction::Sub(dst, r2));
                    } else {
                        x86_func.push(X86Instruction::Neg(r2));
                        x86_func.push(X86Instruction::Mov(dst, r2));
                        x86_func.push(X86Instruction::Add(dst, r1));
                    }
                }

                Instruction::Sub(dst, Value::Register(register), Value::Constant(imm))
                | Instruction::Sub(dst, Value::Constant(imm), Value::Register(register)) => {
                    let imm = const_to_bits(dst.ty, imm);
                    x86_func.push(X86Instruction::Mov(dst, register));
                    x86_func.push(X86Instruction::SubImm(dst, imm));
                }

                Instruction::Mul(dst, Value::Register(r1), Value::Register(r2)) => {
                    let dst = Register::new(dst.id, I32);
                    let r1 = Register::new(r1.id, I32);
                    let r2 = Register::new(r2.id, I32);

                    if dst != r2 {
                        x86_func.push(X86Instruction::Mov(dst, r1));
                        x86_func.push(X86Instruction::Imul(dst, r2));
                    } else {
                        x86_func.push(X86Instruction::Mov(dst, r2));
                        x86_func.push(X86Instruction::Imul(dst, r1));
                    }
                }

                Instruction::Mul(dst, Value::Register(register), Value::Constant(imm))
                | Instruction::Mul(dst, Value::Constant(imm), Value::Register(register)) => {
                    let dst = Register::new(dst.id, I32);
                    let register = Register::new(register.id, I32);
                    let imm = const_to_bits(dst.ty, imm);
                    x86_func.push(X86Instruction::ImulImm(dst, register, imm));
                }

                Instruction::Sdiv(dst, Value::Register(r1), Value::Register(r2)) => {
                    if r2.id == EAX && dst.id != EBX {
                        x86_func.push(X86Instruction::Push(Register::new(EBX, I32)));
                        x86_func.push(X86Instruction::Mov(Register::new(EBX, r2.ty), r2));
                    }

                    let eax = Register::new(EAX, dst.ty);
                    x86_func.push(X86Instruction::Mov(eax, r1));

                    match dst.ty {
                        I8 => x86_func.push(X86Instruction::Cbw),
                        I16 => x86_func.push(X86Instruction::Cwd),
                        I32 => x86_func.push(X86Instruction::Cdq),
                        _ => unreachable!(),
                    }

                    if r2.id == EAX && dst.id != EBX {
                        x86_func.push(X86Instruction::Idiv(Register::new(EBX, r2.ty)));
                        x86_func.push(X86Instruction::Pop(Register::new(EBX, I32)));
                    } else {
                        x86_func.push(X86Instruction::Idiv(r2));
                    }

                    x86_func.push(X86Instruction::Mov(dst, eax));
                }

                Instruction::Sdiv(dst, Value::Register(r1), Value::Constant(imm)) => {
                    let imm = const_to_bits(dst.ty, imm);

                    if dst.id != EBX {
                        x86_func.push(X86Instruction::Push(Register::new(EBX, I32)));
                    }

                    x86_func.push(X86Instruction::MovImm(Register::new(EBX, r1.ty), imm));

                    let eax = Register::new(EAX, dst.ty);
                    x86_func.push(X86Instruction::Mov(eax, r1));

                    match dst.ty {
                        I8 => x86_func.push(X86Instruction::Cbw),
                        I16 => x86_func.push(X86Instruction::Cwd),
                        I32 => x86_func.push(X86Instruction::Cdq),
                        _ => unreachable!(),
                    }

                    x86_func.push(X86Instruction::Idiv(Register::new(EBX, r1.ty)));

                    if dst.id != EBX {
                        x86_func.push(X86Instruction::Pop(Register::new(EBX, I32)));
                    }

                    x86_func.push(X86Instruction::Mov(dst, eax));
                }

                Instruction::Sdiv(dst, Value::Constant(imm), Value::Register(r1)) => {
                    let imm = const_to_bits(dst.ty, imm);

                    if dst.id != EBX {
                        x86_func.push(X86Instruction::Push(Register::new(EBX, I32)));
                    }

                    x86_func.push(X86Instruction::Mov(Register::new(EBX, r1.ty), r1));

                    let eax = Register::new(EAX, dst.ty);
                    x86_func.push(X86Instruction::MovImm(eax, imm));

                    match dst.ty {
                        I8 => x86_func.push(X86Instruction::Cbw),
                        I16 => x86_func.push(X86Instruction::Cwd),
                        I32 => x86_func.push(X86Instruction::Cdq),
                        _ => unreachable!(),
                    }

                    x86_func.push(X86Instruction::Idiv(Register::new(EBX, r1.ty)));

                    if dst.id != EBX {
                        x86_func.push(X86Instruction::Pop(Register::new(EBX, I32)));
                    }

                    x86_func.push(X86Instruction::Mov(dst, eax));
                }

                Instruction::Udiv(dst, Value::Register(r1), Value::Register(r2)) => {
                    if r2.id == EAX && dst.id != EBX {
                        x86_func.push(X86Instruction::Push(Register::new(EBX, I32)));
                        x86_func.push(X86Instruction::Mov(Register::new(EBX, r2.ty), r2));
                    }

                    match dst.ty {
                        I8 => x86_func.push(X86Instruction::MovImm(Register::new(EAX, I32), 0)),
                        I16 | I32 => {
                            x86_func.push(X86Instruction::MovImm(Register::new(EDX, I32), 0))
                        }
                        _ => unreachable!(),
                    }

                    let eax = Register::new(EAX, dst.ty);
                    x86_func.push(X86Instruction::Mov(eax, r1));

                    if r2.id == EAX && dst.id != EBX {
                        x86_func.push(X86Instruction::Div(Register::new(EBX, r2.ty)));
                        x86_func.push(X86Instruction::Pop(Register::new(EBX, I32)));
                    } else {
                        x86_func.push(X86Instruction::Div(r2));
                    }

                    x86_func.push(X86Instruction::Mov(dst, eax));
                }

                Instruction::Udiv(dst, Value::Register(r1), Value::Constant(imm)) => {
                    let imm = const_to_bits(dst.ty, imm);

                    if dst.id != EBX {
                        x86_func.push(X86Instruction::Push(Register::new(EBX, I32)));
                    }

                    x86_func.push(X86Instruction::MovImm(Register::new(EBX, r1.ty), imm));

                    match dst.ty {
                        I8 => x86_func.push(X86Instruction::MovImm(Register::new(EAX, I32), 0)),
                        I16 | I32 => {
                            x86_func.push(X86Instruction::MovImm(Register::new(EDX, I32), 0))
                        }
                        _ => unreachable!(),
                    }

                    let eax = Register::new(EAX, dst.ty);
                    x86_func.push(X86Instruction::Mov(eax, r1));
                    x86_func.push(X86Instruction::Div(Register::new(EBX, r1.ty)));

                    if dst.id != EBX {
                        x86_func.push(X86Instruction::Pop(Register::new(EBX, I32)));
                    }

                    x86_func.push(X86Instruction::Mov(dst, eax));
                }

                Instruction::Udiv(dst, Value::Constant(imm), Value::Register(r1)) => {
                    let imm = const_to_bits(dst.ty, imm);

                    if dst.id != EBX {
                        x86_func.push(X86Instruction::Push(Register::new(EBX, I32)));
                    }

                    x86_func.push(X86Instruction::Mov(Register::new(EBX, r1.ty), r1));

                    match dst.ty {
                        I8 => x86_func.push(X86Instruction::MovImm(Register::new(EAX, I32), 0)),
                        I16 | I32 => {
                            x86_func.push(X86Instruction::MovImm(Register::new(EDX, I32), 0))
                        }
                        _ => unreachable!(),
                    }

                    let eax = Register::new(EAX, dst.ty);
                    x86_func.push(X86Instruction::MovImm(eax, imm));
                    x86_func.push(X86Instruction::Div(Register::new(EBX, r1.ty)));

                    if dst.id != EBX {
                        x86_func.push(X86Instruction::Pop(Register::new(EBX, I32)));
                    }

                    x86_func.push(X86Instruction::Mov(dst, eax));
                }

                // TODO: f32/f64/i64
                Instruction::Push(register) => {
                    let register = Register::new(register.id, I32);
                    x86_func.push(X86Instruction::Push(register));
                }
                Instruction::Pop(register) => {
                    let register = Register::new(register.id, I32);
                    x86_func.push(X86Instruction::Pop(register));
                }

                Instruction::Add(..) => todo!(),
                Instruction::Sub(..) => todo!(),
                Instruction::Mul(..) => todo!(),
                Instruction::Sdiv(..) => todo!(),
                Instruction::Udiv(..) => todo!(),

                Instruction::Call(register, name, params, _) => {
                    if register.ty == I64 || register.ty == F32 || register.ty == F64 {
                        todo!()
                    }

                    x86_func.push(X86Instruction::Call(name));

                    if register.ty != Void {
                        let eax = Register::new(EAX, register.ty);
                        x86_func.push(X86Instruction::Mov(register, eax));
                    }

                    let mut sz = 0;

                    for param in params.iter().rev() {
                        sz += param_stack_size(param.ty);
                    }

                    if sz != 0 {
                        x86_func.push(X86Instruction::AddImm(Register::new(ESP, I32), sz));
                    }
                }

                Instruction::Label(lbl) => x86_func.push(X86Instruction::Label(self.label(lbl))),
                Instruction::Jump(lbl) => x86_func.push(X86Instruction::Jmp(self.label(lbl))),
                Instruction::Branch(register, ift, iff) => {
                    x86_func.push(X86Instruction::Test(register, register));
                    x86_func.push(X86Instruction::Jcc(X86Condition::NotZero, self.label(ift)));
                    if let Some(iff) = iff {
                        x86_func.push(X86Instruction::Jmp(self.label(iff)));
                    }
                }

                Instruction::Ret(Some(Value::Constant(imm))) => {
                    let ty = func.decl.ty;
                    let imm = const_to_bits(ty, imm);
                    x86_func.push(X86Instruction::MovImm(Register::new(EAX, ty), imm));
                    x86_func.push(X86Instruction::Ret);
                }
                Instruction::Ret(Some(Value::Register(r))) => {
                    x86_func.push(X86Instruction::Mov(Register::new(EAX, r.ty), r));
                    x86_func.push(X86Instruction::Ret);
                }
                Instruction::Ret(None) => {
                    x86_func.push(X86Instruction::Ret);
                }
            }
        }

        used_regs.retain(|&id| X86.registers().is_callee_saved(id));
        add_prologue_and_epilogue(&mut x86_func, &used_regs);
        self.functions.push(x86_func);
    }

    fn emit_asm(&self, module: &Module) -> Result<String, std::fmt::Error> {
        let mut asm = String::new();

        writeln!(asm, "\t.text")?;
        writeln!(asm, "\t.intel_syntax noprefix")?;

        for func in &self.functions {
            writeln!(asm, "\t.globl {}", func.name)?;
            writeln!(asm, "{}:", func.name)?;

            for instr in &func.instrs {
                if let X86Instruction::Label(_) = instr {
                    writeln!(asm, "{}", instr)?;
                } else {
                    writeln!(asm, "\t{}", instr)?;
                }
            }
        }

        let mut current_section = None;

        for var in &module.global_vars {
            let section = match var.kind {
                GlobalVarKind::Public | GlobalVarKind::Private => ".data",
                GlobalVarKind::Const => ".section .rodata",
            };

            if current_section != Some(section) {
                current_section = Some(section);
                writeln!(asm, "\t{}", section)?;
            }

            if let GlobalVarKind::Public = var.kind {
                writeln!(asm, "\t.globl {}", var.name)?;
            }

            writeln!(asm, "{}:", self.global_var_symbols.get(&var.name).unwrap())?;

            for (ty, imm) in &var.data {
                let directive = match ty {
                    I8 => ".byte",
                    I16 => ".short",
                    I32 => ".int",
                    I64 => ".quad",
                    F32 => ".float",
                    F64 => ".double",
                    Void => unreachable!(),
                };

                let imm = const_to_bits(*ty, *imm);
                writeln!(asm, "\t{} {}", directive, imm)?;
            }
        }

        Ok(asm)
    }
}

fn const_to_bits(ty: Type, imm: Constant) -> i128 {
    match imm {
        Constant::Int(imm) => match ty {
            I8 => imm & 0xff,
            I16 => imm & 0xffff,
            I32 => imm & 0xffff_ffff,
            I64 => imm & 0xffff_ffff_ffff_ffff,
            _ => unreachable!(),
        },
        Constant::Float(imm) => match ty {
            F32 => f32::to_bits(imm as f32) as i128,
            F64 => f64::to_bits(imm) as i128,
            _ => unreachable!(),
        },
    }
}

fn reg(register: Register) -> String {
    let i = match register.ty {
        I32 => 0,
        I16 => 1,
        I8 => 2,
        _ => unreachable!("unsupported register type: {}", register.ty),
    };

    match register.id {
        EAX => ["eax", "ax", "al"][i],
        ECX => ["ecx", "cx", "cl"][i],
        EDX => ["edx", "dx", "dl"][i],
        EBX => ["ebx", "bx", "bl"][i],
        ESP => ["esp", "sp"][i],
        EBP => ["ebp", "bp"][i],
        ESI => ["esi", "si"][i],
        EDI => ["edi", "di"][i],
        _ => unreachable!("unsupported register id: {}", register.id),
    }
    .to_string()
}

fn add_prologue_and_epilogue(x86_func: &mut X86Function, regs_to_save: &[RegisterId]) {
    let mut instrs = vec![];

    let stack_size = x86_func.memory_slot_offset;

    let ebp = Register::new(EBP, I32);
    let esp = Register::new(ESP, I32);

    for &id in regs_to_save {
        instrs.push(X86Instruction::Push(Register::new(id, I32)));
    }

    if stack_size != 0 || x86_func.uses_params {
        instrs.push(X86Instruction::Push(ebp));
        instrs.push(X86Instruction::Mov(ebp, esp));
        instrs.push(X86Instruction::SubImm(esp, stack_size));
    }

    for instr in &x86_func.instrs {
        if let X86Instruction::Ret = instr {
            if stack_size != 0 || x86_func.uses_params {
                instrs.push(X86Instruction::AddImm(esp, stack_size));
                instrs.push(X86Instruction::Pop(ebp));
            }

            for &id in regs_to_save.iter().rev() {
                instrs.push(X86Instruction::Pop(Register::new(id, I32)));
            }
        }

        instrs.push(instr.clone());
    }

    x86_func.instrs = instrs;
}

fn peephole_optimize(instrs: &[X86Instruction]) -> Vec<X86Instruction> {
    let mut opt = Vec::new();

    for instr in instrs {
        match instr {
            X86Instruction::Mov(dst, src) if dst == src => continue,
            X86Instruction::MovImm(dst, 0) => opt.push(X86Instruction::Xor(*dst, *dst)),
            X86Instruction::AddImm(_, 0) => continue,
            X86Instruction::SubImm(_, 0) => continue,
            _ => opt.push(instr.clone()),
        }
    }

    opt
}

fn param_stack_size(ty: Type) -> i128 {
    match ty {
        I8 | I16 | I32 | F32 => 4,
        I64 | F64 => 8,
        _ => unreachable!(),
    }
}
