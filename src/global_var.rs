use std::u8;

use crate::instruction::{Constant, Pointer, Type};

#[derive(Debug, Clone)]
pub enum GlobalVarKind {
    Public,
    Private,
    Const,
}

#[derive(Debug, Clone)]
pub struct GlobalVar {
    pub name: String,
    pub kind: GlobalVarKind,
    pub data: Vec<(Type, Constant)>,
}

impl GlobalVar {
    pub fn new(name: String, kind: GlobalVarKind) -> Self {
        Self {
            name,
            kind,
            data: Vec::new(),
        }
    }

    pub fn add_data(&mut self, ty: Type, value: Constant) {
        self.data.push((ty, value));
    }
}

impl Into<Pointer> for GlobalVar {
    fn into(self) -> Pointer {
        Pointer::GlobalVar(self.name)
    }
}

#[derive(Debug, Clone)]
pub struct ExternalVar {
    pub name: String,
}

impl ExternalVar {
    pub fn new(name: String) -> Self {
        Self { name }
    }
}

impl Into<Pointer> for ExternalVar {
    fn into(self) -> Pointer {
        Pointer::GlobalVar(self.name)
    }
}

impl std::fmt::Display for GlobalVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.kind {
            GlobalVarKind::Public => write!(f, "pub let {} = ", self.name)?,
            GlobalVarKind::Private => write!(f, "let {} = ", self.name)?,
            GlobalVarKind::Const => write!(f, "const {} = ", self.name)?,
        }

        if self.data.is_empty() {
            return write!(f, "0");
        }

        let is_i8 = self.data.iter().all(|(ty, _)| *ty == Type::I8);

        if is_i8 {
            let string = self
                .data
                .iter()
                .map(|(_, value)| match value {
                    Constant::Int(value) => format!("{}", *value as u8 as char),
                    Constant::Float(_) => unreachable!(),
                })
                .collect::<String>();

            write!(f, "\"")?;

            for c in string.chars() {
                match c as u8 {
                    92 => write!(f, "\\\\")?,
                    34 => write!(f, "\\\"")?,
                    32..=126 => write!(f, "{}", c)?,
                    _ => write!(f, "\\x{:02x}", c as u8)?,
                }
            }

            return write!(f, "\"");
        }

        write!(f, "{}_{}", self.data[0].1, self.data[0].0)?;

        for (ty, value) in self.data.iter().skip(1) {
            write!(f, ", {}_{}", value, ty)?;
        }

        Ok(())
    }
}

impl std::fmt::Display for ExternalVar {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "extern let {}", self.name)
    }
}
