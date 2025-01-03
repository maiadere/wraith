use crate::{function::Function, instruction::Type};

#[derive(Debug, Clone)]
pub struct Module {
    functions: Vec<Function>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn function(&mut self, name: impl ToString, ty: Type) -> &mut Function {
        let function = Function::new(name.to_string(), ty);
        self.functions.push(function);
        self.functions.last_mut().unwrap()
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for function in &self.functions {
            writeln!(f, "{}", function)?;
        }
        Ok(())
    }
}
