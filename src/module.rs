use std::collections::HashMap;

use crate::function::Function;

#[derive(Debug, Clone)]
pub struct Module {
    functions: HashMap<String, Function>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: HashMap::new(),
        }
    }

    pub fn function(&mut self, name: &str) -> &mut Function {
        self.functions
            .entry(name.to_string())
            .or_insert_with(Function::new)
    }
}
