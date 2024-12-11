use crate::function::FunctionBuilder;

#[derive(Debug, Clone)]
pub struct ModuleBuilder {
    functions: Vec<FunctionBuilder>,
}

impl ModuleBuilder {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
        }
    }

    pub fn function(&mut self, name: impl ToString) -> &mut FunctionBuilder {
        let function = FunctionBuilder::new(name.to_string());
        self.functions.push(function);
        self.functions.last_mut().unwrap()
    }
}
