use crate::{
    function::{ExternalFunction, Function, FunctionDecl},
    global_var::{ExternalVar, GlobalVar, GlobalVarKind},
    instruction::Type,
};

#[derive(Debug, Clone)]
pub struct Module {
    pub functions: Vec<Function>,
    pub global_vars: Vec<GlobalVar>,
    pub external_fns: Vec<ExternalFunction>,
    pub external_vars: Vec<ExternalVar>,
}

impl Module {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            global_vars: Vec::new(),
            external_fns: Vec::new(),
            external_vars: Vec::new(),
        }
    }

    pub fn function_decls(&self) -> impl Iterator<Item = &FunctionDecl> {
        self.functions
            .iter()
            .map(|f| &f.decl)
            .chain(self.external_fns.iter().map(|f| &f.decl))
            .into_iter()
    }

    pub fn function(&mut self, name: impl ToString, ty: Type) -> &mut Function {
        let function = Function::new(name.to_string(), ty);
        self.functions.push(function);
        self.functions.last_mut().unwrap()
    }

    pub fn global_var(&mut self, name: impl ToString, kind: GlobalVarKind) -> &mut GlobalVar {
        let global_var = GlobalVar::new(name.to_string(), kind);
        self.global_vars.push(global_var);
        self.global_vars.last_mut().unwrap()
    }

    pub fn external_function(&mut self, name: impl ToString, ty: Type) -> &mut ExternalFunction {
        let extern_fn = ExternalFunction::new(name.to_string(), ty);
        self.external_fns.push(extern_fn);
        self.external_fns.last_mut().unwrap()
    }

    pub fn external_var(&mut self, name: impl ToString) -> &mut ExternalVar {
        let extern_var = ExternalVar::new(name.to_string());
        self.external_vars.push(extern_var);
        self.external_vars.last_mut().unwrap()
    }
}

impl std::fmt::Display for Module {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        for extern_fn in &self.external_fns {
            writeln!(f, "{}", extern_fn)?;
        }

        if !self.external_fns.is_empty() {
            writeln!(f)?;
        }

        for extern_var in &self.external_vars {
            writeln!(f, "{}", extern_var)?;
        }

        if !self.external_vars.is_empty() {
            writeln!(f)?;
        }

        for global_var in &self.global_vars {
            writeln!(f, "{}", global_var)?;
        }

        if !self.global_vars.is_empty() {
            writeln!(f)?;
        }

        for function in &self.functions {
            writeln!(f, "{}\n", function)?;
        }

        Ok(())
    }
}
