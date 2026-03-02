use std::collections::HashMap;

use crate::compiler::ir;

struct Locals {
    var_to_location: HashMap<ir::IRVar, String>,
    stack_used: u32,
}

impl Locals {
    fn new(variables: Vec<ir::IRVar>) -> Self {
        let mut var_to_location: HashMap<ir::IRVar, String> = HashMap::new();
        let mut stack_used: u32 = 0;

        for var in variables {
            stack_used += 1;
            var_to_location.insert(var, format!("-{}(%rbp)", stack_used * 8));
        }

        Self {
            var_to_location,
            stack_used,
        }
    }

    fn get_ref(&self, var: ir::IRVar) -> Result<String, String> {
        if let Some(location) = self.var_to_location.get(&var) {
            Ok(location.clone())
        } else {
            Err(format!("undefined var: {}", var))
        }
    }

    fn stack_used(&self) -> u32 {
        self.stack_used
    }
}
