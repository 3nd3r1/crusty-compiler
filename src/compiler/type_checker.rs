use std::{cell::RefCell, collections::HashMap, rc::Rc};

pub mod types {
    #[derive(Debug, Clone, PartialEq)]
    pub enum Type {
        Int,
        Bool,
        Unit,
        Function {
            params: Vec<Type>,
            return_type: Box<Type>,
        },
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct TypeSymTab {
    pub locals: HashMap<String, types::Type>,
    pub parent: Option<Rc<RefCell<TypeSymTab>>>,
}

impl TypeSymTab {
    fn lookup(&self, identifier: &str) -> Result<types::Type, String> {
        if let Some(value) = self.locals.get(identifier) {
            Ok(value.clone())
        } else {
            if let Some(parent) = &self.parent {
                parent.borrow().lookup(identifier)
            } else {
                Err(format!("undefined identifier: {}", identifier))
            }
        }
    }

    fn declare(&mut self, identifier: &str, value: types::Type) {
        self.locals.insert(identifier.to_string(), value);
    }

    fn assign(&mut self, identifier: &str, value: types::Type) -> Result<(), String> {
        if let Some(_) = self.locals.get(identifier) {
            self.locals.insert(identifier.to_string(), value.clone());
            Ok(())
        } else {
            if let Some(parent) = &self.parent {
                parent.borrow_mut().assign(identifier, value)
            } else {
                Err(format!("undefined identifier: {}", identifier))
            }
        }
    }
}
