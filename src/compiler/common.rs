use std::{cell::RefCell, collections::HashMap, rc::Rc};

#[derive(Debug, Clone, PartialEq)]
pub struct Location {
    pub line: u32,
    pub column: u32,
}

#[derive(Debug, Clone, PartialEq)]
pub struct SymTab<V> {
    pub locals: HashMap<String, V>,
    pub parent: Option<Rc<RefCell<SymTab<V>>>>,
}

impl<V: Clone> SymTab<V> {
    pub fn lookup(&self, identifier: &str) -> Result<V, String> {
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

    pub fn declare(&mut self, identifier: &str, value: V) {
        self.locals.insert(identifier.to_string(), value);
    }

    pub fn assign(&mut self, identifier: &str, value: V) -> Result<(), String> {
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
