use std::collections::{hash_map::Entry, HashMap};

use crate::intern::Symbol;

use super::Shared;

#[derive(Clone, Debug)]
pub struct JSValue {
    kind: JSValueKind,
}

impl JSValue {
    pub fn undefined() -> Self {
        Self {
            kind: JSValueKind::Undefined,
        }
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self.kind, JSValueKind::Undefined)
    }

    pub fn null() -> Self {
        Self {
            kind: JSValueKind::Null,
        }
    }

    pub fn is_null(&self) -> bool {
        matches!(self.kind, JSValueKind::Null)
    }
}

impl From<bool> for JSValue {
    fn from(value: bool) -> Self {
        JSValue {
            kind: JSValueKind::Bool(value),
        }
    }
}

impl From<Shared<JSObject>> for JSValue {
    fn from(value: Shared<JSObject>) -> Self {
        JSValue {
            kind: JSValueKind::Object(value),
        }
    }
}

#[derive(Clone, Debug)]
enum JSValueKind {
    Null,
    Undefined,
    Bool(bool),
    Object(Shared<JSObject>),
}

#[derive(Default, Debug)]
pub struct JSObject {
    properties: HashMap<Symbol, PropertyDescriptor>,
}

impl JSObject {
    pub fn define_property_or_throw(&mut self, name: Symbol, prop: PropertyDescriptor) {
        assert!(self.define_own_property(name, prop))
    }

    pub fn define_own_property(&mut self, name: Symbol, prop: PropertyDescriptor) -> bool {
        match self.properties.entry(name) {
            Entry::Occupied(mut o) if o.get().writable => {
                *o.get_mut() = prop;
                true
            }
            Entry::Vacant(v) => {
                v.insert(prop);
                true
            }
            _ => false,
        }
    }
}

#[derive(Debug)]
pub struct PropertyDescriptor {
    value: JSValue,
    writable: bool,
    // get: (),
    // set: (),
    enumerable: bool,
    configurable: bool,
}

impl PropertyDescriptor {
    /// Create a property descriptor that contains a given value, and is not
    /// writable, enumerable or configurable`
    pub fn new(value: JSValue) -> Self {
        Self {
            value,
            writable: false,
            enumerable: false,
            configurable: false,
        }
    }

    pub fn value(self, value: JSValue) -> Self {
        Self { value, ..self }
    }

    pub fn writable(self, writable: bool) -> Self {
        Self { writable, ..self }
    }

    pub fn enumerable(self, enumerable: bool) -> Self {
        Self { enumerable, ..self }
    }

    pub fn configurable(self, configurable: bool) -> Self {
        Self {
            configurable,
            ..self
        }
    }
}

impl Default for PropertyDescriptor {
    fn default() -> Self {
        Self::new(JSValue::undefined())
    }
}
