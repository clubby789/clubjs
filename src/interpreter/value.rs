use std::collections::{hash_map::Entry, HashMap};

use crate::{ast, intern::Symbol, lex::kw};

use super::{EnvironmentRecord, ExecutionContext, PrivateEnvironmentRecord, Realm, Shared};

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
    // utf16 🤓
    String(Symbol),
    Symbol {
        sym: Symbol,
        description: Option<Symbol>,
    },
    Number(f64),
    // TODO: add bigint
    BigInt(()),
    Object(Shared<JSObject>),
}

#[derive(Default, Debug)]
pub struct JSObject {
    prototype: Option<Shared<JSObject>>,
    extensible: bool,
    properties: HashMap<Symbol, PropertyDescriptor>,
    extra_slots: AdditionalSlots,
}

impl JSObject {
    pub fn get_prototype_of(&self) -> Option<Shared<JSObject>> {
        self.prototype.clone()
    }

    pub fn set_prototype_of(&mut self, value: Option<Shared<JSObject>>) -> bool {
        let current = self.prototype.clone();
        // TODO: check samevalue
        let extensible = self.extensible;
        if !extensible {
            return false;
        }
        let mut p = value.clone();
        let mut done = false;
        while let Some(pv) = p {
            // TODO: if samevalue(p, self) return false
            // TODO: if p[[GetPrototypeOf]] != Self::get_prototype_of, break
            p = pv.borrow().get_prototype_of();
        }
        self.prototype = value;
        true
    }

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

    /// Implements InstantiateOrdinaryFunctionObject
    pub fn ordinary_function_object(
        function: ast::Function,
        env: EnvironmentRecord,
        private_env: PrivateEnvironmentRecord,
    ) -> Self {
        // 1. Let name be StringValue of BindingIdentifier.
        let name = function.name.unwrap_or(kw::default);
        // OrdinaryFunctionCreate

        todo!()
    }

    /// Implements CreateBuiltinFunction
    pub fn builtin_function_object(
        behaviour: BuiltinFunctionPointer,
        length: usize,
        name: Symbol,
        realm: Shared<Realm>,
    ) -> Self {
        let slots = BuiltinFunctionSlots::new(realm, name, behaviour);
        JSObject {
            prototype: None,
            extensible: true,
            properties: HashMap::new(),
            extra_slots: AdditionalSlots::BuiltinFunction(slots),
        }
    }
}

/// Hold additional slots for an ordinary object
#[derive(Default, Debug)]
enum AdditionalSlots {
    #[default]
    Ordinary,
    BuiltinFunction(BuiltinFunctionSlots),
    Function(FunctionSlots),
}

pub type BuiltinFunctionPointer = fn(Shared<Realm>, Option<JSValue>, Vec<JSValue>) -> JSValue;

#[derive(Debug)]
struct BuiltinFunctionSlots {
    realm: Shared<Realm>,
    initial_name: Symbol,
    func: BuiltinFunctionPointer,
}

impl BuiltinFunctionSlots {
    pub fn new(realm: Shared<Realm>, initial_name: Symbol, func: BuiltinFunctionPointer) -> Self {
        Self {
            realm,
            initial_name,
            func,
        }
    }

    pub fn call(
        &self,
        function_obj: JSValue,
        this: Option<JSValue>,
        args: Vec<JSValue>,
    ) -> JSValue {
        let callee_context =
            ExecutionContext::from_realm_and_function(self.realm.clone(), function_obj);
        self.realm.borrow().push_execution_context(callee_context);
        let result = (self.func)(self.realm.clone(), this, args);
        self.realm.borrow().pop_execution_context();
        result
    }
}

#[derive(Debug)]
struct FunctionSlots {
    environment: EnvironmentRecord,
    private_environment: Option<PrivateEnvironmentRecord>,
    formal_paramaters: Vec<ast::FunctionParam>,
    ecma_script_code: ast::Block,
    realm: Shared<Realm>,
    this_mode: ThisMode,
    // strict: bool,
    home_object: Shared<JSObject>,
}

#[derive(Debug)]
enum ThisMode {
    Lexical,
    // Strict,
    Global,
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
