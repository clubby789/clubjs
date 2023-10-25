use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{Debug, Display},
    rc::Rc,
};

use crate::{ast, codegen::Script, intern::Symbol, lex::kw};

use super::{EnvironmentRecord, ExecutionContext, Realm, ReferenceRecord, Shared};

#[derive(Clone, Default)]
pub struct JSValue {
    kind: JSValueKind,
}

impl Debug for JSValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.kind, f)
    }
}

impl Display for JSValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.kind, f)
    }
}

impl JSValue {
    pub fn undefined() -> Self {
        Self {
            kind: JSValueKind::Undefined,
        }
    }

    pub fn null() -> Self {
        Self {
            kind: JSValueKind::Null,
        }
    }

    pub fn int(n: u128) -> Self {
        Self {
            kind: JSValueKind::Number(n as f64),
        }
    }

    pub fn object(obj: Shared<JSObject>) -> Self {
        Self {
            kind: JSValueKind::Object(obj),
        }
    }

    pub fn to_object(&self) -> Option<Shared<JSObject>> {
        if let JSValueKind::Object(o) = &self.kind {
            Some(o.clone())
        } else {
            None
        }
    }

    pub fn reference(r: ReferenceRecord) -> Self {
        Self {
            kind: JSValueKind::Reference(Box::new(r)),
        }
    }

    pub fn to_reference(&self) -> Option<&ReferenceRecord> {
        if let JSValueKind::Reference(r) = &self.kind {
            Some(r)
        } else {
            None
        }
    }

    pub fn get_value(&self) -> Self {
        let Some(r) = self.to_reference() else {
            return self.clone();
        };

        // TODO: property reference
        r.base.get_binding_value(r.referenced_name)
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

#[derive(Clone, Default)]
enum JSValueKind {
    #[default]
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
    Reference(Box<ReferenceRecord>),
}

impl Debug for JSValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Null => write!(f, "Null"),
            Self::Undefined => write!(f, "Undefined"),
            Self::Bool(arg0) => f.debug_tuple("Bool").field(arg0).finish(),
            Self::String(arg0) => f.debug_tuple("String").field(arg0).finish(),
            Self::Symbol { sym, description } => f
                .debug_struct("Symbol")
                .field("sym", sym)
                .field("description", description)
                .finish(),
            Self::Number(arg0) => f.debug_tuple("Number").field(arg0).finish(),
            Self::BigInt(arg0) => f.debug_tuple("BigInt").field(arg0).finish(),
            Self::Object(_) => f.debug_tuple("Object").finish(),
            Self::Reference(arg0) => f.debug_tuple("Reference").field(arg0).finish(),
        }
    }
}

impl Display for JSValueKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JSValueKind::Null => write!(f, "null"),
            JSValueKind::Undefined => write!(f, "undefined"),
            JSValueKind::Bool(b) => write!(f, "{b}"),
            JSValueKind::String(s) => write!(f, "{s}"),
            JSValueKind::Symbol { description, .. } => {
                write!(f, "Symbol({})", description.unwrap_or(kw::Empty))
            }
            JSValueKind::Number(n) => write!(f, "{n}"),
            JSValueKind::BigInt(_) => todo!(),
            JSValueKind::Object(_) => write!(f, "[object Object]"),
            JSValueKind::Reference(r) => {
                write!(f, "{}", r.base.get_binding_value(r.referenced_name))
            }
        }
    }
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
        // let current = self.prototype.clone();
        // TODO: check samevalue
        let extensible = self.extensible;
        if !extensible {
            return false;
        }
        let mut p = value.clone();
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

    // TODO: property keys
    pub fn get_own_property(&self, name: Symbol) -> Option<PropertyDescriptor> {
        // TODO: accessor properties
        self.properties.get(&name).cloned()
    }

    pub fn ordinary_get(&self, name: Symbol) -> JSValue {
        let desc = self.get_own_property(name);
        if let Some(d) = desc {
            return d.value;
        }
        let mut cur = self.get_prototype_of();
        while let Some(parent) = cur {
            if let Some(v) = parent.borrow().get_own_property(name) {
                return v.value;
            }
            cur = self.get_prototype_of();
        }
        JSValue::undefined()
    }

    pub fn has_own_property(&self, name: Symbol) -> bool {
        self.properties.contains_key(&name)
    }

    pub fn has_property(&self, name: Symbol) -> bool {
        if self.has_own_property(name) {
            return true;
        }

        let mut cur = self.get_prototype_of();
        while let Some(parent) = cur {
            if parent.borrow().has_own_property(name) {
                return true;
            }
            cur = parent.borrow().get_prototype_of();
        }
        false
    }

    pub fn ordinary_object(prototype: Option<Shared<JSObject>>) -> Self {
        Self {
            prototype,
            extensible: true,
            properties: HashMap::new(),
            extra_slots: AdditionalSlots::None,
        }
    }

    /// Implements InstantiateOrdinaryFunctionObject
    pub fn ordinary_function_object(
        function: ast::Function,
        _env: EnvironmentRecord,
        // _private_env: PrivateEnvironmentRecord,
    ) -> Self {
        // 1. Let name be StringValue of BindingIdentifier.
        let _name = function.name.unwrap_or(kw::default);
        // OrdinaryFunctionCreate

        todo!()
    }

    /// Implements CreateBuiltinFunction
    pub fn builtin_function_object(
        behaviour: BuiltinFunctionPointer,
        _length: usize,
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

    pub fn extensible(&self) -> bool {
        self.extensible
    }

    pub fn callable(&self) -> bool {
        matches!(
            self.extra_slots,
            AdditionalSlots::BuiltinFunction(_) | AdditionalSlots::Function(_)
        )
    }
}

impl Shared<JSObject> {
    pub fn call(&self, script: Rc<Script>, this: JSValue, args: Vec<JSValue>) -> JSValue {
        let borrowed = self.borrow();
        match borrowed.extra_slots.clone() {
            AdditionalSlots::BuiltinFunction(b) => {
                drop(borrowed);
                b.call(script, JSValue::object(self.clone()), this, args)
            }
            AdditionalSlots::Function(_) => todo!(),
            _ => unreachable!("callers should check this is callable"),
        }
    }
}

/// Hold additional slots for an ordinary object
#[derive(Default, Debug, Clone)]
enum AdditionalSlots {
    #[default]
    None,
    BuiltinFunction(BuiltinFunctionSlots),
    Function(FunctionSlots),
}

pub type BuiltinFunctionPointer = fn(Shared<Realm>, JSValue, Vec<JSValue>) -> JSValue;

#[derive(Debug, Clone)]
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
        script: Rc<Script>,
        function_obj: JSValue,
        this: JSValue,
        args: Vec<JSValue>,
    ) -> JSValue {
        let realm = self.realm.clone();
        let callee_context = ExecutionContext::from_realm_and_function(
            realm,
            function_obj,
            EnvironmentRecord::default(),
            EnvironmentRecord::default(),
            script,
        );
        self.realm.borrow().push_execution_context(callee_context);
        let result = (self.func)(self.realm.clone(), this, args);
        self.realm.borrow().pop_execution_context();
        result
    }
}

#[derive(Debug, Clone)]
struct FunctionSlots {
    environment: EnvironmentRecord,
    // private_environment: Option<Shared<PrivateEnvironmentRecord>>,
    formal_paramaters: Vec<ast::FunctionParam>,
    ecma_script_code: ast::Block,
    realm: Shared<Realm>,
    this_mode: ThisMode,
    // strict: bool,
    home_object: Shared<JSObject>,
}

#[derive(Debug, Clone, Copy)]
enum ThisMode {
    Lexical,
    // Strict,
    Global,
}

#[derive(Debug, Clone)]
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
