use std::{
    collections::{hash_map::Entry, HashMap},
    fmt::{Debug, Display},
    rc::Rc,
};

use either::Either;

use crate::{
    codegen::{self, Script},
    intern::Symbol,
    lex::kw,
};

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
        Display::fmt(&self.get_value().kind, f)
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

    pub fn string(s: Symbol) -> Self {
        Self {
            kind: JSValueKind::String(s),
        }
    }

    pub fn object(obj: Shared<JSObject>) -> Self {
        Self {
            kind: JSValueKind::Object(obj),
        }
    }

    pub fn as_object(&self) -> Option<Shared<JSObject>> {
        if let JSValueKind::Object(o) = &self.kind {
            Some(o.clone())
        } else {
            None
        }
    }

    pub fn as_function(&self) -> Option<Function> {
        let JSValueKind::Object(o) = &self.kind else {
            return None;
        };
        if let Object::Function(f) = &o.borrow().object {
            Some(f.clone())
        } else {
            None
        }
    }

    pub fn reference(r: ReferenceRecord) -> Self {
        Self {
            kind: JSValueKind::Reference(Box::new(r)),
        }
    }

    pub fn as_reference(&self) -> Option<&ReferenceRecord> {
        if let JSValueKind::Reference(r) = &self.kind {
            Some(r)
        } else {
            None
        }
    }

    pub fn get_value(&self) -> Self {
        let Some(r) = self.as_reference() else {
            return self.clone();
        };

        match &r.base {
            Either::Left(env) => env.get_binding_value(r.referenced_name),
            Either::Right(val) => {
                let object = val
                    .get_value()
                    .as_object()
                    .unwrap_or_else(|| panic!("TypeError: {val} is not an object"));
                let object = object.borrow();
                object.ordinary_get(r.referenced_name, r.get_this_value())
            }
        }
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
            JSValueKind::Reference(_) => {
                /*match r.base {
                    Either::Left(env) => env.get_binding_value(r.referenced_name),
                    Either::Right(o) => Display::fmt(o)
                }*/
                // write!(f, "{}", r.base.get_binding_value(r.referenced_name))
                unreachable!("we should never be printing a reference")
            }
        }
    }
}

#[derive(Default, Debug)]
pub struct JSObject {
    prototype: Option<Shared<JSObject>>,
    extensible: bool,
    properties: HashMap<Symbol, PropertyDescriptor>,
    object: Object,
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

    pub fn ordinary_get(&self, name: Symbol, _receiver: JSValue) -> JSValue {
        // TODO: use the receiver for getters
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
            object: Object::Ordinary,
        }
    }

    pub fn ordinary_function_object(
        prototype: Option<Shared<JSObject>>,
        param_list: Vec<Symbol>,
        body: Rc<codegen::Function>,
        // TODO: check this is handled right (OrdinaryFunctionCreate)
        this_mode: ThisMode,
        env: EnvironmentRecord,
        realm: Shared<Realm>,
    ) -> Self {
        let mut f = Self::ordinary_object(prototype);
        let function = Function::new(env, param_list, body, realm, this_mode);
        f.object = Object::Function(function);
        f
    }

    /// Implements CreateBuiltinFunction
    pub fn builtin_function_object(
        behaviour: BuiltinFunctionPointer,
        _length: usize,
        name: Symbol,
        realm: Shared<Realm>,
    ) -> Self {
        let function = BuiltinFunction::new(realm, name, behaviour);
        JSObject {
            prototype: None,
            extensible: true,
            properties: HashMap::new(),
            object: Object::BuiltinFunction(function),
        }
    }

    pub fn extensible(&self) -> bool {
        self.extensible
    }

    pub fn callable(&self) -> bool {
        self.object.callable()
    }

    pub fn environment_record(&self) -> EnvironmentRecord {
        let Object::Function(f) = &self.object else {
            todo!("blah blah")
        };
        f.environment.clone()
    }
}

impl Shared<JSObject> {
    pub fn call(&self, script: Rc<Script>, this: JSValue, args: Vec<JSValue>) {
        let borrowed = self.borrow();
        match borrowed.object.clone() {
            Object::BuiltinFunction(b) => {
                drop(borrowed);
                // TODO: return this again
                b.call(script, JSValue::object(self.clone()), this, args);
            }
            Object::Function(f) => {
                f.call(script, JSValue::object(self.clone()), this, args);
            }
            _ => unreachable!("callers should check this is callable"),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub enum Object {
    #[default]
    Ordinary,
    Function(Function),
    BuiltinFunction(BuiltinFunction),
}

impl Object {
    pub fn callable(&self) -> bool {
        matches!(self, Object::Function(_) | Object::BuiltinFunction(_))
    }
}

pub type BuiltinFunctionPointer = fn(Shared<Realm>, JSValue, Vec<JSValue>) -> JSValue;

#[derive(Debug, Clone)]
pub struct BuiltinFunction {
    realm: Shared<Realm>,
    initial_name: Symbol,
    func: BuiltinFunctionPointer,
}

impl BuiltinFunction {
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
pub struct Function {
    environment: EnvironmentRecord,
    // private_environment: Option<Shared<PrivateEnvironmentRecord>>,
    formal_paramaters: Rc<Vec<Symbol>>,
    ecma_script_code: Rc<codegen::Function>,
    realm: Shared<Realm>,
    this_mode: ThisMode,
    // strict: bool,
    home_object: Option<Shared<JSObject>>,
}

impl Function {
    pub fn new(
        env: EnvironmentRecord,
        formal_paramaters: Vec<Symbol>,
        ecma_script_code: Rc<codegen::Function>,
        realm: Shared<Realm>,
        this_mode: ThisMode,
    ) -> Self {
        Self {
            environment: env,
            formal_paramaters: Rc::new(formal_paramaters),
            ecma_script_code,
            realm,
            this_mode,
            home_object: None,
        }
    }

    pub fn call(
        &self,
        script: Rc<Script>,
        function_obj: JSValue,
        _this: JSValue,
        args: Vec<JSValue>,
    ) {
        // TODO: properly set up environment records
        let realm = self.realm.clone();
        let local_env =
            EnvironmentRecord::new_function_environment(function_obj.as_object().unwrap().clone());

        let callee_context = ExecutionContext::from_realm_and_function(
            realm,
            function_obj,
            local_env.clone(),
            EnvironmentRecord::default(),
            script,
        );
        self.realm.borrow().push_execution_context(callee_context);
        assert_eq!(
            args.len(),
            0,
            "passing args to ordinary functions is not supported"
        );
    }

    pub fn code(&self) -> &codegen::Function {
        self.ecma_script_code.as_ref()
    }
}

#[derive(Debug, Clone, Copy)]
pub enum ThisMode {
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
    _enumerable: bool,
    configurable: bool,
}

impl PropertyDescriptor {
    /// Create a property descriptor that contains a given value, and is not
    /// writable, enumerable or configurable`
    pub fn new(value: JSValue) -> Self {
        Self {
            value,
            writable: false,
            _enumerable: false,
            configurable: false,
        }
    }

    pub fn writable(self, writable: bool) -> Self {
        Self { writable, ..self }
    }

    pub fn enumerable(self, enumerable: bool) -> Self {
        Self {
            _enumerable: enumerable,
            ..self
        }
    }

    pub fn configurable(self, configurable: bool) -> Self {
        Self {
            configurable,
            ..self
        }
    }

    pub fn is_configurable(&self) -> bool {
        self.configurable
    }
}

impl Default for PropertyDescriptor {
    fn default() -> Self {
        Self::new(JSValue::undefined())
    }
}
