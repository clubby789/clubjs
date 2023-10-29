use std::{
    cell::RefCell,
    cmp::Ordering,
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
pub enum JSValue {
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

#[allow(clippy::wrong_self_convention)] // We use ES6's naming scheme
impl JSValue {
    pub const fn undefined() -> Self {
        JSValue::Undefined
    }

    pub fn is_undefined(&self) -> bool {
        matches!(self, JSValue::Undefined)
    }

    pub const fn null() -> Self {
        JSValue::Null
    }

    pub fn is_null(&self) -> bool {
        matches!(self, JSValue::Null)
    }

    pub fn int(n: u128) -> Self {
        JSValue::Number(n as f64)
    }

    pub fn number(n: f64) -> Self {
        JSValue::Number(n)
    }

    pub fn string(s: Symbol) -> Self {
        JSValue::String(s)
    }

    pub fn object(obj: Shared<JSObject>) -> Self {
        JSValue::Object(obj)
    }

    pub fn as_object(&self) -> Option<Shared<JSObject>> {
        // TODO: do this without recursion
        if let JSValue::Reference(_) = self {
            self.get_value().as_object()
        } else if let JSValue::Object(o) = &self {
            Some(o.clone())
        } else {
            None
        }
    }

    pub fn as_function(&self) -> Option<&Function> {
        let JSValue::Object(o) = &self else {
            return None;
        };

        if let Object::Function(f) = &o.object {
            Some(f)
        } else {
            None
        }
    }

    pub fn reference(r: ReferenceRecord) -> Self {
        JSValue::Reference(Box::new(r))
    }

    pub fn as_reference(&self) -> Option<&ReferenceRecord> {
        if let JSValue::Reference(r) = &self {
            Some(r)
        } else {
            None
        }
    }

    pub fn same_type(&self, other: &Self) -> bool {
        // TODO: check object types
        std::mem::discriminant(self) == std::mem::discriminant(other)
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
                object.ordinary_get(r.referenced_name, r.get_this_value())
            }
        }
    }

    pub fn put_value(&self, val: JSValue) {
        let JSValue::Reference(r) = &self else {
            panic!("ReferenceError: not a reference")
        };
        // TODO: set on global if unresolved
        // IsPropertyReference - if Right it is a property reference,
        // otherwise no
        match &r.base {
            Either::Left(env) => {
                env.set_mutable_binding(r.referenced_name, val);
            }
            Either::Right(val) => {
                let base_obj = val.clone().to_object();
                base_obj.ordinary_set(r.referenced_name, val.clone(), r.get_this_value());
            }
        }
    }

    pub fn to_object(self) -> Shared<JSObject> {
        match self {
            JSValue::Object(o) => o,
            JSValue::Reference(_) => self.get_value().to_object(),
            _ => todo!("other ToObject conversions: {self:?}"),
        }
    }

    // TODO: preferred type
    pub fn to_primitive(self) -> Self {
        match self {
            JSValue::Object(_o) => todo!("converting object to primitive"),
            JSValue::Reference(_) => self.get_value().to_primitive(),
            _ => self,
        }
    }

    pub fn to_numeric(self) -> Self {
        let prim = self.to_primitive();
        if let JSValue::BigInt(_) = prim {
            todo!("bigints");
        };
        prim.to_number()
    }

    pub fn to_number(self) -> Self {
        match self {
            JSValue::Number(_) => self,
            JSValue::Symbol { .. } | JSValue::BigInt(_) => {
                panic!("TypeError: converting `{self}` to a number")
            }
            JSValue::Undefined => Self::number(f64::NAN),
            JSValue::Null => Self::number(0.0),
            JSValue::Bool(b) => Self::number(b as u8 as f64),
            JSValue::String(s) => Self::number(s.as_str().parse().unwrap_or(f64::NAN)),
            JSValue::Object(_) => todo!("requires toprimitive preferred"),
            JSValue::Reference(_) => unreachable!(),
        }
    }

    pub fn to_string(self) -> Symbol {
        match self {
            JSValue::String(s) => s,
            _ => todo!("ToString"),
        }
    }

    pub fn to_boolean(self) -> bool {
        match self {
            JSValue::Bool(b) => b,
            JSValue::Undefined | JSValue::Null => false,
            JSValue::Number(n) if n == 0.0 || n == -0.0 || n.is_nan() => false,
            JSValue::String(s) if s.as_str().is_empty() => false,
            _ => true,
        }
    }

    /// Adds together two [`JSValue::Number`]s or [`JSValue::BigInt`]s
    pub fn add(self, other: Self) -> Self {
        debug_assert!(self.same_type(&other));
        match (self, other) {
            (JSValue::Number(l), JSValue::Number(r)) => JSValue::number(l + r),
            (JSValue::BigInt(_), JSValue::BigInt(_)) => todo!("bigint::add"),
            _ => unreachable!(),
        }
    }

    /// Compares two [`JSValue::Number`]s or [`JSValue::BigInt`]s
    pub fn less_than(self, other: Self) -> Self {
        debug_assert!(self.same_type(&other));
        match (self, other) {
            (JSValue::Number(l), JSValue::Number(r)) => match l.partial_cmp(&r) {
                Some(Ordering::Less) => JSValue::Bool(true),
                None => JSValue::Number(f64::NAN),
                _ => JSValue::Bool(false),
            },
            (JSValue::BigInt(_), JSValue::BigInt(_)) => todo!("bigint::lessThan"),
            _ => unreachable!(),
        }
    }
}

impl From<bool> for JSValue {
    fn from(value: bool) -> Self {
        JSValue::Bool(value)
    }
}

impl From<Shared<JSObject>> for JSValue {
    fn from(value: Shared<JSObject>) -> Self {
        JSValue::Object(value)
    }
}

impl Debug for JSValue {
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

impl Display for JSValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            JSValue::Null => write!(f, "null"),
            JSValue::Undefined => write!(f, "undefined"),
            JSValue::Bool(b) => write!(f, "{b}"),
            JSValue::String(s) => write!(f, "{s}"),
            JSValue::Symbol { description, .. } => {
                write!(f, "Symbol({})", description.unwrap_or(kw::Empty))
            }
            JSValue::Number(n) => write!(f, "{n}"),
            JSValue::BigInt(_) => todo!(),
            JSValue::Object(_) => write!(f, "[object Object]"),
            JSValue::Reference(_) => {
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
    prototype: RefCell<Option<Shared<JSObject>>>,
    extensible: bool,
    properties: RefCell<HashMap<Symbol, PropertyDescriptor>>,
    object: Object,
}

impl JSObject {
    pub fn get_prototype_of(&self) -> Option<Shared<JSObject>> {
        self.prototype.borrow().clone()
    }

    pub fn set_prototype_of(&self, value: Option<Shared<JSObject>>) -> bool {
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
            p = pv.get_prototype_of();
        }
        *self.prototype.borrow_mut() = value;
        true
    }

    pub fn define_property_or_throw(&self, name: Symbol, prop: PropertyDescriptor) {
        assert!(self.define_own_property(name, prop))
    }

    pub fn define_own_property(&self, name: Symbol, prop: PropertyDescriptor) -> bool {
        match self.properties.borrow_mut().entry(name) {
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

    pub fn create_data_property(&self, name: Symbol, value: JSValue) -> bool {
        let prop = PropertyDescriptor::new(value)
            .writable(true)
            .configurable(true)
            .enumerable(true);
        self.define_own_property(name, prop)
    }

    pub fn create_data_property_or_throw(&self, name: Symbol, value: JSValue) {
        assert!(
            self.create_data_property(name, value),
            "TypeError: could not set `{name}`++"
        )
    }

    // TODO: property keys
    pub fn get_own_property(&self, name: Symbol) -> Option<PropertyDescriptor> {
        // TODO: accessor properties
        self.properties.borrow().get(&name).cloned()
    }

    pub fn ordinary_get(&self, name: Symbol, _receiver: JSValue) -> JSValue {
        // TODO: use the receiver for getters
        let desc = self.get_own_property(name);
        if let Some(d) = desc {
            return d.value;
        }
        let mut cur = self.get_prototype_of();
        while let Some(parent) = cur {
            if let Some(v) = parent.get_own_property(name) {
                return v.value;
            }
            cur = parent.get_prototype_of();
        }
        JSValue::undefined()
    }

    pub fn ordinary_set(&self, name: Symbol, value: JSValue, receiver: JSValue) -> bool {
        // TODO: use the receiver for getters
        let own_desc = if let Some(desc) = self.get_own_property(name) {
            desc
        } else {
            if let Some(parent) = self.get_prototype_of() {
                return parent.ordinary_set(name, value, receiver);
            }
            PropertyDescriptor::default()
                .writable(true)
                .enumerable(true)
                .configurable(true)
        };
        // TODO: accessors
        if !own_desc.writable {
            return false;
        }
        let Some(recv_obj) = receiver.as_object() else {
            return false;
        };
        if let Some(existing) = recv_obj.get_own_property(name) {
            if !existing.writable {
                return false;
            }
            let prop = PropertyDescriptor::new(value)
                .writable(true)
                .enumerable(true)
                .configurable(true);
            recv_obj.define_own_property(name, prop)
        } else {
            recv_obj.create_data_property(name, value)
        }
    }

    pub fn has_own_property(&self, name: Symbol) -> bool {
        self.properties.borrow().contains_key(&name)
    }

    pub fn has_property(&self, name: Symbol) -> bool {
        if self.has_own_property(name) {
            return true;
        }

        let mut cur = self.get_prototype_of();
        while let Some(parent) = cur {
            if parent.has_own_property(name) {
                return true;
            }
            cur = parent.get_prototype_of();
        }
        false
    }

    pub fn ordinary_object(prototype: Option<Shared<JSObject>>) -> Self {
        Self {
            prototype: RefCell::new(prototype),
            extensible: true,
            properties: RefCell::new(HashMap::new()),
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
            prototype: RefCell::new(None),
            extensible: true,
            properties: RefCell::new(HashMap::new()),
            object: Object::BuiltinFunction(function),
        }
    }

    pub fn extensible(&self) -> bool {
        self.extensible
    }

    pub fn callable(&self) -> bool {
        self.object.callable()
    }

    /// Asserting that this is a function, retrieve the [`ThisMode`]
    pub fn this_mode(&self) -> ThisMode {
        match &self.object {
            Object::Function(f) => f.this_mode,
            Object::BuiltinFunction(_) => todo!("builtin thismode"),
            _ => panic!("not a function"),
        }
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
        match self.object.clone() {
            Object::BuiltinFunction(b) => {
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
        self.realm.push_execution_context(callee_context);
        let result = (self.func)(self.realm.clone(), this, args);
        self.realm.pop_execution_context();
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
        this: JSValue,
        args: Vec<JSValue>,
    ) {
        // PrepareForOrdinaryCall
        let realm = self.realm.clone();
        let function_obj = function_obj.as_object().unwrap();
        let local_env = EnvironmentRecord::new_function_environment(function_obj.clone());

        let callee_context = ExecutionContext::from_realm_and_function(
            realm.clone(),
            JSValue::object(function_obj.clone()),
            local_env.clone(),
            local_env.clone(),
            script,
        );
        // OrdinaryCallBindThis
        let this_mode = function_obj.this_mode();
        if this_mode != ThisMode::Lexical {
            let callee_realm = realm;
            let this_value = if this.is_null() || this.is_undefined() {
                callee_realm.global_env.borrow().global_this().clone()
            } else {
                this.to_object()
            };
            local_env.bind_this_value(JSValue::object(this_value));
        }

        self.realm.push_execution_context(callee_context);
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

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ThisMode {
    Lexical,
    NonLexical,
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
