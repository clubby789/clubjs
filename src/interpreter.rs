#![allow(unused)]

use crate::{intern::Symbol, lex::kw, util::NonEmptyStack};
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{hash_map::Entry, HashMap},
    ops::Deref,
    rc::Rc,
};

struct Realm {
    intrinsics: RealmIntrinsics,
    global_object: Shared<JSObject>,
    global_env: GlobalEnvironmentRecord,
}

macro_rules! make_prop {
    () => {
        PropertyDescriptor {
            value: JSValue::undefined(),
            writable: true,
            enumerable: true,
            configurable: true,
        }
    };
    ($value:expr) => {
        PropertyDescriptor {
            value: $value,
            writable: false,
            enumerable: false,
            configurable: false,
        }
    };
    ($value:expr, w) => {
        PropertyDescriptor {
            value: $value,
            writable: true,
            enumerable: false,
            configurable: false,
        }
    };
}

impl Realm {
    pub fn new() -> Self {
        Self {
            intrinsics: RealmIntrinsics,
            global_object: Shared::new(JSObject::default()),
            global_env: GlobalEnvironmentRecord::default(),
        }
    }

    pub fn set_global_object(
        &mut self,
        global_obj: Option<Shared<JSObject>>,
        this_value: Option<Shared<JSObject>>,
    ) {
        let global_obj = global_obj.unwrap_or_else(|| todo!("OrdinaryObjectCreate"));
        let this_value = this_value.unwrap_or_else(|| global_obj.clone());
        self.global_object = global_obj.clone();
        self.global_env = GlobalEnvironmentRecord::new_global_environment(global_obj, this_value)
    }

    pub fn set_default_global_bindings(&mut self) {
        // TODO: add functions constructors and other props
        let global = self.global_object.clone();
        for (name, prop) in [
            (
                kw::globalThis,
                make_prop!(self.global_env.global_this.clone().into(), w),
            ),
            // TODO: floats
            (kw::Infinity, make_prop!(JSValue::undefined())),
            (kw::NaN, make_prop!(JSValue::undefined())),
            (kw::undefined, make_prop!(JSValue::undefined())),
        ] {
            self.global_object
                .borrow_mut()
                .define_property_or_throw(name, prop);
        }
    }

    pub fn set_custom_global_bindings(&mut self) {
        let name = kw::console_log;
        let prop = make_prop!();
        self.global_object
            .borrow_mut()
            .define_property_or_throw(name, prop)
    }
}

struct RealmIntrinsics;

// TODO: make this gc?
#[derive(Debug, Default)]
pub struct Shared<T>(Rc<RefCell<T>>);

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Shared<T> {
    pub fn new(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }

    pub fn borrow(&self) -> Ref<'_, T> {
        self.0.borrow()
    }

    pub fn borrow_mut(&self) -> RefMut<'_, T> {
        self.0.borrow_mut()
    }
}

impl<T> Deref for Shared<T> {
    type Target = Rc<RefCell<T>>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

#[derive(Clone, Debug)]
struct JSValue {
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
struct JSObject {
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
struct PropertyDescriptor {
    value: JSValue,
    writable: bool,
    // get: (),
    // set: (),
    enumerable: bool,
    configurable: bool,
}

#[derive(Default)]
struct GlobalEnvironmentRecord {
    er: EnvironmentRecord,
    object_record: ObjectEnvironmentRecord,
    global_this: Shared<JSObject>,
    declarative_record: DeclarativeEnvironmentRecord,
    var_names: Vec<Symbol>,
}

impl GlobalEnvironmentRecord {
    pub fn new_global_environment(g: Shared<JSObject>, this_value: Shared<JSObject>) -> Self {
        let object_record = ObjectEnvironmentRecord::new_object_environment(g, false, None);
        let dcl_rec = DeclarativeEnvironmentRecord::new(None);

        Self {
            er: EnvironmentRecord::default(),
            object_record,
            global_this: this_value,
            declarative_record: dcl_rec,
            var_names: vec![],
        }
    }
}

#[derive(Default)]
struct ObjectEnvironmentRecord {
    er: EnvironmentRecord,
    binding_object: Shared<JSObject>,
    is_with_environment: bool,
}

impl ObjectEnvironmentRecord {
    pub fn new_object_environment(
        binding_object: Shared<JSObject>,
        is_with_environment: bool,
        outer_env: Option<Shared<EnvironmentRecord>>,
    ) -> Self {
        Self {
            binding_object,
            is_with_environment,
            er: EnvironmentRecord { outer_env },
        }
    }
}

/// Map of bindings to their values and their mutability
#[derive(Default)]
struct DeclarativeEnvironmentRecord {
    er: EnvironmentRecord,
    bindings: HashMap<Symbol, (Option<JSValue>, bool)>,
}

impl DeclarativeEnvironmentRecord {
    pub fn new(er: Option<EnvironmentRecord>) -> Self {
        Self {
            er: er.unwrap_or_default(),
            bindings: HashMap::new(),
        }
    }
}

#[derive(Default)]
struct EnvironmentRecord {
    outer_env: Option<Shared<EnvironmentRecord>>,
}

struct ExecutionContext {
    function: JSValue,
    realm: Rc<RefCell<Realm>>,
}

impl ExecutionContext {
    pub fn new() -> Self {
        Self {
            function: JSValue::null(),
            realm: Rc::new(RefCell::new(Realm::new())),
        }
    }

    pub fn from_realm(realm: Rc<RefCell<Realm>>) -> ExecutionContext {
        Self {
            function: JSValue::null(),
            realm,
        }
    }

    pub fn from_realm_and_function(
        realm: Rc<RefCell<Realm>>,
        function: JSValue,
    ) -> ExecutionContext {
        Self { function, realm }
    }
}

pub struct Agent {
    contexts: NonEmptyStack<ExecutionContext>,
}

impl Agent {
    pub fn new() -> Self {
        let realm = Rc::new(RefCell::new(Realm::new()));
        let context = ExecutionContext::from_realm(realm.clone());
        let mut contexts = NonEmptyStack::new(context);
        {
            let mut realm = contexts.last_mut().realm.borrow_mut();
            realm.set_global_object(None, None);
        }

        Self { contexts }
    }

    fn current_context(&self) -> &ExecutionContext {
        self.contexts.last()
    }

    fn current_context_mut(&mut self) -> &mut ExecutionContext {
        self.contexts.last_mut()
    }
}
