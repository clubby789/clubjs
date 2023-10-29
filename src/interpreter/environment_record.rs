use std::{
    cell::{Cell, RefCell},
    collections::{hash_map::Entry, HashMap, HashSet},
    rc::Rc,
};

use crate::intern::Symbol;

use super::{
    value::{PropertyDescriptor, ThisMode},
    JSObject, JSValue, Shared,
};

// Environment records should use Rc on the outside, and RefCell on their fields
// This is because Rc<RefCell<..>> tends to lead to hard to debug long lasting
// borrows. No EnvironmentRecord variant should have &mut self methods

#[derive(Default, Debug)]
pub struct GlobalEnvironmentRecord {
    object_record: RefCell<ObjectEnvironmentRecord>,
    global_this: Shared<JSObject>,
    declarative_record: RefCell<DeclarativeEnvironmentRecord>,
    var_names: RefCell<HashSet<Symbol>>,
}

impl GlobalEnvironmentRecord {
    pub fn new_global_environment(g: Shared<JSObject>, this_value: Shared<JSObject>) -> Self {
        let object_record = RefCell::new(ObjectEnvironmentRecord::new_object_environment(g, None));
        let dcl_rec = RefCell::new(DeclarativeEnvironmentRecord::new(None));

        Self {
            object_record,
            global_this: this_value,
            declarative_record: dcl_rec,
            var_names: RefCell::new(HashSet::new()),
        }
    }

    pub fn global_this(&self) -> &Shared<JSObject> {
        &self.global_this
    }

    pub fn create_global_var_binding(&self, name: Symbol, deletable: bool) {
        let should_create_binding = {
            let global_obj = self.object_record.borrow();
            let global_obj = &global_obj.binding_object;
            let has_prop = global_obj.has_own_property(name);
            let extensible = global_obj.extensible();
            !has_prop && extensible
        };

        if should_create_binding {
            self.object_record
                .borrow_mut()
                .create_mutable_binding(name, deletable);
        }

        self.var_names.borrow_mut().insert(name);
    }

    pub fn create_global_function_binding(&self, name: Symbol, value: JSValue, deletable: bool) {
        let obj_rec = self.object_record.borrow();
        let global_obj = &obj_rec.binding_object;
        let existing = global_obj.get_own_property(name);
        let desc = match existing {
            Some(p) if !p.is_configurable() => PropertyDescriptor::new(value)
                .writable(true)
                .enumerable(true)
                .configurable(deletable),
            _ => PropertyDescriptor::new(value),
        };
        global_obj.define_property_or_throw(name, desc);
        self.var_names.borrow_mut().insert(name);
    }

    pub fn create_mutable_binding(&self, name: Symbol, deletable: bool) {
        if self.declarative_record.borrow().has_binding(name) {
            panic!("TypeError: binding `{name}` exists");
        }
        self.declarative_record
            .borrow_mut()
            .create_mutable_binding(name, deletable);
    }

    pub fn initialize_binding(&self, name: Symbol, value: JSValue) {
        if self.declarative_record.borrow().has_binding(name) {
            return self
                .declarative_record
                .borrow_mut()
                .initialize_binding(name, value);
        }
        self.object_record
            .borrow_mut()
            .initialize_binding(name, value);
    }

    pub fn set_mutable_binding(&self, name: Symbol, value: JSValue) {
        if self.declarative_record.borrow().has_binding(name) {
            return self
                .declarative_record
                .borrow_mut()
                .set_mutable_binding(name, value);
        }
        self.object_record
            .borrow_mut()
            .set_mutable_binding(name, value);
    }

    pub fn has_binding(&self, name: Symbol) -> bool {
        self.declarative_record.borrow().has_binding(name)
            || self.object_record.borrow().has_binding(name)
    }

    pub fn get_this_binding(&self) -> JSValue {
        JSValue::object(self.global_this.clone())
    }

    pub fn get_binding_value(&self, name: Symbol) -> JSValue {
        if self.declarative_record.borrow().has_binding(name) {
            self.declarative_record.borrow_mut().get_binding_value(name)
        } else {
            self.object_record.borrow_mut().get_binding_value(name)
        }
    }
}

#[derive(Default, Debug)]
pub struct ObjectEnvironmentRecord {
    outer_env: Option<EnvironmentRecord>,
    binding_object: Shared<JSObject>,
}

impl ObjectEnvironmentRecord {
    pub fn new_object_environment(
        binding_object: Shared<JSObject>,
        outer_env: Option<EnvironmentRecord>,
    ) -> Self {
        Self {
            binding_object,
            outer_env,
        }
    }

    pub fn create_mutable_binding(&self, name: Symbol, deletable: bool) {
        self.binding_object.define_property_or_throw(
            name,
            PropertyDescriptor::default()
                .writable(true)
                .enumerable(true)
                .configurable(deletable),
        );
    }

    pub fn initialize_binding(&self, name: Symbol, value: JSValue) {
        self.set_mutable_binding(name, value);
    }

    pub fn set_mutable_binding(&self, name: Symbol, value: JSValue) {
        // TODO: if strict check still exists
        // let still_exists = self.binding_object.borrow().has_property(name);
        let obj = self.binding_object.clone();
        self.binding_object
            .ordinary_set(name, value, JSValue::object(obj));
    }

    pub fn has_binding(&self, name: Symbol) -> bool {
        let binding_object = &self.binding_object;
        binding_object.has_property(name)
    }

    pub fn get_binding_value(&self, name: Symbol) -> JSValue {
        self.binding_object
            .ordinary_get(name, JSValue::object(self.binding_object.clone()))
    }
}

/// Map of bindings to their values and their mutability
#[derive(Default, Debug)]
pub struct DeclarativeEnvironmentRecord {
    outer_env: Option<EnvironmentRecord>,
    bindings: RefCell<HashMap<Symbol, (Option<JSValue>, bool)>>,
}

impl DeclarativeEnvironmentRecord {
    pub fn new(outer_env: Option<EnvironmentRecord>) -> Self {
        Self {
            outer_env,
            bindings: RefCell::new(HashMap::new()),
        }
    }

    pub fn has_binding(&self, name: Symbol) -> bool {
        self.bindings.borrow().contains_key(&name)
    }

    pub fn create_mutable_binding(&self, name: Symbol, deletable: bool) {
        debug_assert!(
            !self.bindings.borrow().contains_key(&name),
            "binding `{name}` exists"
        );
        self.bindings.borrow_mut().insert(name, (None, deletable));
    }

    pub fn initialize_binding(&self, name: Symbol, value: JSValue) {
        self.bindings
            .borrow_mut()
            .get_mut(&name)
            .expect("we should already have a binding")
            .0 = Some(value);
    }

    pub fn set_mutable_binding(&self, name: Symbol, value: JSValue) {
        match self.bindings.borrow_mut().entry(name) {
            Entry::Occupied(mut o) => {
                if o.get().0.is_none() {
                    panic!("ReferenceError: {name} is uninitialized");
                }
                if !o.get().1 {
                    panic!("TypeError: {name} is immutable");
                }
                o.get_mut().0 = Some(value);
            }
            Entry::Vacant(v) => {
                // TODO: referenceerror if strict
                v.insert((Some(value), true));
            }
        }
    }

    pub fn get_binding_value(&self, name: Symbol) -> JSValue {
        self.bindings
            .borrow()
            .get(&name)
            .map(|(v, _)| v.clone())
            .unwrap_or_else(|| unreachable!("no binding for {name}"))
            .unwrap_or_else(|| panic!("ReferenceError: {name} is uninitialized"))
    }
}

#[derive(Debug, Clone)]
pub enum EnvironmentRecord {
    Declarative(Rc<DeclarativeEnvironmentRecord>),
    Function(Rc<FunctionEnvironmentRecord>),
    Global(Rc<GlobalEnvironmentRecord>),
}

#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
enum ThisBindingStatus {
    Lexical,
    Initialized,
    #[default]
    Uninitialized,
}

#[derive(Default, Debug)]
pub struct FunctionEnvironmentRecord {
    decl: DeclarativeEnvironmentRecord,
    this_value: RefCell<JSValue>,
    this_binding_status: Cell<ThisBindingStatus>,
    function_object: Shared<JSObject>,
    // new_target: (),
}

impl FunctionEnvironmentRecord {
    pub fn get_this_binding(&self) -> JSValue {
        match self.this_binding_status.get() {
            ThisBindingStatus::Lexical => panic!("assertion failed"),
            ThisBindingStatus::Uninitialized => panic!("ReferenceError: `this` is uninitialized"),
            ThisBindingStatus::Initialized => self.this_value.borrow().clone(),
        }
    }

    pub fn bind_this_value(&self, value: JSValue) {
        match self.this_binding_status.get() {
            ThisBindingStatus::Lexical => panic!("assertion failed"),
            ThisBindingStatus::Initialized => panic!("ReferenceError: `this` is initialized"),
            ThisBindingStatus::Uninitialized => *self.this_value.borrow_mut() = value,
        }
        self.this_binding_status.set(ThisBindingStatus::Initialized);
    }

    pub fn has_this_binding(&self) -> bool {
        !matches!(self.this_binding_status.get(), ThisBindingStatus::Lexical)
    }
}

impl EnvironmentRecord {
    pub fn outer_env(&self) -> Option<EnvironmentRecord> {
        match self {
            EnvironmentRecord::Declarative(d) => d.outer_env.clone(),
            EnvironmentRecord::Function(f) => f.decl.outer_env.clone(),
            EnvironmentRecord::Global(_) => None,
        }
    }

    pub fn has_binding(&self, name: Symbol) -> bool {
        match self {
            EnvironmentRecord::Declarative(d) => d.has_binding(name),
            EnvironmentRecord::Function(f) => f.decl.has_binding(name),
            EnvironmentRecord::Global(g) => g.has_binding(name),
        }
    }

    pub fn has_this_binding(&self) -> bool {
        match self {
            EnvironmentRecord::Declarative(_d) => false,
            EnvironmentRecord::Function(f) => f.has_this_binding(),
            EnvironmentRecord::Global(_g) => true,
        }
    }

    pub fn get_this_binding(&self) -> JSValue {
        match self {
            EnvironmentRecord::Global(g) => g.get_this_binding(),
            EnvironmentRecord::Function(f) => f.get_this_binding(),
            _ => unreachable!(),
        }
    }

    /// Binds the 'this' value, asserting that this is a function record
    pub fn bind_this_value(&self, value: JSValue) {
        let EnvironmentRecord::Function(f) = self else {
            panic!("not a FunctionEnvironmentRecord");
        };
        f.bind_this_value(value);
    }

    pub fn get_binding_value(&self, name: Symbol) -> JSValue {
        match self {
            EnvironmentRecord::Declarative(d) => d.get_binding_value(name),
            EnvironmentRecord::Function(f) => f.decl.get_binding_value(name),
            EnvironmentRecord::Global(g) => g.get_binding_value(name),
        }
    }

    pub fn set_mutable_binding(&self, name: Symbol, value: JSValue) {
        match self {
            EnvironmentRecord::Declarative(d) => d.set_mutable_binding(name, value),
            EnvironmentRecord::Function(f) => f.decl.set_mutable_binding(name, value),
            EnvironmentRecord::Global(g) => g.set_mutable_binding(name, value),
        }
    }

    pub fn initialize_binding(&self, name: Symbol, value: JSValue) {
        match self {
            EnvironmentRecord::Declarative(d) => d.initialize_binding(name, value),
            EnvironmentRecord::Function(f) => f.decl.initialize_binding(name, value),
            EnvironmentRecord::Global(g) => g.initialize_binding(name, value),
        }
    }

    pub fn new_function_environment(f: Shared<JSObject>) -> Self {
        let this_binding_status = match f.this_mode() {
            ThisMode::Lexical => ThisBindingStatus::Lexical,
            _ => ThisBindingStatus::Uninitialized,
        };

        let f = FunctionEnvironmentRecord {
            decl: DeclarativeEnvironmentRecord::new(Some(f.environment_record())),
            this_value: RefCell::new(JSValue::undefined()),
            this_binding_status: Cell::new(this_binding_status),
            function_object: f.clone(),
        };
        EnvironmentRecord::Function(Rc::new(f))
    }
}

impl Default for EnvironmentRecord {
    fn default() -> Self {
        EnvironmentRecord::Declarative(Rc::new(DeclarativeEnvironmentRecord::default()))
    }
}
