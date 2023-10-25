use std::collections::{HashMap, HashSet};

use crate::intern::Symbol;

use super::{value::PropertyDescriptor, JSObject, JSValue, Shared};

#[derive(Default, Debug)]
pub struct GlobalEnvironmentRecord {
    object_record: ObjectEnvironmentRecord,
    global_this: Shared<JSObject>,
    declarative_record: DeclarativeEnvironmentRecord,
    var_names: HashSet<Symbol>,
}

impl GlobalEnvironmentRecord {
    pub fn new_global_environment(g: Shared<JSObject>, this_value: Shared<JSObject>) -> Self {
        let object_record = ObjectEnvironmentRecord::new_object_environment(g, false, None);
        let dcl_rec = DeclarativeEnvironmentRecord::new(None);

        Self {
            object_record,
            global_this: this_value,
            declarative_record: dcl_rec,
            var_names: HashSet::new(),
        }
    }

    pub fn global_this(&self) -> &Shared<JSObject> {
        &self.global_this
    }

    pub fn create_global_var_binding(&mut self, name: Symbol, deletable: bool) {
        let should_create_binding = {
            let global_obj = self.object_record.binding_object.borrow();
            let has_prop = global_obj.has_own_property(name);
            let extensible = global_obj.extensible();
            !has_prop && extensible
        };

        if should_create_binding {
            self.object_record.create_mutable_binding(name, deletable);
        }

        self.var_names.insert(name);
    }

    pub fn create_global_function_binding(
        &mut self,
        name: Symbol,
        value: JSValue,
        deletable: bool,
    ) {
        let obj_rec = &self.object_record;
        let global_obj = &obj_rec.binding_object;
        let existing = global_obj.borrow().get_own_property(name);
        let desc = match existing {
            Some(p) if !p.is_configurable() => PropertyDescriptor::new(value)
                .writable(true)
                .enumerable(true)
                .configurable(deletable),
            _ => PropertyDescriptor::new(value),
        };
        global_obj.borrow_mut().define_property_or_throw(name, desc);
        self.var_names.insert(name);
    }

    pub fn create_mutable_binding(&mut self, name: Symbol, deletable: bool) {
        if self.declarative_record.has_binding(name) {
            panic!("TypeError: binding `{name}` exists");
        }
        self.declarative_record
            .create_mutable_binding(name, deletable);
    }

    pub fn has_binding(&self, name: Symbol) -> bool {
        self.declarative_record.has_binding(name) || self.object_record.has_binding(name)
    }

    pub fn get_binding_value(&self, name: Symbol) -> JSValue {
        if self.declarative_record.has_binding(name) {
            self.declarative_record.get_binding_value(name)
        } else {
            self.object_record.get_binding_value(name)
        }
    }
}

#[derive(Default, Debug)]
pub struct ObjectEnvironmentRecord {
    outer_env: Option<EnvironmentRecord>,
    binding_object: Shared<JSObject>,
    is_with_environment: bool,
}

impl ObjectEnvironmentRecord {
    pub fn new_object_environment(
        binding_object: Shared<JSObject>,
        is_with_environment: bool,
        outer_env: Option<EnvironmentRecord>,
    ) -> Self {
        Self {
            binding_object,
            is_with_environment,
            outer_env,
        }
    }

    pub fn create_mutable_binding(&mut self, name: Symbol, deletable: bool) {
        self.binding_object.borrow_mut().define_property_or_throw(
            name,
            PropertyDescriptor::default()
                .writable(true)
                .enumerable(true)
                .configurable(deletable),
        );
    }

    pub fn has_binding(&self, name: Symbol) -> bool {
        let binding_object = &self.binding_object;
        if !binding_object.borrow().has_property(name) {
            return false;
        }
        if !self.is_with_environment {
            return true;
        }

        // todo: unscopables

        true
    }

    pub fn get_binding_value(&self, name: Symbol) -> JSValue {
        self.binding_object
            .borrow()
            .ordinary_get(name, JSValue::object(self.binding_object.clone()))
    }
}

/// Map of bindings to their values and their mutability
#[derive(Default, Debug)]
pub struct DeclarativeEnvironmentRecord {
    outer_env: Option<EnvironmentRecord>,
    bindings: HashMap<Symbol, (Option<JSValue>, bool)>,
}

impl DeclarativeEnvironmentRecord {
    pub fn new(outer_env: Option<EnvironmentRecord>) -> Self {
        Self {
            outer_env,
            bindings: HashMap::new(),
        }
    }

    pub fn has_binding(&self, name: Symbol) -> bool {
        self.bindings.contains_key(&name)
    }

    pub fn create_mutable_binding(&mut self, name: Symbol, deletable: bool) {
        debug_assert!(
            !self.bindings.contains_key(&name),
            "binding `{name}` exists"
        );
        self.bindings.insert(name, (None, deletable));
    }

    pub fn get_binding_value(&self, name: Symbol) -> JSValue {
        self.bindings
            .get(&name)
            .map(|(v, _)| v.clone())
            .unwrap_or_else(|| unreachable!("no binding for {name}"))
            .unwrap_or_else(|| panic!("ReferenceError: {name} is uninitialized"))
    }
}

#[derive(Debug, Clone)]
pub enum EnvironmentRecord {
    Declarative(Shared<DeclarativeEnvironmentRecord>),
    Object(Shared<ObjectEnvironmentRecord>),
    Global(Shared<GlobalEnvironmentRecord>),
}

impl EnvironmentRecord {
    pub fn outer_env(&self) -> Option<EnvironmentRecord> {
        match self {
            EnvironmentRecord::Declarative(d) => d.borrow().outer_env.clone(),
            EnvironmentRecord::Object(o) => o.borrow().outer_env.clone(),
            EnvironmentRecord::Global(_) => None,
        }
    }

    pub fn set_outer_env(&self, env: Option<EnvironmentRecord>) {
        match self {
            EnvironmentRecord::Declarative(d) => d.borrow_mut().outer_env = env,
            EnvironmentRecord::Object(o) => o.borrow_mut().outer_env = env,
            EnvironmentRecord::Global(_) => panic!("can't set outer env of a global"),
        }
    }

    pub fn has_binding(&self, name: Symbol) -> bool {
        match self {
            EnvironmentRecord::Declarative(d) => d.borrow().has_binding(name),
            EnvironmentRecord::Object(o) => o.borrow().has_binding(name),
            EnvironmentRecord::Global(g) => g.borrow().has_binding(name),
        }
    }

    pub fn get_binding_value(&self, name: Symbol) -> JSValue {
        match self {
            EnvironmentRecord::Declarative(d) => d.borrow().get_binding_value(name),
            EnvironmentRecord::Object(o) => o.borrow().get_binding_value(name),
            EnvironmentRecord::Global(g) => g.borrow().get_binding_value(name),
        }
    }

    pub fn new_function_environment(f: Shared<JSObject>) -> Self {
        // TODO: Add FunctionEnvironmentRecord
        let outer_env = f.borrow().environment_record();
        EnvironmentRecord::Declarative(Shared::new(DeclarativeEnvironmentRecord::new(Some(
            outer_env,
        ))))
    }
}

impl Default for EnvironmentRecord {
    fn default() -> Self {
        EnvironmentRecord::Declarative(Shared::new(DeclarativeEnvironmentRecord::default()))
    }
}
