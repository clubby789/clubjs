use std::collections::HashMap;

use crate::intern::Symbol;

use super::{JSObject, JSValue, Shared};

#[derive(Default, Debug)]
pub struct GlobalEnvironmentRecord {
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
            object_record,
            global_this: this_value,
            declarative_record: dcl_rec,
            var_names: vec![],
        }
    }

    pub fn global_this(&self) -> &Shared<JSObject> {
        &self.global_this
    }
}

#[derive(Default, Debug)]
pub struct ObjectEnvironmentRecord {
    outer_env: Option<Shared<EnvironmentRecord>>,
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
            outer_env,
        }
    }

    pub fn outer_env(&self) -> Option<&Shared<EnvironmentRecord>> {
        self.outer_env.as_ref()
    }
}

/// Map of bindings to their values and their mutability
#[derive(Default, Debug)]
pub struct DeclarativeEnvironmentRecord {
    outer_env: Option<Shared<EnvironmentRecord>>,
    bindings: HashMap<Symbol, (Option<JSValue>, bool)>,
}

impl DeclarativeEnvironmentRecord {
    pub fn new(outer_env: Option<Shared<EnvironmentRecord>>) -> Self {
        Self {
            outer_env,
            bindings: HashMap::new(),
        }
    }

    pub fn outer_env(&self) -> Option<&Shared<EnvironmentRecord>> {
        self.outer_env.as_ref()
    }
}

#[derive(Debug, Clone)]
pub enum EnvironmentRecord {
    Declarative(Shared<DeclarativeEnvironmentRecord>),
    Object(Shared<ObjectEnvironmentRecord>),
    Global(Shared<GlobalEnvironmentRecord>),
}

#[derive(Default, Debug)]
pub struct PrivateEnvironmentRecord {
    outer_private_env: Option<Shared<PrivateEnvironmentRecord>>,
}

impl PrivateEnvironmentRecord {
    pub fn outer_private_env(&self) -> Option<&Shared<PrivateEnvironmentRecord>> {
        self.outer_private_env.as_ref()
    }
}
