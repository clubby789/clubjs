use std::collections::HashMap;

use crate::intern::Symbol;

use super::{value::JSValue, JSObject, Shared};

#[derive(Default)]
pub struct GlobalEnvironmentRecord {
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

    pub fn outer_env(&self) -> Option<&Shared<EnvironmentRecord>> {
        self.er.outer_env()
    }

    pub fn global_this(&self) -> &Shared<JSObject> {
        &self.global_this
    }
}

#[derive(Default)]
pub struct ObjectEnvironmentRecord {
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

    pub fn outer_env(&self) -> Option<&Shared<EnvironmentRecord>> {
        self.er.outer_env()
    }
}

/// Map of bindings to their values and their mutability
#[derive(Default)]
pub struct DeclarativeEnvironmentRecord {
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

    pub fn outer_env(&self) -> Option<&Shared<EnvironmentRecord>> {
        self.er.outer_env()
    }
}

#[derive(Default)]
pub struct EnvironmentRecord {
    outer_env: Option<Shared<EnvironmentRecord>>,
}

impl EnvironmentRecord {
    pub fn outer_env(&self) -> Option<&Shared<EnvironmentRecord>> {
        self.outer_env.as_ref()
    }
}
