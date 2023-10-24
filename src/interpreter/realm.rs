use crate::lex::kw;

use super::GlobalEnvironmentRecord;
use super::{JSObject, JSValue, PropertyDescriptor, Shared};

pub struct Realm {
    intrinsics: RealmIntrinsics,
    global_object: Shared<JSObject>,
    global_env: GlobalEnvironmentRecord,
}

pub struct RealmIntrinsics;

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
                PropertyDescriptor::new(self.global_env.global_this().clone().into())
                    .writable(true),
            ),
            // TODO: floats
            (kw::Infinity, PropertyDescriptor::default()),
            (kw::NaN, PropertyDescriptor::default()),
            (kw::undefined, PropertyDescriptor::default()),
        ] {
            self.global_object
                .borrow_mut()
                .define_property_or_throw(name, prop);
        }
    }

    pub fn set_custom_global_bindings(&mut self) {
        let name = kw::console_log;
        let prop = PropertyDescriptor::default()
            .writable(true)
            .configurable(true)
            .enumerable(true);
        self.global_object
            .borrow_mut()
            .define_property_or_throw(name, prop)
    }
}
