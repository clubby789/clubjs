use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::{Rc, Weak};

use crate::intern::Symbol;
use crate::lex::kw;

use super::value;
use super::{Agent, ExecutionContext, GlobalEnvironmentRecord};
use super::{JSObject, JSValue, PropertyDescriptor, Shared};

pub(super) struct Realm {
    intrinsics: RealmIntrinsics,
    global_object: Shared<JSObject>,
    global_env: GlobalEnvironmentRecord,
    agent: Weak<RefCell<Agent>>,
}

impl Debug for Realm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Realm").finish()
    }
}

pub struct RealmIntrinsics;

impl Realm {
    pub fn new() -> Self {
        Self {
            intrinsics: RealmIntrinsics,
            global_object: Shared::new(JSObject::default()),
            global_env: GlobalEnvironmentRecord::default(),
            agent: Weak::new(),
        }
    }

    pub(super) fn set_agent(&mut self, agent: Weak<RefCell<Agent>>) {
        assert!(
            self.agent.upgrade().is_none(),
            "a realm's agent should only be set once, after the agent is constructed"
        );
        self.agent = agent;
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

    fn agent(&self) -> Rc<RefCell<Agent>> {
        self.agent
            .upgrade()
            .expect("realm should not outlive agent")
    }

    pub fn push_execution_context(&self, context: ExecutionContext) {
        self.agent().borrow_mut().push_context(context);
    }

    pub fn pop_execution_context(&self) -> ExecutionContext {
        self.agent().borrow_mut().pop_context()
    }
}

impl Shared<Realm> {
    pub fn set_custom_global_bindings(&self) {
        let name = kw::console_log;
        fn console_log(_: Shared<Realm>, _: Option<JSValue>, args: Vec<JSValue>) -> JSValue {
            println!("{args:?}");
            JSValue::undefined()
        }
        let value = JSObject::builtin_function_object(console_log, 0, name, self.clone());
        let prop = PropertyDescriptor::default()
            .writable(true)
            .configurable(true)
            .enumerable(true);
        self.borrow()
            .global_object
            .borrow_mut()
            .define_property_or_throw(name, prop)
    }
}
