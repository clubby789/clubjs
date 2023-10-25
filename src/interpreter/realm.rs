use std::fmt::Debug;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicBool, Ordering};

use crate::codegen::Script;
use crate::lex::kw;

use super::{Agent, ExecutionContext, GlobalEnvironmentRecord};
use super::{JSObject, JSValue, PropertyDescriptor, Shared};

pub(super) struct Realm {
    intrinsics: RealmIntrinsics,
    global_object: Shared<JSObject>,
    pub global_env: Shared<GlobalEnvironmentRecord>,
    agent: Weak<Agent>,
}

impl Debug for Realm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Realm").finish()
    }
}

#[allow(non_snake_case)]
pub struct RealmIntrinsics {
    Object: ObjectIntrinsic,
}

impl RealmIntrinsics {
    pub fn new() -> Self {
        Self {
            Object: ObjectIntrinsic::new(),
        }
    }
}

struct ObjectIntrinsic {
    pub prototype: Shared<JSObject>,
}

impl ObjectIntrinsic {
    pub fn new() -> Self {
        Self {
            prototype: Shared::new(JSObject::ordinary_object(None)),
        }
    }
}

impl Realm {
    pub fn new() -> Self {
        static CREATED: AtomicBool = AtomicBool::new(false);
        if CREATED.swap(true, Ordering::Relaxed) {
            panic!("creating multiple realms is not supported");
        }
        Self {
            intrinsics: RealmIntrinsics::new(),
            global_object: Shared::new(JSObject::default()),
            global_env: Shared::new(GlobalEnvironmentRecord::default()),
            agent: Weak::new(),
        }
    }

    pub(super) fn set_agent(&mut self, agent: Weak<Agent>) {
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
        let global_obj = global_obj.unwrap_or_else(|| {
            Shared::new(JSObject::ordinary_object(Some(
                self.intrinsics.Object.prototype.clone(),
            )))
        });
        let this_value = this_value.unwrap_or_else(|| global_obj.clone());
        self.global_object = global_obj.clone();
        self.global_env = Shared::new(GlobalEnvironmentRecord::new_global_environment(
            global_obj, this_value,
        ))
    }

    pub fn set_default_global_bindings(&mut self) {
        // TODO: add functions constructors and other props
        let global = self.global_object.clone();
        for (name, prop) in [
            (
                kw::globalThis,
                PropertyDescriptor::new(self.global_env.borrow().global_this().clone().into())
                    .writable(true),
            ),
            // TODO: floats
            (kw::Infinity, PropertyDescriptor::default()),
            (kw::NaN, PropertyDescriptor::default()),
            (kw::undefined, PropertyDescriptor::default()),
        ] {
            global.borrow_mut().define_property_or_throw(name, prop);
        }
    }

    fn agent(&self) -> Rc<Agent> {
        self.agent
            .upgrade()
            .expect("realm should not outlive agent")
    }

    pub fn push_execution_context(&self, context: ExecutionContext) {
        self.agent().push_context(context);
    }

    pub fn pop_execution_context(&self) -> ExecutionContext {
        self.agent().pop_context()
    }

    pub fn script(&self) -> Rc<Script> {
        self.agent().current_context().script.clone()
    }
}

impl Shared<Realm> {
    pub fn set_custom_global_bindings(&self) {
        let name = kw::console_log;
        fn console_log(_: Shared<Realm>, _: JSValue, args: Vec<JSValue>) -> JSValue {
            let mut peekable = args.into_iter().peekable();
            while let Some(a) = peekable.next() {
                print!("{a}");
                if peekable.peek().is_some() {
                    print!(" ");
                }
            }
            println!();
            JSValue::undefined()
        }
        let value = JSValue::object(Shared::new(JSObject::builtin_function_object(
            console_log,
            0,
            name,
            self.clone(),
        )));
        let prop = PropertyDescriptor::new(value)
            .writable(true)
            .configurable(true)
            .enumerable(true);
        self.borrow()
            .global_object
            .borrow_mut()
            .define_property_or_throw(name, prop)
    }
}