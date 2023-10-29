use std::cell::RefCell;
use std::fmt::Debug;
use std::rc::{Rc, Weak};
use std::sync::atomic::{AtomicBool, Ordering};

use crate::ast::FunctionParam;
use crate::codegen::{Function, Script};
use crate::lex::kw;

use super::environment_record::EnvironmentRecord;
use super::value::ThisMode;
use super::{Agent, ExecutionContext, GlobalEnvironmentRecord};
use super::{JSObject, JSValue, PropertyDescriptor, Shared};

pub struct Realm {
    intrinsics: RealmIntrinsics,
    global_object: RefCell<Shared<JSObject>>,
    pub global_env: RefCell<Rc<GlobalEnvironmentRecord>>,
    agent: RefCell<Weak<Agent>>,
}

impl Debug for Realm {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Realm").finish()
    }
}

#[allow(non_snake_case)]
pub struct RealmIntrinsics {
    pub Object: ObjectIntrinsic,
}

impl RealmIntrinsics {
    pub fn new() -> Self {
        Self {
            Object: ObjectIntrinsic::new(),
        }
    }
}

pub struct ObjectIntrinsic {
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
            global_object: RefCell::new(Shared::new(JSObject::default())),
            global_env: RefCell::new(Rc::new(GlobalEnvironmentRecord::default())),
            agent: RefCell::new(Weak::new()),
        }
    }

    pub(super) fn set_agent(&self, agent: Weak<Agent>) {
        assert!(
            self.agent.borrow().upgrade().is_none(),
            "a realm's agent should only be set once, after the agent is constructed"
        );
        *self.agent.borrow_mut() = agent;
    }

    pub fn set_global_object(
        &self,
        global_obj: Option<Shared<JSObject>>,
        this_value: Option<Shared<JSObject>>,
    ) {
        let global_obj = global_obj.unwrap_or_else(|| {
            Shared::new(JSObject::ordinary_object(Some(
                self.intrinsics.Object.prototype.clone(),
            )))
        });
        let this_value = this_value.unwrap_or_else(|| global_obj.clone());
        *self.global_object.borrow_mut() = global_obj.clone();
        *self.global_env.borrow_mut() = Rc::new(GlobalEnvironmentRecord::new_global_environment(
            global_obj, this_value,
        ))
    }

    pub fn set_default_global_bindings(&self) {
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
            global.borrow().define_property_or_throw(name, prop);
        }
    }

    fn agent(&self) -> Rc<Agent> {
        self.agent
            .borrow()
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

    pub fn intrinsics(&self) -> &RealmIntrinsics {
        &self.intrinsics
    }
}

impl Shared<Realm> {
    pub fn set_custom_global_bindings(&self) {
        let console = self.console_object();
        let prop = PropertyDescriptor::new(console)
            .writable(true)
            .configurable(true)
            .enumerable(true);
        self.global_object
            .borrow()
            .define_property_or_throw(kw::console, prop)
    }

    fn console_object(&self) -> JSValue {
        let obj = JSObject::ordinary_object(None);
        fn log(_: Shared<Realm>, _: JSValue, args: Vec<JSValue>) -> JSValue {
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
        let log_obj = JSValue::object(Shared::new(JSObject::builtin_function_object(
            log,
            0,
            kw::log,
            self.clone(),
        )));
        let prop = PropertyDescriptor::new(log_obj).enumerable(true);
        obj.define_own_property(kw::log, prop);
        JSValue::object(Shared::new(obj))
    }

    pub fn instantiate_function_object(
        &self,
        body: Rc<Function>,
        env: EnvironmentRecord,
    ) -> JSValue {
        // TODO: %Function.prototype%
        let params = body
            .params()
            .map(|p| {
                let FunctionParam::Normal(sym) = p else {
                    todo!("defaulted params aren't supported yet")
                };
                *sym
            })
            .collect();
        let f = JSObject::ordinary_function_object(
            None,
            params,
            body,
            ThisMode::Global,
            env,
            self.clone(),
        );
        // TODO: set function name
        JSValue::object(Shared::new(f))
    }
}
