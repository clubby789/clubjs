#![allow(unused)]

use crate::{ast, intern::Symbol, lex::kw, span::Node};
use environment_record::{
    DeclarativeEnvironmentRecord, EnvironmentRecord, GlobalEnvironmentRecord,
    ObjectEnvironmentRecord, PrivateEnvironmentRecord,
};
use realm::Realm;
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{hash_map::Entry, HashMap},
    ops::Deref,
    path::PathBuf,
    rc::{Rc, Weak},
};
use value::{JSObject, JSValue, PropertyDescriptor};

mod environment_record;
mod realm;
mod value;

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

struct ExecutionContext {
    function: JSValue,
    realm: Shared<Realm>,
    lexical_environment: EnvironmentRecord,
    variable_environment: EnvironmentRecord,
    // private_environment: Option<PrivateEnvironmentRecord>,
}

impl ExecutionContext {
    pub fn from_realm(
        realm: Shared<Realm>,
        lexical_environment: EnvironmentRecord,
        variable_environment: EnvironmentRecord,
    ) -> ExecutionContext {
        Self {
            function: JSValue::null(),
            realm,
            lexical_environment,
            variable_environment,
        }
    }

    pub fn from_realm_and_function(
        realm: Shared<Realm>,
        function: JSValue,
        lexical_environment: EnvironmentRecord,
        variable_environment: EnvironmentRecord,
    ) -> ExecutionContext {
        Self {
            function,
            realm,
            lexical_environment,
            variable_environment,
        }
    }
}

pub struct Agent {
    realm: Shared<Realm>,
    contexts: Vec<ExecutionContext>,
}

pub struct ScriptRecord {
    realm: Shared<Realm>,
    pub ecma_script_code: Node<ast::Program>,
}

impl Agent {
    pub fn new() -> Rc<RefCell<Self>> {
        let realm = Shared::new(Realm::new());
        let agent = Rc::new(RefCell::new(Self {
            realm: realm.clone(),
            contexts: vec![],
        }));
        let mut r = realm.borrow_mut();
        r.set_global_object(None, None);
        r.set_agent(Rc::downgrade(&agent));
        agent
    }

    fn current_context(&self) -> &ExecutionContext {
        self.contexts.last().expect("should always have a context")
    }

    fn current_context_mut(&mut self) -> &mut ExecutionContext {
        self.contexts
            .last_mut()
            .expect("should always have a context")
    }

    fn push_context(&mut self, context: ExecutionContext) {
        self.contexts.push(context)
    }

    fn pop_context(&mut self) -> ExecutionContext {
        self.contexts.pop().expect("should always have a context")
    }

    pub fn parse_script(&self, source: &str, path: PathBuf) -> ScriptRecord {
        ScriptRecord {
            ecma_script_code: ast::Parser::new(source, path).parse(),
            realm: self.current_context().realm.clone(),
        }
    }

    pub fn script_evaluation(&mut self, script: ScriptRecord) {
        let global_env = EnvironmentRecord::Global(self.realm.borrow().global_env.clone());
        let script_context = ExecutionContext::from_realm(
            self.realm.clone(),
            global_env.clone(),
            global_env.clone(),
        );
        self.push_context(script_context);
        // TODO: run it lmao
        self.pop_context();
    }
}
