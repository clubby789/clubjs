#![allow(unused)]

use crate::{intern::Symbol, lex::kw, util::NonEmptyStack};
use environment_record::{
    DeclarativeEnvironmentRecord, EnvironmentRecord, GlobalEnvironmentRecord,
    ObjectEnvironmentRecord,
};
use realm::Realm;
use std::{
    cell::{Ref, RefCell, RefMut},
    collections::{hash_map::Entry, HashMap},
    ops::Deref,
    rc::Rc,
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
