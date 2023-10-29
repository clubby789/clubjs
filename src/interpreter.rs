use crate::{
    ast,
    codegen::{self, Declaration, Opcode, ScopeAnalysis, Script},
    intern::Symbol,
    span::Node,
};
use either::Either;
use environment_record::{EnvironmentRecord, GlobalEnvironmentRecord};
use realm::Realm;
use std::{
    cell::{Cell, Ref, RefCell, RefMut},
    collections::HashSet,
    ops::Deref,
    path::PathBuf,
    rc::Rc,
};
use value::{JSObject, JSValue, PropertyDescriptor};

use self::value::ThisMode;

mod environment_record;
mod realm;
mod value;

// TODO: make this gc?
#[derive(Default)]
pub struct Shared<T>(Rc<RefCell<T>>);

impl<T: std::fmt::Debug> std::fmt::Debug for Shared<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        std::fmt::Debug::fmt(&self.0, f)
    }
}

impl<T> Clone for Shared<T> {
    fn clone(&self) -> Self {
        Self(self.0.clone())
    }
}

impl<T> Shared<T> {
    pub fn new(value: T) -> Self {
        Self(Rc::new(RefCell::new(value)))
    }

    #[track_caller]
    pub fn borrow(&self) -> Ref<'_, T> {
        self.0.borrow()
    }

    #[track_caller]
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

pub struct ExecutionContext {
    function: JSValue,
    realm: Shared<Realm>,
    lexical_environment: EnvironmentRecord,
    variable_environment: EnvironmentRecord,
    // private_environment: Option<PrivateEnvironmentRecord>,
    script: Rc<Script>,
    state: VmState,
}

impl ExecutionContext {
    pub fn from_realm(
        realm: Shared<Realm>,
        lexical_environment: EnvironmentRecord,
        variable_environment: EnvironmentRecord,
        script: Rc<Script>,
    ) -> ExecutionContext {
        Self {
            function: JSValue::null(),
            realm,
            lexical_environment,
            variable_environment,
            script,
            state: VmState::default(),
        }
    }

    pub fn from_realm_and_function(
        realm: Shared<Realm>,
        function: JSValue,
        lexical_environment: EnvironmentRecord,
        variable_environment: EnvironmentRecord,
        script: Rc<Script>,
    ) -> ExecutionContext {
        Self {
            function,
            realm,
            lexical_environment,
            variable_environment,
            script,
            state: VmState::default(),
        }
    }

    pub fn resolve_binding(&self, name: Symbol, env: Option<EnvironmentRecord>) -> ReferenceRecord {
        let mut env = Some(env.unwrap_or(self.lexical_environment.clone()));
        while let Some(e) = env {
            if e.has_binding(name) {
                return ReferenceRecord {
                    base: Either::Left(e),
                    referenced_name: name,
                    this_value: None,
                };
            }
            env = e.outer_env();
        }
        panic!("could not resolve binding `{name}`");
    }

    // Load a string from this context's string table
    pub fn get_string(&self, index: codegen::StringIndex) -> Symbol {
        if let Some(f) = self.function.as_function() {
            f.code().strings()[index]
        } else {
            self.script.strings()[index]
        }
    }

    // Load a name from this context's name table
    pub fn get_name(&self, index: codegen::NameIndex) -> Symbol {
        if let Some(f) = self.function.as_function() {
            f.code().names()[index]
        } else {
            self.script.names()[index]
        }
    }

    // Load a fuction from this context's anon function table
    pub fn get_func(&self, index: codegen::NameIndex) -> Rc<codegen::Function> {
        if let Some(f) = self.function.as_function() {
            f.code().anon_functions()[index].clone()
        } else {
            self.script.anon_functions()[index].clone()
        }
    }

    pub fn get_op(&self, index: usize) -> Option<Opcode<codegen::TemporaryIndex>> {
        if let Some(f) = self.function.as_function() {
            f.code().opcodes().get(index).copied()
        } else {
            self.script.opcodes().get(index).copied()
        }
    }

    pub fn get_this_environment(&self) -> EnvironmentRecord {
        let mut env = self.lexical_environment.clone();
        while !env.has_this_binding() {
            env = env.outer_env().expect("9.4.3.2.d");
        }
        env
    }
}

pub const REG_COUNT: usize = 255;
/// Interpreter state associated with a specific execution context
struct VmState {
    pc: Cell<usize>,
    acc: Cell<JSValue>,
    regs: [Cell<JSValue>; REG_COUNT],
}

impl VmState {
    pub fn inc_pc(&self) -> usize {
        let old = self.pc.get();
        self.pc.set(old + 1);
        old + 1
    }

    /// Moves from a register into acc, setting the register value
    /// to null
    pub fn mov_reg_acc(&self, reg: usize) {
        self.acc.set(self.regs[reg].take());
    }

    /// Moves from acc into a register, setting the value of acc
    /// to null
    pub fn mov_acc_reg(&self, reg: usize) {
        self.regs[reg].set(self.acc.take());
    }

    /// Copies from a register into another register
    pub fn copy_reg_reg(&self, from: usize, to: usize) {
        let content = self.regs[from].take();
        self.regs[to].set(content.clone());
        self.regs[from].set(content);
    }

    pub fn set_acc(&self, value: JSValue) {
        self.acc.set(value);
    }
}

impl Default for VmState {
    fn default() -> Self {
        // Benchmark shows that std::array::from_fn is slow so make a const
        // value
        const NULL: Cell<JSValue> = Cell::new(JSValue::null());
        Self {
            pc: Default::default(),
            acc: NULL,
            regs: [NULL; 255],
        }
    }
}

#[derive(Clone, Debug)]
pub struct ReferenceRecord {
    base: Either<EnvironmentRecord, JSValue>,
    referenced_name: Symbol,
    // strict: bool,
    this_value: Option<JSValue>,
}

impl ReferenceRecord {
    pub fn get_this_value(&self) -> JSValue {
        self.this_value
            .clone()
            .unwrap_or_else(|| {
                self.base
                    .clone()
                    .expect_right("callee ensures this is okay")
            })
            .clone()
    }

    pub fn is_property_reference(&self) -> bool {
        self.base.is_right()
    }
}

pub struct Agent {
    realm: Shared<Realm>,
    contexts: RefCell<Vec<ExecutionContext>>,
}

pub struct ScriptRecord {
    realm: Shared<Realm>,
    pub ecma_script_code: Node<ast::Program>,
}

impl Agent {
    pub fn new() -> Rc<Self> {
        let realm = Shared::new(Realm::new());
        // FIXME: either make the real global context here, or refactor so this isn't needed
        // maybe an 'initialise' call which must run before parsing
        let outer_ctx = ExecutionContext::from_realm(
            realm.clone(),
            EnvironmentRecord::default(),
            EnvironmentRecord::default(),
            Rc::new(Script::default()),
        );
        let agent = Rc::new(Self {
            realm: realm.clone(),
            contexts: RefCell::new(vec![outer_ctx]),
        });
        let mut r = realm.borrow_mut();
        r.set_global_object(None, None);
        r.set_default_global_bindings();
        r.set_agent(Rc::downgrade(&agent));
        drop(r);
        realm.set_custom_global_bindings();
        agent
    }

    fn current_context(&self) -> Ref<'_, ExecutionContext> {
        fn get_last(v: &Vec<ExecutionContext>) -> &ExecutionContext {
            v.last().expect("should always have a context")
        }
        Ref::map(self.contexts.borrow(), get_last)
    }

    fn script(&self) -> Rc<Script> {
        self.current_context().script.clone()
    }

    fn push_context(&self, context: ExecutionContext) {
        self.contexts.borrow_mut().push(context)
    }

    fn pop_context(&self) -> ExecutionContext {
        debug_assert!(self.contexts.borrow().len() >= 2);
        self.contexts
            .borrow_mut()
            .pop()
            .expect("should always have a context")
    }

    pub fn parse_script(&self, source: &str, path: PathBuf) -> ScriptRecord {
        ScriptRecord {
            ecma_script_code: ast::Parser::new(source, path).parse(),
            realm: self.current_context().realm.clone(),
        }
    }

    pub fn script_evaluation(&self, script: ScriptRecord) {
        let env = self.realm.borrow().global_env.clone();
        let code = codegen::FunctionBuilder::codegen_script(script.ecma_script_code);
        Self::global_declaration_instantiation(&code, env.clone(), self.realm.clone());

        let global_env = EnvironmentRecord::Global(env);
        let script_context = ExecutionContext::from_realm(
            self.realm.clone(),
            global_env.clone(),
            global_env.clone(),
            Rc::new(code),
        );
        self.push_context(script_context);
        self.run();
        self.pop_context();
    }

    fn global_declaration_instantiation(
        script: &codegen::Script,
        env: Shared<GlobalEnvironmentRecord>,
        realm: Shared<Realm>,
    ) {
        let mut env_r = env.borrow_mut();
        let var_declarations = script.var_scoped_declarations().collect::<Vec<_>>();

        let mut declared_function_names = HashSet::new();
        let mut functions_to_initialize = vec![];
        for (sym, func) in var_declarations.iter().rev().filter_map(|(s, d)| {
            if let Declaration::Function(f) = d {
                Some((*s, f.clone()))
            } else {
                None
            }
        }) {
            if !declared_function_names.insert(sym) {
                continue;
            }
            functions_to_initialize.insert(0, (sym, func));
        }
        let mut declared_var_names = HashSet::new();
        for sym in var_declarations.iter().filter_map(|(s, d)| {
            if let Declaration::Variable = d {
                Some(*s)
            } else {
                None
            }
        }) {
            if declared_function_names.contains(&sym) {
                continue;
            }
            declared_var_names.insert(sym);
        }

        for (dn, _) in script.lexically_scoped_declarations() {
            // TODO: support const
            env_r.create_mutable_binding(dn, false);
        }

        for (name, func) in functions_to_initialize {
            let fo =
                realm.instantiate_function_object(func, EnvironmentRecord::Global(env.clone()));
            env_r.create_global_function_binding(name, fo, false);
        }

        for vn in declared_var_names {
            env_r.create_global_var_binding(vn, false);
        }
    }

    fn run(&self) -> JSValue {
        while self.step() {}
        JSValue::undefined()
    }

    fn step(&self) -> bool {
        let ctx = self.current_context();
        let Some(op) = ctx.get_op(ctx.state.pc.get()) else {
            return false;
        };
        ctx.state.inc_pc();

        match op {
            Opcode::LoadIdent(n) => {
                let name = ctx.get_name(n);
                let reference = ctx.resolve_binding(name, None);
                ctx.state.set_acc(JSValue::reference(reference));
            }
            Opcode::LoadThis => {
                let rec = ctx.get_this_environment();
                ctx.state.set_acc(rec.get_this_binding());
            }
            Opcode::StoreAcc(n) => {
                ctx.state.mov_acc_reg(n);
            }
            Opcode::LoadInt(n) => {
                ctx.state.set_acc(JSValue::int(n));
            }
            Opcode::LoadString(s) => {
                ctx.state.set_acc(JSValue::string(ctx.get_string(s)));
            }
            Opcode::LoadFunc(f) => {
                let func = ctx.get_func(f);
                let env = ctx.lexical_environment.clone();
                // TODO: %Function.prototype%
                let param_list = func.params().map(|p| p.name()).collect();
                let closure = JSObject::ordinary_function_object(
                    None,
                    param_list,
                    func,
                    ThisMode::NonLexical,
                    env,
                    ctx.realm.clone(),
                );
                ctx.state.acc.set(JSValue::object(Shared::new(closure)))
            }
            Opcode::LoadAcc(n) => {
                ctx.state.mov_reg_acc(n);
            }
            Opcode::Move { from, to } => {
                ctx.state.copy_reg_reg(from, to);
            }
            Opcode::Call0 => {
                let target = ctx.state.acc.take();
                drop(ctx);
                self.do_call(target, []);
            }
            Opcode::Call1(arg) => {
                let target = ctx.state.acc.take();
                let arg = ctx.state.regs[arg].take().get_value();
                drop(ctx);
                self.do_call(target, [arg]);
            }
            Opcode::GetNamedProperty { obj, name } => {
                let base_value = ctx.state.regs[obj].take();
                let name = ctx.get_name(name);
                ctx.state.set_acc(JSValue::reference(ReferenceRecord {
                    base: Either::Right(base_value),
                    referenced_name: name,
                    this_value: None,
                }))
            }
            Opcode::GetComputedProperty { obj, index } => {
                let base_value = ctx.state.regs[obj].take();
                let prop_name_value = ctx.state.regs[index].take().get_value();
                let prop_key = prop_name_value.to_primitive(/*TODO: prefer STRING*/);
                // TODO: handle symbol
                let name = prop_key.to_string();
                ctx.state.set_acc(JSValue::reference(ReferenceRecord {
                    base: Either::Right(base_value),
                    referenced_name: name,
                    this_value: None,
                }))
            }
            Opcode::StoreNamedProperty { obj, name } => {
                let base_value = ctx.state.regs[obj].take();
                let name = ctx.get_name(name);
                let value = ctx.state.acc.take();
                let Some(obj) = base_value.as_object() else {
                    unreachable!("StoreNamedProperty is used on objects only");
                };
                obj.borrow_mut().create_data_property_or_throw(name, value);
            }
            Opcode::StoreComputedProperty { obj, index } => {
                let base_value = ctx.state.regs[obj].take();
                let prop_name_value = ctx.state.regs[index].take().get_value();
                let value = ctx.state.acc.take();
                let prop_key = prop_name_value.to_primitive(/*TODO: prefer STRING*/);
                // TODO: handle symbol
                let name = prop_key.to_string();
                let Some(obj) = base_value.as_object() else {
                    unreachable!("StoreNamedProperty is used on objects only");
                };
                obj.borrow_mut().create_data_property_or_throw(name, value);
            }
            Opcode::InitializeIdent { name } => {
                let name = ctx.get_name(name);
                let value = ctx.state.acc.take();
                let reference = ctx.resolve_binding(name, None);
                let Either::Left(record) = reference.base else {
                    unreachable!("Binding is resolved with no env");
                };
                record.initialize_binding(name, value);
            }
            Opcode::StoreIdent { name } => {
                let name = ctx.get_name(name);
                let value = ctx.state.acc.take();
                let reference = ctx.resolve_binding(name, None);
                JSValue::reference(reference).put_value(value);
            }
            Opcode::Add(l, r) => {
                let res = self.do_add(ctx.state.regs[l].take(), ctx.state.regs[r].take());
                ctx.state.set_acc(res);
            }
            Opcode::CreateObject => {
                let res = JSObject::ordinary_object(Some(
                    self.realm.borrow().intrinsics().Object.prototype.clone(),
                ));
                ctx.state.set_acc(JSValue::object(Shared::new(res)));
            }
            Opcode::Return { value } => {
                let return_value = if value {
                    ctx.state.acc.take()
                } else {
                    JSValue::undefined()
                };
                drop(ctx);
                self.realm.borrow().pop_execution_context();
                self.current_context().state.acc.set(return_value);
            }
            o => todo!("{o:?} not implemented"),
        }
        true
    }

    fn do_call<const N: usize>(&self, target: JSValue, args: [JSValue; N]) {
        let this_value = if let Some(reference) = target.as_reference() {
            if reference.is_property_reference() {
                reference.get_this_value()
            } else {
                let ref_env = reference
                    .base
                    .as_ref()
                    .expect_left("checked above that this is not a property");
                ref_env
                    .with_base_object()
                    .map(JSValue::object)
                    .unwrap_or(JSValue::undefined())
            }
        } else {
            JSValue::undefined()
        };
        let target = target.get_value();
        let Some(func) = target.as_object() else {
            panic!("TypeError: call target is not an object")
        };
        if !func.borrow().callable() {
            panic!("TypeError: call target is not callable")
        }
        func.call(self.script(), this_value, args.to_vec());
    }

    fn do_add(&self, left: JSValue, right: JSValue) -> JSValue {
        let lprim = left.to_primitive();
        let rprim = right.to_primitive();
        // TODO: strings
        let lnum = lprim.to_numeric();
        let rnum = rprim.to_numeric();
        assert!(
            lnum.same_type(&rnum),
            "TypeError: adding `{lnum}` and `{rnum}`"
        );
        lnum.add(rnum)
    }
}
