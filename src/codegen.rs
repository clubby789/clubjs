#![allow(unused)]

use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use indexmap::{set::Slice, IndexSet};
use smallvec::SmallVec;

use crate::{
    ast::{
        self, AssignmentOperator, BinaryOperator, Block, Expression, ExpressionKind, ForInit,
        MemberKey, Program, Scope, Statement, StatementKind, VariableDeclaration, VariableKind,
    },
    intern::Symbol,
    lex::{kw, Literal},
    span::Node,
};

/// [`TemporaryKind`] will either be an incrementing counter or
/// a register/memory reference after optimization`
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Opcode<TemporaryKind> {
    /// Creates a new lexcial environment for the block
    EnterBlock { scope: ScopeIndex },
    /// Restores the previous LexcialEnvironment in the running context
    LeaveBlock,
    /// Trigger the debugger
    Debugger,
    /// Return from the running function. If [`value`] is true,
    /// in the accumulator is used as the return value,
    /// and placed into the previous execution context's accumulator.
    Return { value: bool },
    /// Load <this> into the accumulator
    LoadThis,
    /// Create an empty array in the accumulator
    CreateArray,
    /// Store the value in the accumulator into the array in [`arr`],
    /// at the index in [`idx`]
    StoreInArrayIdx { arr: TemporaryKind, idx: usize },
    /// Store the value in the accumulator into the array in [`arr`],
    /// at the index in temporary [`idx`]
    StoreInArrayReg {
        arr: TemporaryKind,
        idx: TemporaryKind,
    },
    /// Create an empty object in the accumulator
    CreateObject,
    /// Store the value in the accumulator into the object in [`obj`],
    /// at the property name held in [`name`]. Used for initializing
    /// object literals.
    StoreNamedProperty { obj: TemporaryKind, name: NameIndex },
    /// Store the value in the accumulator into the object in [`obj`],
    /// at the property held in [`index`]
    StoreComputedProperty {
        obj: TemporaryKind,
        index: TemporaryKind,
    },
    /// Load the property of the object held in the accumulator held in
    /// [`name`], and store it in the accumulator.
    GetNamedProperty { obj: TemporaryKind, name: NameIndex },
    /// Load the property of the object held in the accumulator held in
    /// [`index`], and store it in the accumulator.
    GetComputedProperty {
        obj: TemporaryKind,
        index: TemporaryKind,
    },
    /// Store the value in the accumulator at the variable held in [`name`]
    StoreIdent { name: NameIndex },
    /// Initialize the variable held in [`name`] with the value in the accumulator
    InitializeIdent { name: NameIndex },
    /// Store the value in the accumulator into the given temporary
    StoreAcc(TemporaryKind),
    /// Load the value in the given temporary into acc
    LoadAcc(TemporaryKind),
    /// Shallow copy the value in [`from`] to [`to`]
    Move {
        from: TemporaryKind,
        to: TemporaryKind,
    },
    /// Load the given integer into acc
    LoadInt(u128),
    /// Load the string at the nth index of this script/function's string table
    LoadString(StringIndex),
    /// Load the function at the nth index of this script/function's anon function table
    LoadFunc(AnonFunctionIndex),
    /// Call the function in the accumulator with no args
    Call0,
    /// Call the function in the accumulator with the arg in the given temporary
    Call1(TemporaryKind),
    /// Resolve the given name in the environment, loading a
    /// [`crate::interpreter::value::JSValue::Reference`] into the accumulator
    LoadIdent(NameIndex),
    /// Perform addition of the two given registers, according to 13.15.3 ApplyStringOrNumericBinaryOperator,
    /// storing the result in the accumulator
    Add(TemporaryKind, TemporaryKind),
    /// Compares the left and right values, according to `7.2.13 IsLessThan`, storing the result
    /// (a boolean or undefined) in the accumulator
    LessThan(TemporaryKind, TemporaryKind),
    /// Create a new environment as per `14.7.4.4 CreatePerIterationEnvironment`, if we have any
    /// lexical declarations
    // TODO: do bindings
    CreatePerIterationEnvironment,
    #[cfg(debug_assertions)]
    /// No-op, but when debug_assertions are enabled all jumps must land on a JumpTarget instruction
    JumpTarget,
    /// Set the pc to the given index
    Jump { idx: usize },
    /// If `ToBoolean(GetValue(accumulator))` is false, set the pc to the given index
    JumpIfFalse { idx: usize },

    /// Placeholder opcode used only during codegen. Should never be reached
    Unreachable,
}

pub type TemporaryIndex = usize;
pub type NameIndex = usize;
pub type StringIndex = usize;
pub type AnonFunctionIndex = usize;
pub type ScopeIndex = usize;

#[derive(Debug, Default)]
pub struct Script {
    code: Vec<Opcode<TemporaryIndex>>,
    variable_declarations: HashMap<Symbol, VariableKind>,
    anon_functions: Vec<Rc<Function>>,
    functions: HashMap<Symbol, Rc<Function>>,
    names: IndexSet<Symbol>,
    strings: IndexSet<Symbol>,
    scopes: Vec<Scope>,
}

impl Script {
    pub fn opcodes(&self) -> &[Opcode<usize>] {
        self.code.as_ref()
    }

    pub fn names(&self) -> &Slice<Symbol> {
        self.names.as_slice()
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    pub fn strings(&self) -> &Slice<Symbol> {
        self.strings.as_slice()
    }

    pub fn anon_functions(&self) -> &[Rc<Function>] {
        self.anon_functions.as_ref()
    }
}

impl ScopeAnalysis for Script {
    fn declarations(&self) -> impl Iterator<Item = (Symbol, Declaration, VariableKind)> {
        self.variable_declarations
            .iter()
            .map(|(n, k)| (*n, Declaration::Variable, *k))
            .chain(
                self.functions
                    .iter()
                    .map(|(n, k)| (*n, Declaration::Function(k.clone()), VariableKind::Var)),
            )
    }
}

#[derive(Debug)]
pub struct Function {
    code: Vec<Opcode<TemporaryIndex>>,
    variable_declarations: HashMap<Symbol, VariableKind>,
    functions: HashMap<Symbol, Rc<Function>>,
    anon_functions: Vec<Rc<Function>>,
    names: IndexSet<Symbol>,
    name: Symbol,
    scopes: Vec<Scope>,
    strings: IndexSet<Symbol>,
    // TODO: make sure this interacts properly with names/bound names!!!!
    param_list: Vec<ast::FunctionParam>,
}

impl Function {
    pub fn params(&self) -> impl Iterator<Item = &ast::FunctionParam> {
        self.param_list.iter()
    }

    pub fn names(&self) -> &Slice<Symbol> {
        self.names.as_slice()
    }

    pub fn opcodes(&self) -> &[Opcode<usize>] {
        self.code.as_ref()
    }

    pub fn scopes(&self) -> &[Scope] {
        &self.scopes
    }

    pub fn strings(&self) -> &Slice<Symbol> {
        self.strings.as_slice()
    }

    pub fn anon_functions(&self) -> &[Rc<Function>] {
        self.anon_functions.as_ref()
    }
}

impl ScopeAnalysis for Function {
    fn declarations(&self) -> impl Iterator<Item = (Symbol, Declaration, VariableKind)> {
        self.variable_declarations
            .iter()
            .map(|(n, k)| (*n, Declaration::Variable, *k))
            .chain(
                self.functions
                    .iter()
                    .map(|(n, k)| (*n, Declaration::Function(k.clone()), VariableKind::Let)),
            )
    }
}

/// Implements the Syntax Directed Operations described in 8.2:
/// Scope Analysis
pub trait ScopeAnalysis {
    fn declarations(&self) -> impl Iterator<Item = (Symbol, Declaration, VariableKind)>;
    fn bound_names(&self) -> impl Iterator<Item = (Symbol, VariableKind)> {
        self.declarations().map(|(s, d, v)| (s, v))
    }
    fn lexically_declared_names(&self) -> impl Iterator<Item = Symbol> {
        self.bound_names()
            .filter_map(|(n, v)| v.lexical().then_some(n))
    }
    fn lexically_scoped_declarations(&self) -> impl Iterator<Item = (Symbol, Declaration)> {
        self.declarations()
            .filter_map(|(n, k, v)| v.lexical().then_some((n, k)))
    }
    fn var_declared_names(&self) -> impl Iterator<Item = Symbol> {
        self.bound_names().filter_map(|(n, v)| v.var().then_some(n))
    }
    fn var_scoped_declarations(&self) -> impl Iterator<Item = (Symbol, Declaration)> {
        self.declarations()
            .filter_map(|(n, k, v)| v.var().then_some((n, k)))
    }
}

#[derive(Debug)]
pub enum Declaration {
    Variable,
    Function(Rc<Function>),
}

// FIXME: refactor this into a generic 'codegen context'
#[derive(Default)]
pub struct FunctionBuilder {
    code: Vec<Opcode<TemporaryIndex>>,
    anon_functions: Vec<Rc<Function>>,
    functions: HashMap<Symbol, Rc<Function>>,
    /// Holds the next temporary index to use,
    /// incrementing every time we allocate a new one
    tmp_idx: TemporaryIndex,
    bound_names: HashMap<Symbol, VariableKind>,
    names: IndexSet<Symbol>,
    strings: IndexSet<Symbol>,
    scopes: Vec<Scope>,
}

impl FunctionBuilder {
    pub fn codegen_script(script: Node<Program>) -> Script {
        let mut f = Self {
            code: Vec::with_capacity(script.body.len()),
            ..Default::default()
        };
        let Block { statements, scope } = script.take().body;
        for stmt in statements {
            f.codegen_statement(stmt)
        }
        Script {
            code: f.code,
            anon_functions: f.anon_functions,
            functions: f.functions,
            names: f.names,
            variable_declarations: f.bound_names,
            strings: f.strings,
            scopes: f.scopes,
        }
    }

    fn codegen_function(func: Node<ast::Function>) -> Function {
        let mut f = Self {
            code: Vec::with_capacity(func.body.len()),
            ..Default::default()
        };
        let name = func.name.unwrap_or(kw::default);
        // TODO: handle rest
        let ast::Function {
            params,
            body: Block { statements, scope },
            ..
        } = func.take();
        for stmt in statements {
            f.codegen_statement(stmt)
        }
        if !matches!(f.code.last(), Some(Opcode::Return { .. })) {
            // implicit empty return
            f.code.push(Opcode::Return { value: false });
        }
        Function {
            code: f.code,
            anon_functions: f.anon_functions,
            functions: f.functions,
            names: f.names,
            variable_declarations: f.bound_names,
            param_list: params.into_inner(),
            strings: f.strings,
            scopes: f.scopes,
            name,
        }
    }

    fn alloc_temporary(&mut self) -> TemporaryIndex {
        let ret = self.tmp_idx;
        self.tmp_idx += 1;
        ret
    }

    fn codegen_statement(&mut self, stmt: Node<Statement>) {
        match stmt.take().kind {
            StatementKind::Empty => (),
            StatementKind::Debugger => self.code.push(Opcode::Debugger),
            StatementKind::Block(block) => self.codegen_block(block),
            StatementKind::Expression(expr) => self.codegen_expression(expr),
            StatementKind::If(_, _, _, _) => todo!(),
            StatementKind::Labeled(_, _) => todo!(),
            StatementKind::Break(_) => todo!(),
            StatementKind::Continue(_) => todo!(),
            StatementKind::Switch(_, _) => todo!(),
            StatementKind::Return(expr) => {
                if let Some(e) = expr {
                    self.codegen_expression(e);
                    self.code.push(Opcode::Return { value: true })
                } else {
                    self.code.push(Opcode::Return { value: false })
                }
            }
            StatementKind::Throw(_) => todo!(),
            StatementKind::Try(_, _, _) => todo!(),
            StatementKind::While(_, _) => todo!(),
            StatementKind::DoWhile(_, _) => todo!(),
            StatementKind::For {
                init,
                test,
                update,
                header_scope,
                body,
                body_scope,
            } => self.codegen_for_loop(init, test, update, header_scope, *body, body_scope),
            StatementKind::ForIn {
                target,
                iter,
                header_scope,
                body,
                body_scope,
            } => todo!(),
            StatementKind::ForOf {
                target,
                iter,
                header_scope,
                body,
                body_scope,
            } => todo!(),
            StatementKind::VariableDeclaration(decl) => {
                self.codegen_variable_declaration(decl);
            }
            StatementKind::FunctionDeclaration(f) => {
                let f = Self::codegen_function(f);
                // TODO: handle functions at non top level as lexical declarations
                self.functions.insert(f.name, Rc::new(f));
            }
        }
    }

    fn codegen_block(&mut self, block: Block) {
        self.code.push(Opcode::EnterBlock {
            scope: self.scopes.len(),
        });
        self.scopes.push(block.scope);
        for stmt in block.statements {
            self.codegen_statement(stmt);
        }
        self.code.push(Opcode::LeaveBlock);
    }

    /// Evaluate the given expression, storing the result in the accumulator
    fn codegen_expression(&mut self, expr: Node<Expression>) {
        match expr.take().kind {
            ExpressionKind::This => self.code.push(Opcode::LoadThis),
            ExpressionKind::Array(exprs) => {
                self.code.push(Opcode::CreateArray);
                let arr = self.store_temporary();
                for (idx, expr) in exprs.into_iter().enumerate() {
                    self.codegen_expression(expr);
                    self.code.push(Opcode::StoreInArrayIdx { arr, idx })
                }
                self.load_temporary(arr)
            }
            ExpressionKind::Object(props) => {
                self.code.push(Opcode::CreateObject);
                let obj = self.store_temporary();
                for (name, node) in props.into_iter() {
                    if let Some(init) = node {
                        self.codegen_expression(init);
                        let obj = self.copy(obj);
                        self.store_named_property(obj, name);
                    } else {
                        todo!()
                    }
                }
                self.load_temporary(obj)
            }
            ExpressionKind::Function(func) => {
                let f = Self::codegen_function(func);
                let idx = self.anon_functions.len();
                self.anon_functions.push(Rc::new(f));
                self.code.push(Opcode::LoadFunc(idx));
            }
            ExpressionKind::Arrow(_) => todo!(),
            ExpressionKind::Unary(_, _) => todo!(),
            ExpressionKind::Binary(l, op, r) => self.codegen_binary(*l, op, *r),
            ExpressionKind::Assignment(tgt, op, value) => self.codegen_assign(*tgt, op, *value),
            ExpressionKind::Update(_, _, _) => todo!(),
            ExpressionKind::Logical(_, _, _) => todo!(),
            ExpressionKind::Ternary {
                test,
                consequent,
                alternate,
            } => todo!(),
            ExpressionKind::New(_) => todo!(),
            ExpressionKind::Delete(_) => todo!(),
            ExpressionKind::Call(target, args) => {
                let func = self.codegen_expression_to_temporary(*target);
                let args: Vec<_> = args
                    .into_iter()
                    .map(|expr| self.codegen_expression_to_temporary(expr))
                    .collect();
                self.load_temporary(func);
                match args.as_slice() {
                    [] => self.code.push(Opcode::Call0),
                    &[a] => self.code.push(Opcode::Call1(a)),
                    _ => todo!("calling length {}", args.len()),
                }
            }
            ExpressionKind::Member(base, key) => {
                let base = self.codegen_expression_to_temporary(*base);
                match key {
                    MemberKey::Static(name) => {
                        let name = self.add_name(name);
                        self.code.push(Opcode::GetNamedProperty { obj: base, name })
                    }
                    MemberKey::Computed(expr) => {
                        self.codegen_expression(*expr);
                        let expr = self.store_temporary();
                        self.code.push(Opcode::GetComputedProperty {
                            obj: base,
                            index: expr,
                        })
                    }
                }
            }
            ExpressionKind::Literal(lit) => self.codegen_literal(lit),
            ExpressionKind::Literal(_) => todo!(),
            ExpressionKind::Identifier(ident) => {
                let name = self.add_name(ident);
                self.code.push(Opcode::LoadIdent(name))
            }
        }
    }

    /// Evaluate the given expression, storing the result in the accumulator
    /// then moving it to a new temporary. Returns the index of the new temporary used
    fn codegen_expression_to_temporary(&mut self, expr: Node<Expression>) -> TemporaryIndex {
        self.codegen_expression(expr);
        self.store_temporary()
    }

    fn codegen_literal(&mut self, lit: Literal) {
        match lit {
            Literal::Integer(n) => self.code.push(Opcode::LoadInt(n)),
            Literal::String(s) => {
                let idx = self.add_string(s);
                self.code.push(Opcode::LoadString(idx))
            }
        }
    }

    /// Evaluate first the left, then right expression, then perform
    /// the given operation on them, storing the result in the accumulator
    fn codegen_binary(&mut self, l: Node<Expression>, op: BinaryOperator, r: Node<Expression>) {
        let left = self.codegen_expression_to_temporary(l);
        let right = self.codegen_expression_to_temporary(r);
        let op = match op {
            BinaryOperator::Plus => Opcode::Add(left, right),
            BinaryOperator::Lt => Opcode::LessThan(left, right),
            o => todo!("`{o:?}`"),
        };
        self.code.push(op);
    }

    /// Evaluate first the left, then right expression, then assign the right value
    /// to the left reference. If op is anything other than [`AssignmentOperator::Eq`],
    /// perform the assignment according to the operator.
    fn codegen_assign(&mut self, l: Node<Expression>, op: AssignmentOperator, r: Node<Expression>) {
        let l = l.take();
        let op = match l.kind {
            ExpressionKind::Identifier(ident) => {
                let name_idx = self.add_name(ident);
                self.codegen_expression(r);
                Opcode::StoreIdent { name: name_idx }
            }
            ExpressionKind::Member(left, MemberKey::Static(name)) => {
                let left = self.codegen_expression_to_temporary(*left);
                let name_idx = self.add_name(name);
                self.codegen_expression(r);
                Opcode::StoreNamedProperty {
                    obj: left,
                    name: name_idx,
                }
            }
            ExpressionKind::Member(left, MemberKey::Computed(expr)) => {
                let left = self.codegen_expression_to_temporary(*left);
                let prop = self.codegen_expression_to_temporary(*expr);
                self.codegen_expression(r);
                Opcode::StoreComputedProperty {
                    obj: left,
                    index: prop,
                }
            }
            _ => unreachable!(
                "non simple assignment target {l:?} should have been handled during parsing"
            ),
        };
        self.code.push(op);
    }

    fn codegen_variable_declaration(&mut self, decl: Node<VariableDeclaration>) {
        let decl = decl.take();
        for declr in decl.declarations.into_iter() {
            let declr = declr.take();
            let binding_id = declr.name;
            // TODO: anonymous functions
            let name_idx = self.add_name(declr.name);
            self.bound_names.insert(declr.name, decl.kind);
            if let Some(init) = declr.init {
                self.codegen_expression(init);
                if decl.kind.lexical() {
                    self.code.push(Opcode::InitializeIdent { name: name_idx })
                } else {
                    self.code.push(Opcode::StoreIdent { name: name_idx })
                }
            }
        }
    }

    fn codegen_for_loop(
        &mut self,
        init: Option<Node<ForInit>>,
        test: Option<Node<Expression>>,
        update: Option<Node<Expression>>,
        header_scope: Scope,
        body: Node<Statement>,
        body_scope: Scope,
    ) {
        let mut has_lexical_decls = false;
        if let Some(init) = init {
            match init.item() {
                // TODO: this is bad. Maybe bring back `fn consume(self) -> (T, usize, Span)`
                ForInit::VariableDeclaration(_) => {
                    let decl = init.map(|i| {
                        if let ForInit::VariableDeclaration(d) = i {
                            d
                        } else {
                            has_lexical_decls = true;
                            unreachable!()
                        }
                    });
                    if decl.kind.lexical() {
                        todo!("lexical for declarations aren't supported yet")
                    } else {
                        self.codegen_variable_declaration(decl);
                    }
                }
                ForInit::Expression(_) => {
                    let expr = init.map(|i| {
                        if let ForInit::Expression(d) = i {
                            d
                        } else {
                            unreachable!()
                        }
                    });
                    self.codegen_expression(expr);
                }
            }
        }
        if has_lexical_decls {
            self.code.push(Opcode::CreatePerIterationEnvironment);
        }
        let loop_start_idx = self.code.len();
        // Start of the loop
        #[cfg(debug_assertions)]
        self.code.push(Opcode::JumpTarget);

        // Index of the test's jump instruction. Needs to be edited after the loop
        // body is codegenned so we can jump to the right exit point
        let mut test_jump_idx = None;

        if let Some(test) = test {
            self.codegen_expression(test);
            test_jump_idx = Some(self.code.len());
            self.code.push(Opcode::Unreachable);
        }

        self.codegen_statement(body);
        // TODO: handle break, continue

        if let Some(update) = update {
            self.codegen_expression(update);
        }

        self.code.push(Opcode::Jump {
            idx: loop_start_idx,
        });

        if let Some(idx) = test_jump_idx {
            self.code[idx] = Opcode::JumpIfFalse {
                idx: self.code.len(),
            };
        }
        #[cfg(debug_assertions)]
        self.code.push(Opcode::JumpTarget);
    }

    /// Store the contents of the accumulator into the object in [`obj`]
    /// with the property named [`name`]
    fn store_named_property(&mut self, obj: TemporaryIndex, name: Symbol) {
        let idx = self.add_name(name);
        self.code
            .push(Opcode::StoreNamedProperty { obj, name: idx })
    }

    /// Store the current contents of the accumulator into
    /// a new temporary
    fn store_temporary(&mut self) -> TemporaryIndex {
        let target = self.alloc_temporary();
        self.code.push(Opcode::StoreAcc(target));
        target
    }

    /// Load the content of a temporary into the accumulator
    fn load_temporary(&mut self, temp: TemporaryIndex) {
        self.code.push(Opcode::LoadAcc(temp));
    }

    /// Shallow copy a register
    fn copy(&mut self, temp: TemporaryIndex) -> TemporaryIndex {
        let new = self.alloc_temporary();
        self.code.push(Opcode::Move {
            from: temp,
            to: new,
        });
        new
    }

    /// Add a new name to this function's [`Self::names`] and return
    /// the index.
    fn add_name(&mut self, name: Symbol) -> NameIndex {
        self.names.insert_full(name).0
    }

    /// Add a new string to this function's [`Self::strings`] and return
    /// the index.
    fn add_string(&mut self, string: Symbol) -> StringIndex {
        self.strings.insert_full(string).0
    }
}
