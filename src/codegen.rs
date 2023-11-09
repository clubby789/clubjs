use std::{
    collections::HashMap,
    fmt::{Debug, Display, Formatter},
    rc::Rc,
};

use indexmap::{set::Slice, IndexSet};

use crate::{
    ast::{
        self, AssignmentOperator, BinaryOperator, Block, Expression, ExpressionKind, ForInit,
        MemberKey, Program, Scope, Statement, StatementKind, VariableDeclaration, VariableKind,
    },
    intern::Symbol,
    lex::{kw, Literal},
    span::Node,
};

use self::cfg::{ControlFlowGraph, Terminator};

mod cfg;

/// [`TemporaryKind`] will either be an incrementing counter or
/// a register/memory reference after optimization`
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
#[allow(unused)]
pub enum Opcode<TemporaryKind> {
    /// Creates a new lexcial environment for the block
    EnterBlock { scope: ScopeIndex },
    /// Restores the previous LexcialEnvironment in the running context
    LeaveBlock,
    /// Throw an exception. Right now, this just panics
    Throw,
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
    /// Construct the object in the accumulator with no args
    Construct0,
    /// Construct the object in the accumulator with the arg in the given temporary
    Construct1(TemporaryKind),
    /// Resolve the given name in the environment, loading a
    /// [`crate::interpreter::value::JSValue::Reference`] into the accumulator
    LoadIdent(NameIndex),
    /// Perform addition of the two given registers, according to 13.15.3 ApplyStringOrNumericBinaryOperator,
    /// storing the result in the accumulator
    Add(TemporaryKind, TemporaryKind),
    /// Compares the left and right values, according to `7.2.13 IsLessThan`, storing the result
    /// (a boolean or undefined) in the accumulator
    LessThan(TemporaryKind, TemporaryKind),
    /// Compares the left and right values, using `IsLooselyEqual` if [`strict`] is false;
    /// otherwise, `IsStrictlyEqual`
    Eq {
        left: TemporaryKind,
        right: TemporaryKind,
        strict: bool,
    },
    /// Create a new environment as per `14.7.4.4 CreatePerIterationEnvironment`, if we have any
    /// lexical declarations
    // TODO: do bindings
    CreatePerIterationEnvironment,
    /// All jumps must land on a JumpTarget instruction
    JumpTarget,
    /// Set the pc to the given index
    Jump { idx: usize },
    /// If `ToBoolean(GetValue(accumulator))` is false, set the pc to the given index
    JumpIfFalse { idx: usize },

    /// Placeholder opcode used only during codegen. Should never be reached
    Unreachable,
}

impl<TemporaryKind: Display + Debug> Display for Opcode<TemporaryKind> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self, f)
    }
}

pub type TemporaryIndex = usize;
pub type NameIndex = usize;
pub type StringIndex = usize;
pub type AnonFunctionIndex = usize;
pub type ScopeIndex = usize;

#[derive(Debug, Default)]
pub struct Script {
    cfg: ControlFlowGraph<TemporaryIndex>,
    variable_declarations: HashMap<Symbol, VariableKind>,
    anon_functions: Vec<Rc<Function>>,
    functions: HashMap<Symbol, Rc<Function>>,
    names: IndexSet<Symbol>,
    strings: IndexSet<Symbol>,
    scopes: Vec<Scope>,
}

impl Script {
    pub fn cfg(&self) -> &ControlFlowGraph<TemporaryIndex> {
        &self.cfg
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
    cfg: ControlFlowGraph<TemporaryIndex>,
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

    pub fn cfg(&self) -> &ControlFlowGraph<TemporaryIndex> {
        &self.cfg
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
        self.declarations().map(|(s, _, v)| (s, v))
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
    cfg: ControlFlowGraph<TemporaryIndex>,
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
        let mut f = FunctionBuilder::default();
        let Block { statements, .. } = script.take().body;
        // TODO: remove the linear codegen
        f.cfg.new_block();
        for stmt in statements {
            f.codegen_statement(stmt)
        }
        println!("{}", f.cfg);
        Script {
            cfg: f.cfg,
            anon_functions: f.anon_functions,
            functions: f.functions,
            names: f.names,
            variable_declarations: f.bound_names,
            strings: f.strings,
            scopes: f.scopes,
        }
    }

    fn codegen_function(func: Node<ast::Function>) -> Function {
        let mut f = FunctionBuilder::default();
        let name = func.name.unwrap_or(kw::default);
        // TODO: handle rest
        let ast::Function {
            params,
            body: Block { statements, .. },
            ..
        } = func.take();
        f.cfg.new_block();
        for stmt in statements {
            f.codegen_statement(stmt)
        }
        f.cfg
            .current_block()
            .try_set_terminator(Terminator::Return { value: false })
            .ok();
        Function {
            cfg: f.cfg,
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
            StatementKind::Debugger => {
                self.cfg.add_op(Opcode::Debugger);
            }
            StatementKind::Block(block) => self.codegen_block(block),
            StatementKind::Expression(expr) => self.codegen_expression(expr),
            StatementKind::If(test, then, els, _) => self.codegen_if(test, *then, els.map(|b| *b)),
            StatementKind::Labeled(_, _) => todo!(),
            StatementKind::Break(_) => todo!(),
            StatementKind::Continue(_) => todo!(),
            StatementKind::Switch(_, _) => todo!(),
            StatementKind::Return(expr) => {
                if let Some(e) = expr {
                    self.codegen_expression(e);
                    self.cfg
                        .current_block()
                        .set_terminator(cfg::Terminator::Return { value: true });
                } else {
                    self.cfg
                        .current_block()
                        .set_terminator(cfg::Terminator::Return { value: false });
                }
            }
            StatementKind::Throw(expr) => {
                self.codegen_expression(expr);
                todo!("basic block terminator for Throw")
            }
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
            StatementKind::ForIn { .. } => todo!(),
            StatementKind::ForOf { .. } => todo!(),
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
        let op = Opcode::EnterBlock {
            scope: self.scopes.len(),
        };
        self.cfg.add_op(op);
        self.scopes.push(block.scope);
        for stmt in block.statements {
            self.codegen_statement(stmt);
        }
        self.cfg.add_op(Opcode::LeaveBlock);
    }

    /// Evaluate the given expression, storing the result in the accumulator
    fn codegen_expression(&mut self, expr: Node<Expression>) {
        match expr.take().kind {
            ExpressionKind::This => {
                self.cfg.add_op(Opcode::LoadThis);
            }
            ExpressionKind::Array(exprs) => {
                self.cfg.add_op(Opcode::CreateArray);
                let arr = self.store_temporary();
                for (idx, expr) in exprs.into_iter().enumerate() {
                    self.codegen_expression(expr);
                    let op = Opcode::StoreInArrayIdx { arr, idx };
                    self.cfg.add_op(op);
                }
                self.load_temporary(arr)
            }
            ExpressionKind::Object(props) => {
                self.cfg.add_op(Opcode::CreateObject);
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
                let op = Opcode::LoadFunc(idx);
                self.cfg.add_op(op);
            }
            ExpressionKind::Arrow(_) => todo!(),
            ExpressionKind::Unary(_, _) => todo!(),
            ExpressionKind::Binary(l, op, r) => self.codegen_binary(*l, op, *r),
            ExpressionKind::Assignment(tgt, op, value) => self.codegen_assign(*tgt, op, *value),
            ExpressionKind::Update(_, _, _) => todo!(),
            ExpressionKind::Logical(_, _, _) => todo!(),
            ExpressionKind::Ternary { .. } => todo!(),
            ExpressionKind::New(target, args) => {
                let func = self.codegen_expression_to_temporary(*target);
                let args: Vec<_> = args
                    .into_iter()
                    .map(|expr| self.codegen_expression_to_temporary(expr))
                    .collect();
                self.load_temporary(func);
                let op = match args.as_slice() {
                    [] => Opcode::Construct0,
                    &[a] => Opcode::Construct1(a),
                    _ => todo!("construcing length {}", args.len()),
                };
                self.cfg.add_op(op);
            }
            ExpressionKind::Delete(_) => todo!(),
            ExpressionKind::Call(target, args) => {
                let func = self.codegen_expression_to_temporary(*target);
                let args: Vec<_> = args
                    .into_iter()
                    .map(|expr| self.codegen_expression_to_temporary(expr))
                    .collect();
                self.load_temporary(func);
                let op = match args.as_slice() {
                    [] => Opcode::Call0,
                    &[a] => Opcode::Call1(a),
                    _ => todo!("calling length {}", args.len()),
                };
                self.cfg.add_op(op);
            }
            ExpressionKind::Member(base, key) => {
                let base = self.codegen_expression_to_temporary(*base);
                match key {
                    MemberKey::Static(name) => {
                        let name = self.add_name(name);
                        let op = Opcode::GetNamedProperty { obj: base, name };
                        self.cfg.add_op(op);
                    }
                    MemberKey::Computed(expr) => {
                        self.codegen_expression(*expr);
                        let expr = self.store_temporary();
                        let op = Opcode::GetComputedProperty {
                            obj: base,
                            index: expr,
                        };
                        self.cfg.add_op(op);
                    }
                }
            }
            ExpressionKind::Literal(lit) => self.codegen_literal(lit),
            ExpressionKind::Identifier(ident) => {
                let name = self.add_name(ident);
                let op = Opcode::LoadIdent(name);
                self.cfg.add_op(op);
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
        let op = match lit {
            Literal::Integer(n) => Opcode::LoadInt(n),
            Literal::String(s) => {
                let idx = self.add_string(s);
                Opcode::LoadString(idx)
            }
        };
        self.cfg.add_op(op);
    }

    /// Evaluate first the left, then right expression, then perform
    /// the given operation on them, storing the result in the accumulator
    fn codegen_binary(&mut self, l: Node<Expression>, op: BinaryOperator, r: Node<Expression>) {
        let left = self.codegen_expression_to_temporary(l);
        let right = self.codegen_expression_to_temporary(r);
        let op = match op {
            BinaryOperator::Plus => Opcode::Add(left, right),
            BinaryOperator::Lt => Opcode::LessThan(left, right),
            BinaryOperator::EqEq => Opcode::Eq {
                left,
                right,
                strict: false,
            },
            BinaryOperator::EqEqEq => Opcode::Eq {
                left,
                right,
                strict: true,
            },
            o => todo!("`{o:?}`"),
        };
        self.cfg.add_op(op);
    }

    /// Evaluate first the left, then right expression, then assign the right value
    /// to the left reference. If op is anything other than [`AssignmentOperator::Eq`],
    /// perform the assignment according to the operator.
    fn codegen_assign(&mut self, l: Node<Expression>, op: AssignmentOperator, r: Node<Expression>) {
        let l = l.take();
        assert_eq!(op, AssignmentOperator::Eq);
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
        self.cfg.add_op(op);
    }

    fn codegen_variable_declaration(&mut self, decl: Node<VariableDeclaration>) {
        let decl = decl.take();
        for declr in decl.declarations.into_iter() {
            let declr = declr.take();
            let binding_id = declr.name;
            // TODO: anonymous functions
            let name_idx = self.add_name(binding_id);
            self.bound_names.insert(binding_id, decl.kind);
            if let Some(init) = declr.init {
                self.codegen_expression(init);
                let op = if decl.kind.lexical() {
                    Opcode::InitializeIdent { name: name_idx }
                } else {
                    Opcode::StoreIdent { name: name_idx }
                };
                self.cfg.add_op(op);
            }
        }
    }

    fn codegen_if(
        &mut self,
        test: Node<Expression>,
        then: Node<Statement>,
        els: Option<Node<Statement>>,
    ) {
        let mut successors: Vec<(Option<Node<Expression>>, Node<Statement>)> = vec![];
        let mut cur = els;
        let mut has_unqualified_else = false;
        while let Some(next) = cur {
            if let StatementKind::If(..) = &next.kind {
                // This is pretty bad, is there a better way to 'lazily move'?
                let StatementKind::If(test, then, els, ..) = next.take().kind else {
                    unreachable!()
                };
                successors.push((Some(test), *then));
                cur = els.map(|b| *b);
            } else {
                if has_unqualified_else {
                    panic!("multiple `else` blocks in a single if");
                }
                has_unqualified_else = true;
                successors.push((None, next));
                cur = None;
            }
        }

        self.codegen_expression(test);
        // Blocks beginning with an 'if' test
        // Record these to later jump to the corresponding 'then', or after it
        let mut test_blocks = vec![self.cfg.current_idx()];
        // Blocks to be taken if 'test' is true
        // Record these to later have them jump out of the whole if statement
        let mut then_blocks = vec![self.cfg.new_block()];
        self.codegen_statement(then);

        for (test, body) in successors {
            self.cfg.new_block();
            if let Some(test) = test {
                test_blocks.push(self.cfg.current_idx());
                self.codegen_expression(test);
                then_blocks.push(self.cfg.new_block());
            }
            self.codegen_statement(body);
        }
        let last_block = self.cfg.current_idx();
        assert_eq!(test_blocks.len(), then_blocks.len());
        let end = self.cfg.new_block();
        for i in 0..test_blocks.len() {
            let no_block = if let Some(b) = test_blocks.get(i + 1) {
                *b
            } else if has_unqualified_else {
                last_block
            } else {
                end
            };
            self.cfg
                .get_block(test_blocks[i])
                .set_terminator(Terminator::Conditional {
                    yes: then_blocks[i],
                    no: no_block,
                });
            self.cfg
                .get_block(then_blocks[i])
                .set_terminator(Terminator::Unconditional(end));
        }
        if has_unqualified_else {
            self.cfg
                .get_block(last_block)
                .set_terminator(Terminator::Unconditional(end));
        }
    }

    fn codegen_for_loop(
        &mut self,
        init: Option<Node<ForInit>>,
        test: Option<Node<Expression>>,
        update: Option<Node<Expression>>,
        _header_scope: Scope,
        body: Node<Statement>,
        _body_scope: Scope,
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
            self.cfg.add_op(Opcode::CreatePerIterationEnvironment);
        }
        let test_block = self.cfg.new_successor_block();

        if let Some(test) = test {
            self.codegen_expression(test);
        }

        let loop_body = self.cfg.new_block();
        self.codegen_statement(body);
        // TODO: handle break, continue

        if let Some(update) = update {
            self.codegen_expression(update);
        }

        self.cfg
            .current_block()
            .set_terminator(Terminator::Unconditional(test_block));
        let after = self.cfg.new_block();
        self.cfg
            .get_block(test_block)
            .set_terminator(Terminator::Conditional {
                yes: loop_body,
                no: after,
            });
    }

    /// Store the contents of the accumulator into the object in [`obj`]
    /// with the property named [`name`]
    fn store_named_property(&mut self, obj: TemporaryIndex, name: Symbol) {
        let idx = self.add_name(name);
        let op = Opcode::StoreNamedProperty { obj, name: idx };
        self.cfg.add_op(op);
    }

    /// Store the current contents of the accumulator into
    /// a new temporary
    fn store_temporary(&mut self) -> TemporaryIndex {
        let target = self.alloc_temporary();
        let op = Opcode::StoreAcc(target);
        self.cfg.add_op(op);
        target
    }

    /// Load the content of a temporary into the accumulator
    fn load_temporary(&mut self, temp: TemporaryIndex) {
        let op = Opcode::LoadAcc(temp);
        self.cfg.add_op(op);
    }

    /// Shallow copy a register
    fn copy(&mut self, temp: TemporaryIndex) -> TemporaryIndex {
        let new = self.alloc_temporary();
        let op = Opcode::Move {
            from: temp,
            to: new,
        };
        self.cfg.add_op(op);
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
