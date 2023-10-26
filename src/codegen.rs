#![allow(unused)]

use std::{
    collections::{HashMap, HashSet},
    rc::Rc,
};

use indexmap::{set::Slice, IndexSet};
use smallvec::SmallVec;

use crate::{
    ast::{
        self, BinaryOperator, Block, Expression, ExpressionKind, MemberKey, Program, Scope,
        Statement, StatementKind, VariableKind,
    },
    intern::Symbol,
    lex::{kw, Literal},
    span::Node,
};

/// [`TemporaryKind`] will either be an incrementing counter or
/// a register/memory reference after optimization`
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Opcode<TemporaryKind> {
    /// Trigger the debugger
    Debugger,
    /// Push a new context to the stack
    PushContext,
    /// Remove the current context from the stack
    PopContext,
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
    /// at the property name held in [`name`]
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
    /// Store the value in the accumulator into the given temporary
    StoreAcc(TemporaryKind),
    /// Load the value in the given temporary into acc
    LoadAcc(TemporaryKind),
    /// Load the given integer into acc
    LoadInt(u128),
    /// Load the string at the nth index of this script/function's string table
    LoadString(StringIndex),
    /// Call the function in the accumulator with no args
    Call0,
    /// Call the function in the accumulator with the arg in the given temporary
    Call1(TemporaryKind),
    /// Resolve the given name in the environment, loading a
    /// [`crate::interpreter::value::JSValueKind::Reference`] into the accumulator
    LoadIdent(NameIndex),
    /// Perform addition of the two given registers, according to 13.15.3 ApplyStringOrNumericBinaryOperator,
    /// storing the result in the accumulator
    Add(TemporaryKind, TemporaryKind),
}

pub type TemporaryIndex = usize;
pub type NameIndex = usize;
pub type StringIndex = usize;

#[derive(Debug, Default)]
pub struct Script {
    code: Vec<Opcode<TemporaryIndex>>,
    variable_declarations: HashMap<Symbol, VariableKind>,
    functions: HashMap<Symbol, Rc<Function>>,
    names: IndexSet<Symbol>,
    strings: IndexSet<Symbol>,
}

impl Script {
    pub fn opcodes(&self) -> &[Opcode<usize>] {
        self.code.as_ref()
    }

    pub fn names(&self) -> &Slice<Symbol> {
        self.names.as_slice()
    }

    pub fn strings(&self) -> &Slice<Symbol> {
        self.strings.as_slice()
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
    names: IndexSet<Symbol>,
    name: Symbol,

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

    pub fn strings(&self) -> &Slice<Symbol> {
        self.strings.as_slice()
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
    functions: HashMap<Symbol, Rc<Function>>,
    /// Holds the next temporary index to use,
    /// incrementing every time we allocate a new one
    tmp_idx: TemporaryIndex,
    bound_names: HashMap<Symbol, VariableKind>,
    names: IndexSet<Symbol>,
    strings: IndexSet<Symbol>,
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
            functions: f.functions,
            names: f.names,
            variable_declarations: f.bound_names,
            strings: f.strings,
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
        Function {
            code: f.code,
            functions: f.functions,
            names: f.names,
            variable_declarations: f.bound_names,
            param_list: params.into_inner(),
            strings: f.strings,
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
            StatementKind::Block(block) => {
                self.code.push(Opcode::PushContext);
                for stmt in block.statements {
                    self.codegen_statement(stmt);
                }
                self.code.push(Opcode::PopContext);
            }
            StatementKind::Expression(expr) => self.codegen_expression(expr),
            StatementKind::If(_, _, _, _) => todo!(),
            StatementKind::Labeled(_, _) => todo!(),
            StatementKind::Break(_) => todo!(),
            StatementKind::Continue(_) => todo!(),
            StatementKind::Switch(_, _) => todo!(),
            StatementKind::Return(_) => todo!(),
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
            } => todo!(),
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
            StatementKind::VariableDeclaration(_) => todo!(),
            StatementKind::FunctionDeclaration(f) => {
                let f = Self::codegen_function(f);
                // TODO: handle functions at non top level as lexical declarations
                self.functions.insert(f.name, Rc::new(f));
            }
        }
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
                        self.store_named_property(obj, name);
                    } else {
                        todo!()
                    }
                }
                self.load_temporary(obj)
            }
            ExpressionKind::Function(_) => todo!(),
            ExpressionKind::Arrow(_) => todo!(),
            ExpressionKind::Unary(_, _) => todo!(),
            ExpressionKind::Binary(l, op, r) => self.codegen_binary(*l, op, *r),
            ExpressionKind::Assignment(_, _, _) => todo!(),
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
            o => todo!("`{o:?}`"),
        };
        self.code.push(op);
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
