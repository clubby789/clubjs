#![allow(unused)]

use std::collections::HashMap;

use indexmap::IndexSet;
use smallvec::SmallVec;

use crate::{
    ast::{
        self, Block, Expression, ExpressionKind, Program, Scope, Statement, StatementKind,
        VariableKind,
    },
    intern::Symbol,
    lex::Literal,
    span::Node,
};

/// [`TemporaryKind`] will either be an incrementing counter or
/// a register/memory reference after optimization`
#[derive(Copy, Clone, PartialEq, Eq, Debug)]
pub enum Opcode<TemporaryKind> {
    /// Trigger the debugger
    Debugger,
    /// Add the nth context of this function to the context stack
    PushContext(usize),
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
    /// at the property held in [`name`]
    StoreComputedProperty {
        obj: TemporaryKind,
        index: TemporaryKind,
    },
    /// Store the value in the accumulator into the given temporary
    StoreAcc(TemporaryKind),
    /// Load the value in the given temporary into acc
    LoadAcc(TemporaryKind),
    /// Load the given integer into acc
    LoadInt(u128),
    /// Call the function in the accumulator with no args
    Call0,
    /// Call the function in the accumulator with the arg in the given temporary
    Call1(TemporaryKind),
    /// Resolve the given name in the environment
    LoadIdent(NameIndex),
}

pub type TemporaryIndex = usize;
pub type NameIndex = usize;

#[derive(Debug)]
pub struct Function {
    code: Vec<Opcode<TemporaryIndex>>,
    scopes: SmallVec<[Scope; 255]>,
    functions: HashMap<Symbol, Function>,
    names: IndexSet<Symbol>,
}

#[derive(Default)]
pub struct FunctionBuilder {
    code: Vec<Opcode<TemporaryIndex>>,
    base_scope: Scope,
    scopes: SmallVec<[Scope; 255]>,
    functions: HashMap<Symbol, Function>,
    /// Holds the next temporary index to use,
    /// incrementing every time we allocate a new one
    tmp_idx: TemporaryIndex,
    names: IndexSet<Symbol>,
}

impl FunctionBuilder {
    pub fn codegen(script: Node<Program>) -> Function {
        let mut f = Self {
            code: Vec::with_capacity(script.body.len()),
            ..Default::default()
        };
        let Block { statements, scope } = script.take().body;
        f.base_scope = scope;
        for stmt in statements {
            f.codegen_statement(stmt)
        }
        Function {
            code: f.code,
            scopes: f.scopes,
            functions: f.functions,
            names: f.names,
        }
    }

    fn codegen_function(func: Node<ast::Function>) -> Function {
        let mut f = Self {
            code: Vec::with_capacity(func.body.len()),
            ..Default::default()
        };
        let Block { statements, scope } = func.take().body;
        f.base_scope = scope;
        for stmt in statements {
            f.codegen_statement(stmt)
        }
        Function {
            code: f.code,
            scopes: f.scopes,
            functions: f.functions,
            names: f.names,
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
                self.code.push(Opcode::PushContext(self.scopes.len()));
                self.scopes.push(block.scope);
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
            StatementKind::FunctionDeclaration(_) => todo!(),
        }
    }

    /// Evaluate the given expression, storing the result in the accumulator
    fn codegen_expression(&mut self, stmt: Node<Expression>) {
        match stmt.take().kind {
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
            ExpressionKind::Binary(_, _, _) => todo!(),
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
                if matches!(target.kind, ExpressionKind::Member(..)) {
                    todo!("member calls")
                }
                self.codegen_expression(*target);
                let func = self.store_temporary();
                let args: Vec<_> = args
                    .into_iter()
                    .map(|expr| {
                        self.codegen_expression(expr);
                        self.store_temporary()
                    })
                    .collect();
                self.load_temporary(func);
                match args.as_slice() {
                    [] => self.code.push(Opcode::Call0),
                    &[a] => self.code.push(Opcode::Call1(a)),
                    _ => todo!("calling length {}", args.len()),
                }
            }
            ExpressionKind::Member(_, _) => todo!(),
            ExpressionKind::Literal(Literal::Integer(int)) => self.code.push(Opcode::LoadInt(int)),
            ExpressionKind::Literal(_) => todo!(),
            ExpressionKind::Identifier(ident) => {
                let name = self.add_name(ident);
                self.code.push(Opcode::LoadIdent(name))
            }
        }
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

    /// Store the contents of the accumulator into the object in [`obj`]
    /// with the property named [`name`]
    fn store_named_property(&mut self, obj: TemporaryIndex, name: Symbol) {
        let idx = self.add_name(name);
        self.code
            .push(Opcode::StoreNamedProperty { obj, name: idx })
    }
}
