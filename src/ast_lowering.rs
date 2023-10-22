#![allow(unused)]

use crate::{
    ast::{
        AssignmentOperator, BinaryOperator, ForTarget, LogicalOperator, UnaryOperator,
        UpdateOperator, VariableKind,
    },
    intern::Symbol,
    span::Span,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program<'ir> {
    span: Span,
    body: &'ir [Statement<'ir>],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function<'ir> {
    span: Span,
    // May be None for function expressions
    name: Option<Symbol>,
    params: &'ir [FunctionParam<'ir>],
    rest: Option<Symbol>,
    body: &'ir [Statement<'ir>],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionParam<'ir> {
    Normal(Symbol),
    Defaulted(Symbol, &'ir Expression<'ir>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclaration<'ir> {
    span: Span,
    declarations: &'ir [VariableDeclarator<'ir>],
    kind: VariableKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclarator<'ir> {
    span: Span,
    name: Symbol,
    init: Option<&'ir Expression<'ir>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement<'ir> {
    span: Span,
    kind: StatementKind<'ir>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind<'ir> {
    Empty,
    Debugger,
    Block(&'ir [Statement<'ir>]),
    Expression(&'ir Expression<'ir>),
    If(
        &'ir Expression<'ir>,
        &'ir Statement<'ir>,
        Option<&'ir Statement<'ir>>,
    ),
    Labeled(Symbol, &'ir Statement<'ir>),
    Break(Option<Symbol>),
    Continue(Option<Symbol>),
    Switch(&'ir Expression<'ir>, &'ir [SwitchCase<'ir>]),
    Return(Option<&'ir Expression<'ir>>),
    Throw(&'ir Expression<'ir>),
    Try(
        &'ir [Statement<'ir>],
        Option<&'ir CatchClause<'ir>>,
        Option<&'ir [Statement<'ir>]>,
    ),
    While(&'ir Expression<'ir>, &'ir [Statement<'ir>]),
    DoWhile(&'ir Statement<'ir>, &'ir Expression<'ir>),
    For(
        Option<&'ir ForInit<'ir>>,
        Option<&'ir Expression<'ir>>,
        Option<&'ir Expression<'ir>>,
        &'ir Statement<'ir>,
    ),
    ForIn(ForTarget, &'ir Expression<'ir>, &'ir Statement<'ir>),
    ForOf(ForTarget, &'ir Expression<'ir>, &'ir Statement<'ir>),
    VariableDeclaration(&'ir VariableDeclaration<'ir>),
    FunctionDeclaration(&'ir Function<'ir>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression<'ir> {
    span: Span,
    kind: ExpressionKind<'ir>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind<'ir> {
    This,
    Array(&'ir [Expression<'ir>]),
    Object(&'ir [(Symbol, Option<&'ir Expression<'ir>>)]),
    Function(&'ir Function<'ir>),
    Arrow(&'ir Function<'ir>),
    Unary(UnaryOperator, &'ir Expression<'ir>),
    Binary(&'ir Expression<'ir>, BinaryOperator, &'ir Expression<'ir>),
    Assignment(Symbol, AssignmentOperator, &'ir Expression<'ir>),
    /// the bool is true if the operator is a prefix
    Update(&'ir Expression<'ir>, UpdateOperator, bool),
    Logical(&'ir Expression<'ir>, LogicalOperator, &'ir Expression<'ir>),
    Ternary {
        test: &'ir Expression<'ir>,
        consequent: &'ir Expression<'ir>,
        alternate: &'ir Expression<'ir>,
    },
    New(&'ir Expression<'ir>),
    Call(&'ir Expression<'ir>, &'ir [Expression<'ir>]),
    Member(&'ir Expression<'ir>, MemberKey<'ir>),
    // TODO: Support yield and generators
    // Yield(Option<&'ir Expression<'ir>>),
    Literal(crate::lex::Literal),
    Identifier(Symbol),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemberKey<'ir> {
    Static(Symbol),
    Computed(&'ir Expression<'ir>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SwitchCase<'ir> {
    span: Span,
    test: Option<&'ir Expression<'ir>>,
    consequent: Statement<'ir>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CatchClause<'ir> {
    span: Span,
    param: Option<Symbol>,
    block: &'ir [Statement<'ir>],
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForInit<'ir> {
    VariableDeclaration(&'ir VariableDeclaration<'ir>),
    Expression(&'ir Expression<'ir>),
}
