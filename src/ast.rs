use std::{
    cell::Cell,
    collections::{hash_map::Entry, HashMap, HashSet},
    fmt::Debug,
    path::PathBuf,
};

use crate::{
    intern::Symbol,
    lex::{kw, Lexer, Token, TokenKind},
    session::{report_fatal_error, Session},
    span::{Node, SourceMap, Span},
};

/// Helper trait to get the names bound within an AST node
trait Names {
    fn bound_names(&self) -> impl Iterator<Item = Symbol> {
        self.lexically_declared_names()
            .chain(self.var_declared_names())
    }
    fn lexically_declared_names(&self) -> impl Iterator<Item = Symbol>;
    fn var_declared_names(&self) -> impl Iterator<Item = Symbol>;
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    // May be None for function expressions
    pub name: Option<Symbol>,
    pub params: ParamList,
    pub rest: Option<Symbol>,
    pub body: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ParamList(Vec<FunctionParam>);

impl Names for ParamList {
    fn lexically_declared_names(&self) -> impl Iterator<Item = Symbol> {
        std::iter::empty()
    }

    fn var_declared_names(&self) -> impl Iterator<Item = Symbol> {
        std::iter::empty()
    }

    fn bound_names(&self) -> impl Iterator<Item = Symbol> {
        self.0.iter().map(FunctionParam::name)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionParam {
    Normal(Symbol),
    Defaulted(Symbol, Node<Expression>),
}

impl FunctionParam {
    pub fn name(&self) -> Symbol {
        match self {
            FunctionParam::Normal(s) | FunctionParam::Defaulted(s, _) => *s,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclaration {
    pub declarations: Vec<Node<VariableDeclarator>>,
    pub kind: VariableKind,
}

impl Names for VariableDeclaration {
    fn lexically_declared_names(&self) -> impl Iterator<Item = Symbol> {
        self.kind
            .lexical()
            .then_some(self.declarations.iter())
            .unwrap_or([].iter())
            .map(|v| v.name)
    }

    fn var_declared_names(&self) -> impl Iterator<Item = Symbol> {
        self.kind
            .var()
            .then_some(self.declarations.iter())
            .unwrap_or([].iter())
            .map(|v| v.name)
    }

    fn bound_names(&self) -> impl Iterator<Item = Symbol> {
        self.declarations.iter().map(|v| v.name)
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum VariableKind {
    Var,
    Let,
    Const,
}

impl VariableKind {
    pub fn redeclarable(self) -> bool {
        matches!(self, Self::Var)
    }

    pub fn var(self) -> bool {
        matches!(self, Self::Var)
    }

    pub fn lexical(self) -> bool {
        matches!(self, Self::Const | Self::Let)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclarator {
    pub name: Symbol,
    pub init: Option<Node<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement {
    pub kind: StatementKind,
}

impl Statement {
    pub fn is_labelled_function(&self) -> bool {
        if let StatementKind::Labeled(_, inner) = &self.kind {
            if matches!(inner.kind, StatementKind::FunctionDeclaration(..)) {
                return true;
            }
        }
        false
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind {
    Empty,
    Debugger,
    Block(Block),
    Expression(Node<Expression>),
    If(
        Node<Expression>,
        Box<Node<Statement>>,
        Option<Box<Node<Statement>>>,
        Scope,
    ),
    Labeled(Symbol, Box<Node<Statement>>),
    Break(Option<Symbol>),
    Continue(Option<Symbol>),
    Switch(Node<Expression>, Vec<Node<SwitchCase>>),
    Return(Option<Node<Expression>>),
    Throw(Node<Expression>),
    Try(Block, Option<Node<CatchClause>>, Option<Block>),
    While(Node<Expression>, Box<Node<Statement>>),
    DoWhile(Box<Node<Statement>>, Node<Expression>),
    For {
        init: Option<Node<ForInit>>,
        test: Option<Node<Expression>>,
        update: Option<Node<Expression>>,
        header_scope: Scope,
        body: Box<Node<Statement>>,
        body_scope: Scope,
    },
    ForIn {
        target: ForTarget,
        iter: Node<Expression>,
        header_scope: Scope,
        body: Box<Node<Statement>>,
        body_scope: Scope,
    },
    ForOf {
        target: ForTarget,
        iter: Node<Expression>,
        header_scope: Scope,
        body: Box<Node<Statement>>,
        body_scope: Scope,
    },
    VariableDeclaration(Node<VariableDeclaration>),
    FunctionDeclaration(Node<Function>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Block {
    pub statements: Vec<Node<Statement>>,
    pub scope: Scope,
}

impl Block {
    pub fn len(&self) -> usize {
        self.statements.len()
    }
}

impl IntoIterator for Block {
    type Item = Node<Statement>;

    type IntoIter = std::vec::IntoIter<Self::Item>;

    fn into_iter(self) -> Self::IntoIter {
        self.statements.into_iter()
    }
}

impl Names for Block {
    fn lexically_declared_names(&self) -> impl Iterator<Item = Symbol> {
        self.statements.lexically_declared_names()
    }

    fn var_declared_names(&self) -> impl Iterator<Item = Symbol> {
        self.statements.var_declared_names()
    }
}

impl Names for Vec<Node<Statement>> {
    fn lexically_declared_names(&self) -> impl Iterator<Item = Symbol> {
        self.iter()
            .filter_map(|stmt| {
                if let StatementKind::VariableDeclaration(decl) = &stmt.kind {
                    Some(decl.lexically_declared_names())
                } else {
                    None
                }
            })
            .flatten()
    }

    fn var_declared_names(&self) -> impl Iterator<Item = Symbol> {
        self.iter()
            .filter_map(|stmt| {
                if let StatementKind::VariableDeclaration(decl) = &stmt.kind {
                    Some(decl.var_declared_names())
                } else {
                    None
                }
            })
            .flatten()
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct Expression {
    pub kind: ExpressionKind,
}

impl Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Debug::fmt(&self.kind, f)
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    This,
    Array(Vec<Node<Expression>>),
    Object(Vec<(Symbol, Option<Node<Expression>>)>),
    Function(Node<Function>),
    Arrow(Node<Function>),
    Unary(UnaryOperator, Box<Node<Expression>>),
    Binary(Box<Node<Expression>>, BinaryOperator, Box<Node<Expression>>),
    Assignment(Symbol, AssignmentOperator, Box<Node<Expression>>),
    /// the bool is true if the operator is a prefix
    Update(Box<Node<Expression>>, UpdateOperator, bool),
    Logical(
        Box<Node<Expression>>,
        LogicalOperator,
        Box<Node<Expression>>,
    ),
    Ternary {
        test: Box<Node<Expression>>,
        consequent: Box<Node<Expression>>,
        alternate: Box<Node<Expression>>,
    },
    New(Box<Node<Expression>>),
    Delete(Box<Node<Expression>>),
    Call(Box<Node<Expression>>, Vec<Node<Expression>>),
    Member(Box<Node<Expression>>, MemberKey),
    // TODO: Support yield and generators
    // Yield(Option<Box<Spanned<Expression>>>),
    Literal(crate::lex::Literal),
    Identifier(Symbol),
}

impl ExpressionKind {
    pub fn is_simple_assignment_target(&self) -> bool {
        matches!(
            self,
            ExpressionKind::Identifier(..) | ExpressionKind::Call(..) | ExpressionKind::Member(..)
        )
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemberKey {
    Static(Symbol),
    Computed(Box<Node<Expression>>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    LogicalNot,
    BitwiseNot,
    TypeOf,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BinaryOperator {
    EqEq,
    NotEq,
    EqEqEq,
    NotEqEq,
    Lt,
    LtE,
    Gt,
    GtE,
    LShift,
    RShift,
    GtGtGt,
    Plus,
    Minus,
    Multiply,
    Divide,
    Modulo,
    Or,
    Xor,
    And,
    In,
    InstanceOf,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UpdateOperator {
    PlusPlus,
    MinusMinus,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum AssignmentOperator {
    Eq,
    PlusEq,
    MinusEq,
    MulEq,
    DivEq,
    ModEq,
    LShiftEq,
    RShiftEq,
    GtGtGtEq,
    OrEq,
    XorEq,
    AndEq,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum LogicalOperator {
    Or,
    And,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct SwitchCase {
    pub test: Option<Node<Expression>>,
    pub consequent: Node<Statement>,
    // FIXME: switch case scopes
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CatchClause {
    pub param: Option<Symbol>,
    pub block: Block,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForInit {
    VariableDeclaration(VariableDeclaration),
    Expression(Expression),
}

/// The target of a for/each or for/in loop
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForTarget {
    Declaration(VariableKind, Symbol),
    Variable(Symbol),
}

#[derive(Clone)]
pub struct Parser<'a> {
    lexer: Lexer<'a>,
    prev_token: Token,
    token: Token,
    id_counter: Cell<usize>,
    scopes: Vec<Scope>,
}

#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct Scope(HashMap<Symbol, VariableKind>);

impl Scope {
    pub fn new() -> Self {
        Self(HashMap::new())
    }

    /// Add a given name and variable kind to the scope if it doesn't exist,
    /// OR if it exists but is reassignable by rhe new variable. Returns `true`
    /// if a new entry was created.
    pub fn insert(&mut self, value: Symbol, kind: VariableKind) -> bool {
        match self.0.entry(value) {
            Entry::Occupied(mut o) if o.get().redeclarable() && kind.redeclarable() => {
                o.insert(kind);
                true
            }
            Entry::Vacant(e) => {
                e.insert(kind);
                true
            }
            _ => false,
        }
    }

    /// Add all variables declared in a given declaration to the scope.
    /// Will throw an error if redeclaring a variable that is not allowed.
    pub fn add_declaration(&mut self, decl: &VariableDeclaration) {
        for v in &decl.declarations {
            if !self.insert(v.name, decl.kind) {
                report_fatal_error(format!("redeclaration of {}", v.name.as_str()), v.span());
            }
        }
    }
}

/// Run parsing code with a new scope, returning the parsed value
/// and the (maybe) populated scope.
/// This is a macro to avoid borrow checker errors with nested closures
/// borrowing self mutably.
macro_rules! parse_with_scope {
    ($self:ident : $parse:expr) => {{
        $self.scopes.push(Scope::new());
        let value = $parse;
        (value, $self.scopes.pop().unwrap())
    }};
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str, path: PathBuf) -> Self {
        let mut lexer = Lexer::new(source);
        crate::SESSION
            .set(Session::new(SourceMap::from_src(source.to_string(), path)))
            .unwrap();
        let token = lexer.next_token();
        Self {
            lexer,
            prev_token: Token::default(),
            token,
            id_counter: Cell::new(0),
            scopes: vec![Scope::new()],
        }
    }

    pub fn src(&self) -> &str {
        self.lexer.src()
    }

    fn alloc_node<T>(&self, value: T, span: Span) -> Node<T> {
        let id = self.id_counter.get();
        self.id_counter.set(id.checked_add(1).unwrap());
        Node::new(id, value, span)
    }

    pub fn parse(mut self) -> Node<Program> {
        let span = Span::new(0, self.src().len());
        let block = self.parse_block();
        self.alloc_node(Program { body: block }, span)
    }

    /// Advance to the next token, replacing `prev_token` with token
    /// Returns the old value of `prev_token`
    fn advance(&mut self) -> Token {
        let tmp = std::mem::replace(&mut self.token, self.lexer.next_token());
        std::mem::replace(&mut self.prev_token, tmp)
    }

    /// Advances to the next token if the current token is 'kind'
    fn eat(&mut self, kind: TokenKind) -> bool {
        if self.token.kind() == kind {
            self.advance();
            return true;
        }
        false
    }

    /// Advances several tokens if the next tokens match 'kinds'
    /// If all were consumed, returns each `prev_token` value
    fn eat_many(&mut self, kinds: &[TokenKind]) -> Option<Vec<Token>> {
        let mut tmp = self.clone();
        let mut v = vec![];
        for &tok in kinds {
            if tmp.token.kind() == tok {
                v.push(tmp.advance());
            } else {
                return None;
            }
        }
        *self = tmp;
        Some(v)
    }

    /// Consumes a token of 'kind', panicking if this doesn't match
    #[track_caller]
    fn expect(&mut self, kind: TokenKind) {
        if !self.eat(kind) {
            report_fatal_error(
                format!("expected `{:?}` but found `{:?}`", kind, self.token.kind()),
                self.token.span(),
            );
        }
    }

    fn intern(&self, token: Token) -> Symbol {
        Symbol::intern(token.source_string(self.src()))
    }

    fn eat_ident(&mut self) -> Option<Symbol> {
        self.eat(TokenKind::Ident)
            .then(|| self.intern(self.prev_token))
    }

    fn eat_symbol(&mut self, sym: Symbol) -> bool {
        if matches!(self.token.kind(), TokenKind::Ident) && self.intern(self.token) == sym {
            self.advance();
            true
        } else {
            false
        }
    }

    fn expect_symbol(&mut self, sym: Symbol) {
        if !self.eat_symbol(sym) {
            report_fatal_error(format!("expected `{}`", sym.as_str()), self.token.span());
        }
    }

    fn peek_ident(&mut self) -> Option<Symbol> {
        matches!(self.token.kind(), TokenKind::Ident).then(|| self.intern(self.token))
    }

    #[track_caller]
    fn expect_ident(&mut self) -> Symbol {
        self.eat_ident()
            .unwrap_or_else(|| report_fatal_error("expected an identifier", self.prev_token.span()))
    }

    #[track_caller]
    fn expect_ident_no_kw(&mut self) -> Symbol {
        let sym = self.expect_ident();
        if sym.is_keyword() {
            report_fatal_error(
                "expected an identifier, found keyword",
                self.prev_token.span(),
            )
        }
        sym
    }

    fn parse_delimited_list<T>(
        &mut self,
        delim: TokenKind,
        f: fn(&mut Self) -> Option<T>,
    ) -> Vec<T> {
        let mut content = vec![];
        while let Some(v) = f(self) {
            content.push(v);
            if !self.eat(delim) {
                break;
            }
        }
        content
    }

    /// Returns a copy of the current parser advanced to the end of the current delimited
    /// region. Should already be inside the region
    fn lookahead_delim(&self, open: TokenKind, close: TokenKind) -> Self {
        debug_assert_eq!(self.prev_token.kind(), open);
        let mut lookahead = self.clone();
        let mut depth = 1;
        while depth > 0 {
            if lookahead.token.kind() == open {
                depth += 1;
            } else if lookahead.token.kind() == close {
                depth -= 1;
            }
            lookahead.advance();
        }
        lookahead
    }

    /// Attempts to parse a statement
    fn parse_statement(&mut self) -> Option<Node<Statement>> {
        if matches!(self.token.kind(), TokenKind::Eof | TokenKind::RBrace) {
            return None;
        }
        let span = self.token.span().shrink_to_lo();
        let statement = if self.eat(TokenKind::Semicolon) {
            Statement {
                kind: StatementKind::Empty,
            }
        } else if self.eat_symbol(kw::Debugger) {
            self.eat(TokenKind::Semicolon);
            Statement {
                kind: StatementKind::Debugger,
            }
        } else if self.eat(TokenKind::LBrace) {
            let block = self.parse_block();
            self.expect(TokenKind::RBrace);
            Statement {
                kind: StatementKind::Block(block),
            }
        } else if self.eat_symbol(kw::If) {
            self.expect(TokenKind::LParen);
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            let (content, scope) = parse_with_scope!(self:
                self
                    .parse_statement_or_expr()
                    .unwrap_or_else(|| report_fatal_error("if statements must have a block", self.token.span()))
            );
            if content.is_labelled_function() {
                report_fatal_error(
                    "functions as the body of an if statement may not be labelled",
                    content.span(),
                )
            }
            let alternate = self
                .eat_symbol(kw::Else)
                .then(|| self.parse_statement_or_expr())
                .flatten();
            Statement {
                kind: StatementKind::If(expr, Box::new(content), alternate.map(Box::new), scope),
            }
        } else if self.eat_symbol(kw::While) {
            self.expect(TokenKind::LParen);
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            let content = self
                .parse_statement_or_expr()
                .unwrap_or_else(|| report_fatal_error("while must have a body", self.token.span()));
            if content.is_labelled_function() {
                report_fatal_error(
                    "functions as the body of a while statement may not be labelled",
                    content.span(),
                )
            }
            Statement {
                kind: StatementKind::While(expr, Box::new(content)),
            }
        } else if self.eat_symbol(kw::Do) {
            let content = self.parse_statement().unwrap_or_else(|| {
                report_fatal_error("do/while statements must have a body", self.token.span())
            });
            if content.is_labelled_function() {
                report_fatal_error(
                    "functions as the body of a do/while statement may not be labelled",
                    content.span(),
                )
            }
            self.expect_symbol(kw::While);
            self.expect(TokenKind::LParen);
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            Statement {
                kind: StatementKind::DoWhile(Box::new(content), expr),
            }
        } else if self.eat_symbol(kw::For) {
            self.expect(TokenKind::LParen);
            let span_for_err = self.prev_token.span();
            let mut header_scope = Scope::new();
            let init = if self.eat(TokenKind::Semicolon) {
                None
            } else {
                Some(if let Some(decl) = self.parse_variable_declaration() {
                    header_scope.add_declaration(&decl);
                    decl.map(ForInit::VariableDeclaration)
                } else {
                    self.parse_expression().map(ForInit::Expression)
                })
            };

            let get_target = |loopkind| match init.as_deref().unwrap_or_else(|| {
                report_fatal_error(
                    format!("{loopkind} must have a target variable"),
                    span_for_err,
                )
            }) {
                ForInit::VariableDeclaration(VariableDeclaration {
                    declarations, kind, ..
                }) => {
                    if declarations.len() != 1 {
                        report_fatal_error(
                            format!("{loopkind} loops may only have one target variable"),
                            span_for_err,
                        )
                    }
                    let decl = &declarations[0];
                    if decl.init.is_some() {
                        report_fatal_error(
                            format!("{loopkind} target variables must not have an initializer"),
                            span_for_err,
                        );
                    };
                    ForTarget::Declaration(*kind, decl.name)
                }
                ForInit::Expression(Expression {
                    kind: ExpressionKind::Identifier(ident),
                    ..
                }) => ForTarget::Variable(*ident),
                _ => report_fatal_error(
                    format!("invalid target variable in {loopkind}"),
                    span_for_err,
                ),
            };

            let kind = if self.eat_symbol(kw::Of) {
                let expr = self.parse_expression();
                self.expect(TokenKind::RParen);
                let (content, body_scope) = parse_with_scope!(self:self
                    .parse_statement_or_expr()
                    .unwrap_or_else(|| report_fatal_error("for/of loops must have a body", self.token.span())));
                if content.is_labelled_function() {
                    report_fatal_error(
                        "functions as the body of a for/of statement may not be labelled",
                        content.span(),
                    )
                }
                StatementKind::ForOf {
                    target: get_target("for/of"),
                    iter: expr,
                    header_scope,
                    body: Box::new(content),
                    body_scope,
                }
            } else if self.eat_symbol(kw::In) {
                let expr = self.parse_expression();
                self.expect(TokenKind::RParen);
                let (content, body_scope) = parse_with_scope!(self:self
                    .parse_statement_or_expr()
                    .unwrap_or_else(|| report_fatal_error("for/in loops must have a body", self.token.span())));
                if content.is_labelled_function() {
                    report_fatal_error(
                        "functions as the body of a for/in statement may not be labelled",
                        content.span(),
                    )
                }
                StatementKind::ForIn {
                    header_scope,
                    target: get_target("for/in"),
                    iter: expr,
                    body: Box::new(content),
                    body_scope,
                }
            } else {
                // If init was empty or a declaration, we will have already consumed the semicolon.
                // Otherwise, do it now
                if self.prev_token.kind() != TokenKind::Semicolon {
                    self.expect(TokenKind::Semicolon);
                }
                let test = if self.token.kind() == TokenKind::Semicolon {
                    None
                } else {
                    Some(self.parse_expression())
                };
                self.expect(TokenKind::Semicolon);

                let update = if self.token.kind() == TokenKind::RParen {
                    None
                } else {
                    Some(self.parse_expression())
                };
                self.expect(TokenKind::RParen);
                let (content, body_scope) = parse_with_scope!(self:self
                    .parse_statement_or_expr()
                    .unwrap_or_else(|| report_fatal_error("for loops must have a body", self.token.span())));
                if content.is_labelled_function() {
                    report_fatal_error(
                        "functions as the body of a for statement may not be labelled",
                        content.span(),
                    )
                }
                StatementKind::For {
                    header_scope,
                    init,
                    test,
                    update,
                    body: Box::new(content),
                    body_scope,
                }
            };

            Statement { kind }
        } else if let Some(tokens) = self.eat_many(&[TokenKind::Ident, TokenKind::Colon]) {
            let stmt = self.parse_statement_or_expr().unwrap_or_else(|| {
                report_fatal_error("label must be followed by statement", self.token.span())
            });
            assert_eq!(tokens.len(), 2);
            let ident = Symbol::intern(tokens[1].source_string(self.src()));
            Statement {
                kind: StatementKind::Labeled(ident, Box::new(stmt)),
            }
        } else if self.eat_symbol(kw::Break) {
            // FIXME: Don't allow this outside loops
            let ident = self.eat_ident();
            self.eat(TokenKind::Semicolon);

            Statement {
                kind: StatementKind::Break(ident),
            }
        } else if self.eat_symbol(kw::Continue) {
            // FIXME: Don't allow this outside loops
            let ident = self.eat_ident();
            self.eat(TokenKind::Semicolon);
            Statement {
                kind: StatementKind::Continue(ident),
            }
        } else if self.eat_symbol(kw::Return) {
            let expr = self.try_parse_expression();
            self.eat(TokenKind::Semicolon);
            Statement {
                kind: StatementKind::Return(expr),
            }
        } else if self.eat_symbol(kw::Throw) {
            let expr = self.parse_expression();
            self.eat(TokenKind::Semicolon);
            Statement {
                kind: StatementKind::Throw(expr),
            }
        } else if self.eat_symbol(kw::Function) {
            let name = self.expect_ident_no_kw();
            self.expect(TokenKind::LParen);
            let (params, rest) = self.parse_function_params();
            self.expect(TokenKind::RParen);
            self.expect(TokenKind::LBrace);
            let block = self.parse_block();
            self.expect(TokenKind::RBrace);
            let span = span.to(self.prev_token.span());
            let func = Function {
                name: Some(name),
                params,
                rest,
                body: block,
            };
            check_function_early_errors(&func, span);
            Statement {
                kind: StatementKind::FunctionDeclaration(self.alloc_node(func, span)),
            }
        } else if let Some(decl) = self.parse_variable_declaration() {
            self.eat(TokenKind::Semicolon);
            self.scopes
                .last_mut()
                .expect("there should always be at least one scope")
                .add_declaration(&decl);
            Statement {
                kind: StatementKind::VariableDeclaration(decl),
            }
        } else if self.eat_symbol(kw::Switch) {
            self.expect(TokenKind::LParen);
            let scrutinee = self.parse_expression();
            self.expect(TokenKind::RParen);
            self.expect(TokenKind::LBrace);
            let mut cases = vec![];
            let mut default_set = false;
            loop {
                let span = self.token.span();
                let test = if self.eat_symbol(kw::Default) {
                    if default_set {
                        report_fatal_error(
                            "more than one `default` clause in a switch statement",
                            self.prev_token.span(),
                        )
                    }
                    default_set = true;
                    None
                } else if self.eat_symbol(kw::Case) {
                    Some(self.parse_expression())
                } else {
                    break;
                };
                self.expect(TokenKind::Colon);
                // Special case for empty cases
                let consequent = if matches!(self.peek_ident(), Some(kw::Case | kw::Default)) {
                    self.alloc_node(
                        Statement {
                            kind: StatementKind::Empty,
                        },
                        self.token.span().shrink_to_lo(),
                    )
                } else {
                    self.parse_statement_or_expr().unwrap_or_else(|| {
                        self.alloc_node(
                            Statement {
                                kind: StatementKind::Empty,
                            },
                            self.token.span().shrink_to_lo(),
                        )
                    })
                };
                cases.push(self.alloc_node(
                    SwitchCase { test, consequent },
                    span.to(self.prev_token.span()),
                ));
            }
            self.expect(TokenKind::RBrace);
            Statement {
                kind: StatementKind::Switch(scrutinee, cases),
            }
        } else if self.eat_symbol(kw::Try) {
            self.expect(TokenKind::LBrace);
            let block = self.parse_block();
            self.expect(TokenKind::RBrace);
            let mut catch = None;
            let mut finally = None;

            if self.eat_symbol(kw::Catch) {
                let span = self.prev_token.span();
                let param = self.eat(TokenKind::LParen).then(|| {
                    let sym = self.expect_ident_no_kw();
                    self.expect(TokenKind::RParen);
                    sym
                });
                self.expect(TokenKind::LBrace);
                // inline parse_block here because we need to add the catch variable to the scope
                let block = {
                    let (statements, scope) = parse_with_scope! {self:
                        {
                            if let Some(sym) = param {
                                self.scopes
                                    .last_mut()
                                    .expect("there should always be one scope")
                                    .insert(sym, VariableKind::Var);
                            }
                            std::iter::from_fn(|| self.parse_statement_or_expr()).collect()
                        }
                    };
                    Block { statements, scope }
                };
                self.expect(TokenKind::RBrace);
                catch = Some(self.alloc_node(
                    CatchClause { param, block },
                    span.to(self.prev_token.span()),
                ));
            }

            if self.eat_symbol(kw::Finally) {
                self.expect(TokenKind::LBrace);
                let block = self.parse_block();
                self.expect(TokenKind::RBrace);
                finally = Some(block);
            }

            Statement {
                kind: StatementKind::Try(block, catch, finally),
            }
        } else {
            return None;
        };
        Some(self.alloc_node(statement, span.to(self.prev_token.span().shrink_to_hi())))
    }

    /// Attempts to parse a statement. Will fallback to parsing an expression and wrapping
    /// it in a [`StatementKind::Expression`], or returning a [`StatementKind::Empty`] as
    /// a last resort.
    // FIXME: make this non-optional as it never returns none
    fn parse_statement_or_expr(&mut self) -> Option<Node<Statement>> {
        self.parse_statement().or_else(|| {
            let span = self.token.span().shrink_to_lo();
            let kind = self
                .try_parse_expression()
                .map_or(StatementKind::Empty, StatementKind::Expression);
            self.eat(TokenKind::Semicolon);
            let span = if matches!(kind, StatementKind::Empty) {
                self.prev_token.span()
            } else {
                span.to(self.prev_token.span().shrink_to_hi())
            };
            Some(self.alloc_node(Statement { kind }, span))
        })
    }

    fn parse_block(&mut self) -> Block {
        let (statements, scope) = parse_with_scope! { self:
            std::iter::from_fn(|| self.parse_statement_or_expr()).take_while(|stmt| !matches!(stmt.kind, StatementKind::Empty)).collect()
        };
        Block { statements, scope }
    }

    #[track_caller]
    fn parse_expression(&mut self) -> Node<Expression> {
        self.parse_expression_precedence(Precedence::TERNARY)
    }

    fn try_parse_expression(&mut self) -> Option<Node<Expression>> {
        self.try_parse_expression_precedence(Precedence::TERNARY)
    }

    #[track_caller]
    fn parse_expression_precedence(&mut self, prec: Precedence) -> Node<Expression> {
        self.try_parse_expression_precedence(prec)
            .unwrap_or_else(|| report_fatal_error("expected expression", self.token.span()))
    }

    fn try_parse_expression_precedence(&mut self, prec: Precedence) -> Option<Node<Expression>> {
        let rule = self.get_rule(self.token);
        let func = rule.prefix?;
        self.advance();
        let mut expr = func(self);
        while self.get_rule(self.token).precedence >= prec {
            self.advance();
            expr = self.get_rule(self.prev_token).infix.unwrap()(self, expr);
        }
        Some(expr)
    }

    fn get_rule(&self, token: Token) -> ParseRule<'a> {
        macro_rules! r {
            () => {
                ParseRule {
                    prefix: None,
                    infix: None,
                    precedence: Precedence(0),
                }
            };
            (_, $infix:path) => {
                ParseRule {
                    prefix: None,
                    infix: Some($infix),
                    precedence: Precedence(0),
                }
            };
            (_, $infix:path, $prec:ident) => {
                ParseRule {
                    prefix: None,
                    infix: Some($infix),
                    precedence: Precedence::$prec,
                }
            };
            ($prefix:path, _) => {
                ParseRule {
                    prefix: Some($prefix),
                    infix: None,
                    precedence: Precedence(0),
                }
            };
            ($prefix:path, _, $prec:ident) => {
                ParseRule {
                    prefix: Some($prefix),
                    infix: None,
                    precedence: Precedence::$prec,
                }
            };
            ($prefix:path, $infix:path, $prec:ident) => {
                ParseRule {
                    prefix: Some($prefix),
                    infix: Some($infix),
                    precedence: Precedence::$prec,
                }
            };
        }
        match token.kind() {
            TokenKind::EqualsEquals
            | TokenKind::BangEquals
            | TokenKind::EqualsEqualsEquals
            | TokenKind::BangEqualsEquals => r!(Self::parse_unary, _, EQUALITY),
            TokenKind::Gt | TokenKind::GtEquals | TokenKind::Lt | TokenKind::LtEquals => {
                r!(_, Self::parse_binary, COMPARE)
            }
            TokenKind::LtLt | TokenKind::GtGt | TokenKind::GtGtGt => {
                r!(_, Self::parse_binary, SHIFT)
            }
            TokenKind::Plus | TokenKind::Minus => r!(Self::parse_unary, Self::parse_binary, UNARY),
            TokenKind::Slash | TokenKind::Asterisk | TokenKind::Percent => {
                r!(_, Self::parse_binary, FACTOR)
            }
            TokenKind::Bar => r!(_, Self::parse_binary, BITWISE_OR),
            TokenKind::Caret => r!(_, Self::parse_binary, BITWISE_XOR),
            TokenKind::And => r!(_, Self::parse_binary, BITWISE_AND),
            TokenKind::Bang | TokenKind::Tilde => {
                r!(Self::parse_unary, _, UNARY)
            }
            TokenKind::PlusPlus | TokenKind::MinusMinus => r!(_, Self::parse_postfix, POSTFIX),
            TokenKind::BarBar => r!(_, Self::parse_logical, LOGICAL_OR),
            TokenKind::AndAnd => r!(_, Self::parse_logical, LOGICAL_AND),
            TokenKind::Dot => r!(_, Self::parse_member, MEMBER),
            TokenKind::Ident => match self.intern(token) {
                kw::TypeOf => r!(Self::parse_unary, _, UNARY),
                kw::This => r!(Self::parse_this, _),
                kw::New => r!(Self::parse_new, _, NEW),
                kw::Delete => r!(Self::parse_delete, _, DELETE),
                kw::Function => r!(Self::parse_function_expression, _),
                kw::Case | kw::Default => r!(),
                kw::Of => r!(),
                kw::In => r!(_, Self::parse_binary, COMPARE),
                _ => r!(Self::parse_identifier, _),
            },
            TokenKind::Question => r!(_, Self::parse_ternary, TERNARY),
            TokenKind::Equals
            | TokenKind::PlusEquals
            | TokenKind::MinusEquals
            | TokenKind::AsteriskEquals
            | TokenKind::SlashEquals
            | TokenKind::PercentEquals
            | TokenKind::LtLtEquals
            | TokenKind::GtGtEquals
            | TokenKind::GtGtGtEquals
            | TokenKind::BarEquals
            | TokenKind::CaretEquals
            | TokenKind::AndEquals => r!(_, Self::parse_assign_expression, ASSIGNMENT),
            TokenKind::Literal(_) => r!(Self::parse_literal, _),
            TokenKind::LBracket => r!(Self::parse_array, Self::parse_member, MEMBER),
            TokenKind::LBrace => r!(Self::parse_object, _),
            TokenKind::Comma | TokenKind::RBracket | TokenKind::RBrace => r!(),
            TokenKind::LParen => r!(Self::parse_grouping, Self::parse_call, GROUPING),
            TokenKind::RParen => r!(),
            TokenKind::Semicolon | TokenKind::Colon | TokenKind::Eof => r!(),
            k => todo!("`{k:?}`"),
        }
    }

    fn parse_variable_declaration(&mut self) -> Option<Node<VariableDeclaration>> {
        for (kw, kind) in [
            (kw::Var, VariableKind::Var),
            (kw::Let, VariableKind::Let),
            (kw::Const, VariableKind::Const),
        ] {
            if !self.eat_symbol(kw) {
                continue;
            }
            let span = self.prev_token.span();
            let mut decls = vec![];
            loop {
                let sp = self.token.span();
                let name = self.expect_ident_no_kw();
                let init = self
                    .eat(TokenKind::Equals)
                    .then(|| self.parse_expression_precedence(Precedence::COMMA));
                decls.push(self.alloc_node(
                    VariableDeclarator { name, init },
                    sp.to(self.prev_token.span()),
                ));
                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
            let decl_span = span.to(self.prev_token.span());
            self.eat(TokenKind::Semicolon);
            return Some(self.alloc_node(
                VariableDeclaration {
                    declarations: decls,
                    kind,
                },
                decl_span,
            ));
        }
        None
    }

    fn parse_unary(&mut self) -> Node<Expression> {
        let op = match self.prev_token.kind() {
            TokenKind::Plus => UnaryOperator::Plus,
            TokenKind::Minus => UnaryOperator::Minus,
            TokenKind::Bang => UnaryOperator::LogicalNot,
            TokenKind::Tilde => UnaryOperator::BitwiseNot,
            TokenKind::Ident => {
                debug_assert!(self.intern(self.prev_token) == kw::TypeOf);
                UnaryOperator::TypeOf
            }
            _ => unreachable!(),
        };
        let op_span = self.prev_token.span();
        let expr = self.parse_expression_precedence(Precedence::UNARY);
        let span = op_span.to(expr.span());
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Unary(op, Box::new(expr)),
            },
            span,
        )
    }

    fn parse_binary(&mut self, left: Node<Expression>) -> Node<Expression> {
        let op = match self.prev_token.kind() {
            TokenKind::EqualsEquals => BinaryOperator::EqEq,
            TokenKind::EqualsEqualsEquals => BinaryOperator::EqEqEq,
            TokenKind::BangEquals => BinaryOperator::NotEq,
            TokenKind::BangEqualsEquals => BinaryOperator::NotEqEq,
            TokenKind::Plus => BinaryOperator::Plus,
            TokenKind::Minus => BinaryOperator::Minus,
            TokenKind::Slash => BinaryOperator::Divide,
            TokenKind::Percent => BinaryOperator::Modulo,
            TokenKind::And => BinaryOperator::And,
            TokenKind::Bar => BinaryOperator::Or,
            TokenKind::Caret => BinaryOperator::Xor,
            TokenKind::Asterisk => BinaryOperator::Multiply,
            TokenKind::Lt => BinaryOperator::Lt,
            TokenKind::LtEquals => BinaryOperator::LtE,
            TokenKind::LtLt => BinaryOperator::LShift,
            TokenKind::GtGt => BinaryOperator::RShift,
            TokenKind::GtGtGt => BinaryOperator::GtGtGt,
            TokenKind::Gt => BinaryOperator::Gt,
            TokenKind::GtEquals => BinaryOperator::GtE,
            TokenKind::Ident => match self.intern(self.prev_token) {
                kw::In => BinaryOperator::In,
                kw::InstanceOf => BinaryOperator::InstanceOf,
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };
        let rule = self.get_rule(self.prev_token);
        let right = self.parse_expression_precedence(rule.precedence.next());
        let span = left.span().to(right.span());
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Binary(Box::new(left), op, Box::new(right)),
            },
            span,
        )
    }

    fn parse_logical(&mut self, left: Node<Expression>) -> Node<Expression> {
        let op = match self.prev_token.kind() {
            TokenKind::BarBar => LogicalOperator::Or,
            TokenKind::AndAnd => LogicalOperator::And,
            _ => unreachable!(),
        };
        let rule = self.get_rule(self.prev_token);
        let right = self.parse_expression_precedence(rule.precedence.next());
        let span = left.span().to(right.span());
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Logical(Box::new(left), op, Box::new(right)),
            },
            span,
        )
    }

    fn parse_postfix(&mut self, left: Node<Expression>) -> Node<Expression> {
        if !left.kind.is_simple_assignment_target() {
            report_fatal_error(
                "invalid left side of postfix operation",
                left.span().to(self.prev_token.span()),
            );
        }
        let op = match self.prev_token.kind() {
            TokenKind::PlusPlus => UpdateOperator::PlusPlus,
            TokenKind::MinusMinus => UpdateOperator::MinusMinus,
            _ => unreachable!(),
        };
        let span = left.span().to(self.prev_token.span());
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Update(Box::new(left), op, false),
            },
            span,
        )
    }

    fn parse_identifier(&mut self) -> Node<Expression> {
        // could be a single-arg arrow function
        if let Some(arrow) = self.try_parse_arrow_expression() {
            return arrow;
        }
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Identifier(Symbol::intern(
                    self.prev_token.source_string(self.src()),
                )),
            },
            self.prev_token.span(),
        )
    }

    fn parse_this(&mut self) -> Node<Expression> {
        self.alloc_node(
            Expression {
                kind: ExpressionKind::This,
            },
            self.prev_token.span(),
        )
    }

    fn parse_literal(&mut self) -> Node<Expression> {
        let TokenKind::Literal(lit) = self.prev_token.kind() else {
            unreachable!()
        };
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Literal(lit),
            },
            self.prev_token.span(),
        )
    }

    fn parse_array(&mut self) -> Node<Expression> {
        let start = self.prev_token.span();
        let exprs = self.parse_delimited_list(TokenKind::Comma, Self::try_parse_expression);
        self.expect(TokenKind::RBracket);
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Array(exprs),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_object(&mut self) -> Node<Expression> {
        let start = self.prev_token.span();
        let props = self.parse_delimited_list(TokenKind::Comma, Self::parse_object_prop);

        self.expect(TokenKind::RBrace);
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Object(props),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_object_prop(&mut self) -> Option<(Symbol, Option<Node<Expression>>)> {
        let ident = self.eat_ident()?;
        Some((
            ident,
            self.eat(TokenKind::Colon).then(|| self.parse_expression()),
        ))
    }

    fn parse_member(&mut self, left: Node<Expression>) -> Node<Expression> {
        let start = self.prev_token.span();
        let key = match self.prev_token.kind() {
            TokenKind::Dot => MemberKey::Static(self.expect_ident()),
            TokenKind::LBracket => {
                let rule = self.get_rule(self.prev_token);
                let right = self.parse_expression_precedence(rule.precedence.next());
                self.expect(TokenKind::RBracket);
                MemberKey::Computed(Box::new(right))
            }
            _ => unreachable!(),
        };
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Member(Box::new(left), key),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_call(&mut self, left: Node<Expression>) -> Node<Expression> {
        let start = left.span();
        let args = self.parse_delimited_list(TokenKind::Comma, Self::try_parse_expression);
        self.expect(TokenKind::RParen);
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Call(Box::new(left), args),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_new(&mut self) -> Node<Expression> {
        let start = self.prev_token.span();
        let target = self.parse_expression_precedence(Precedence::CALL);

        self.alloc_node(
            Expression {
                kind: ExpressionKind::New(Box::new(target)),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_delete(&mut self) -> Node<Expression> {
        let start = self.prev_token.span();
        let target = self.parse_expression_precedence(Precedence::DELETE.next());

        self.alloc_node(
            Expression {
                kind: ExpressionKind::Delete(Box::new(target)),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_function_expression(&mut self) -> Node<Expression> {
        let start = self.prev_token.span();
        let name = self.eat_ident();
        self.expect(TokenKind::LParen);
        let (params, rest) = self.parse_function_params();
        self.expect(TokenKind::RParen);
        self.expect(TokenKind::LBrace);
        let body = self.parse_block();
        self.expect(TokenKind::RBrace);
        let span = start.to(self.prev_token.span());

        let func = Function {
            name,
            params,
            rest,
            body,
        };
        check_function_early_errors(&func, span);

        self.alloc_node(
            Expression {
                kind: ExpressionKind::Function(self.alloc_node(func, span)),
            },
            span,
        )
    }

    fn parse_grouping(&mut self) -> Node<Expression> {
        if let Some(expr) = self.try_parse_arrow_expression() {
            return expr;
        }
        let expr = self.parse_expression_precedence(Precedence::COMMA);
        self.expect(TokenKind::RParen);
        expr
    }

    fn try_parse_arrow_expression(&mut self) -> Option<Node<Expression>> {
        // non-parenthesised form
        let span = self.prev_token.span();
        let (params, rest) = if let TokenKind::Ident = self.prev_token.kind() {
            let arg = self.intern(self.prev_token);
            if !self.eat(TokenKind::Arrow) {
                return None;
            }
            (ParamList(vec![FunctionParam::Normal(arg)]), None)
        } else if let TokenKind::LParen = self.prev_token.kind() {
            let lookahead = self.lookahead_delim(TokenKind::LParen, TokenKind::RParen);
            if lookahead.token.kind() != TokenKind::Arrow {
                return None;
            }
            let (params, rest) = self.parse_function_params();
            self.expect(TokenKind::RParen);
            self.expect(TokenKind::Arrow);
            (params, rest)
        } else {
            return None;
        };
        let body = if self.eat(TokenKind::LBrace) {
            let body = self.parse_block();
            self.expect(TokenKind::RBrace);
            body
        } else {
            let expr = self.parse_expression();
            let span = expr.span();
            Block {
                statements: vec![self.alloc_node(
                    Statement {
                        kind: StatementKind::Expression(expr),
                    },
                    span,
                )],
                scope: Scope::new(),
            }
        };
        let span = span.to(self.prev_token.span());
        let func = Function {
            name: None,
            params,
            rest,
            body,
        };
        check_function_early_errors(&func, span);

        let span = span.to(self.prev_token.span());
        Some(self.alloc_node(
            Expression {
                kind: ExpressionKind::Arrow(self.alloc_node(func, span)),
            },
            span,
        ))
    }

    /// Parses function params without parens. Also returns the `rest` param if there was one
    fn parse_function_params(&mut self) -> (ParamList, Option<Symbol>) {
        let params =
            ParamList(self.parse_delimited_list(TokenKind::Comma, Self::parse_function_param));
        let rest = self
            .eat(TokenKind::DotDotDot)
            .then(|| self.expect_ident_no_kw());
        (params, rest)
    }

    fn parse_function_param(&mut self) -> Option<FunctionParam> {
        let ident = self.eat_ident()?;
        if ident.is_keyword() {
            report_fatal_error(
                "expected an identifier, found keyword",
                self.prev_token.span(),
            )
        }
        if self.eat(TokenKind::Equals) {
            Some(FunctionParam::Defaulted(ident, self.parse_expression()))
        } else {
            Some(FunctionParam::Normal(ident))
        }
    }

    fn parse_assign_expression(&mut self, left: Node<Expression>) -> Node<Expression> {
        let op = match self.prev_token.kind() {
            TokenKind::Equals => AssignmentOperator::Eq,
            TokenKind::PlusEquals => AssignmentOperator::PlusEq,
            TokenKind::MinusEquals => AssignmentOperator::MinusEq,
            TokenKind::AsteriskEquals => AssignmentOperator::MulEq,
            TokenKind::SlashEquals => AssignmentOperator::DivEq,
            TokenKind::PercentEquals => AssignmentOperator::ModEq,
            TokenKind::LtLtEquals => AssignmentOperator::LShiftEq,
            TokenKind::GtGtEquals => AssignmentOperator::RShiftEq,
            TokenKind::GtGtGtEquals => AssignmentOperator::GtGtGtEq,
            TokenKind::BarEquals => AssignmentOperator::OrEq,
            TokenKind::CaretEquals => AssignmentOperator::XorEq,
            TokenKind::AndEquals => AssignmentOperator::AndEq,
            _ => unreachable!(),
        };
        let ExpressionKind::Identifier(var) = left.kind else {
            // TODO: support proper patterns
            report_fatal_error(
                "left-hand side of assignment must be variable (for now)",
                left.span(),
            );
        };
        let right = self.parse_expression_precedence(Precedence::ASSIGNMENT.next());
        let span = left.span().to(right.span());
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Assignment(var, op, Box::new(right)),
            },
            span,
        )
    }

    fn parse_ternary(&mut self, left: Node<Expression>) -> Node<Expression> {
        let consequent = Box::new(self.parse_expression_precedence(Precedence::TERNARY.next()));
        self.expect(TokenKind::Colon);
        let alternate = Box::new(self.parse_expression_precedence(Precedence::TERNARY.next()));
        let span = left.span().to(self.prev_token.span());
        self.alloc_node(
            Expression {
                kind: ExpressionKind::Ternary {
                    test: Box::new(left),
                    consequent,
                    alternate,
                },
            },
            span,
        )
    }
}

/// Check for early errors in a function.
/// 1. BoundNames(ParamList).Intersection(LexicallyDefinedNames(body)).len() > 0
fn check_function_early_errors(Function { params, body, .. }: &Function, span: Span) {
    // FIXME: move rest into params
    let bound = params.bound_names().collect::<HashSet<_>>();
    for name in body.lexically_declared_names() {
        if bound.contains(&name) {
            report_fatal_error(
                format!("identifier `{name}` has already been defined as a parameter"),
                span,
            );
        }
    }
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Default, Debug)]
struct Precedence(usize);

impl Precedence {
    pub fn next(self) -> Self {
        Self(self.0 + 1)
    }

    const COMMA: Self = Self(1);
    const TERNARY: Self = Self(2);
    // const ARROW: Self = Self(2);
    const ASSIGNMENT: Self = Self(2);
    const LOGICAL_OR: Self = Self(3);
    const LOGICAL_AND: Self = Self(4);
    const BITWISE_OR: Self = Self(5);
    const BITWISE_XOR: Self = Self(6);
    const BITWISE_AND: Self = Self(7);
    const EQUALITY: Self = Self(8);
    const COMPARE: Self = Self(9);
    const SHIFT: Self = Self(10);
    // const ADDITION: Self = Self(11);
    const FACTOR: Self = Self(12);
    const UNARY: Self = Self(14);
    const POSTFIX: Self = Self(15);
    const MEMBER: Self = Self(17);
    const NEW: Self = Self(17);
    const DELETE: Self = Self(17);
    const CALL: Self = Self(17);
    const GROUPING: Self = Self(18);
}

type PrefixFn<'b> = for<'a> fn(&'a mut Parser<'b>) -> Node<Expression>;
type InfixFn<'b> = for<'a> fn(&'a mut Parser<'b>, Node<Expression>) -> Node<Expression>;
#[derive(Default, Debug)]
struct ParseRule<'b> {
    prefix: Option<PrefixFn<'b>>,
    infix: Option<InfixFn<'b>>,
    precedence: Precedence,
}
