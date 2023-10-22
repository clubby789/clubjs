use crate::{
    intern::Symbol,
    lex::{kw, Lexer, Token, TokenKind},
    session::Session,
    span::{SourceMap, Span, Spanned},
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    body: Vec<Spanned<Statement>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    // May be None for function expressions
    name: Option<Symbol>,
    params: Vec<FunctionParam>,
    rest: Option<Symbol>,
    body: Vec<Spanned<Statement>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum FunctionParam {
    Normal(Symbol),
    Defaulted(Symbol, Spanned<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclaration {
    declarations: Vec<Spanned<VariableDeclarator>>,
    kind: VariableKind,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum VariableKind {
    Var,
    Let,
    Const,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclarator {
    name: Symbol,
    init: Option<Spanned<Expression>>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement {
    kind: StatementKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind {
    Empty,
    Debugger,
    Block(Vec<Spanned<Statement>>),
    Expression(Spanned<Expression>),
    If(
        Spanned<Expression>,
        Box<Spanned<Statement>>,
        Option<Box<Spanned<Statement>>>,
    ),
    Labeled(Symbol, Box<Spanned<Statement>>),
    Break(Option<Symbol>),
    Continue(Option<Symbol>),
    Switch(Spanned<Expression>, Vec<Spanned<SwitchCase>>),
    Return(Option<Spanned<Expression>>),
    Throw(Spanned<Expression>),
    Try(
        Vec<Spanned<Statement>>,
        Option<Spanned<CatchClause>>,
        Option<Vec<Spanned<Statement>>>,
    ),
    While(Spanned<Expression>, Box<Spanned<Statement>>),
    DoWhile(Box<Spanned<Statement>>, Spanned<Expression>),
    For(
        Option<Spanned<ForInit>>,
        Option<Spanned<Expression>>,
        Option<Spanned<Expression>>,
        Box<Spanned<Statement>>,
    ),
    ForIn(ForTarget, Spanned<Expression>, Box<Spanned<Statement>>),
    ForOf(ForTarget, Spanned<Expression>, Box<Spanned<Statement>>),
    VariableDeclaration(Spanned<VariableDeclaration>),
    FunctionDeclaration(Spanned<Function>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    kind: ExpressionKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    This,
    Array(Vec<Spanned<Expression>>),
    Object(Vec<(Symbol, Option<Spanned<Expression>>)>),
    Function(Spanned<Function>),
    Arrow(Spanned<Function>),
    Unary(UnaryOperator, Box<Spanned<Expression>>),
    Binary(
        Box<Spanned<Expression>>,
        BinaryOperator,
        Box<Spanned<Expression>>,
    ),
    Assignment(Symbol, AssignmentOperator, Box<Spanned<Expression>>),
    /// the bool is true if the operator is a prefix
    Update(Box<Spanned<Expression>>, UpdateOperator, bool),
    Logical(
        Box<Spanned<Expression>>,
        LogicalOperator,
        Box<Spanned<Expression>>,
    ),
    Ternary {
        test: Box<Spanned<Expression>>,
        consequent: Box<Spanned<Expression>>,
        alternate: Box<Spanned<Expression>>,
    },
    New(Box<Spanned<Expression>>),
    Call(Box<Spanned<Expression>>, Vec<Spanned<Expression>>),
    Member(Box<Spanned<Expression>>, MemberKey),
    // TODO: Support yield and generators
    // Yield(Option<Box<Spanned<Expression>>>),
    Literal(crate::lex::Literal),
    Identifier(Symbol),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemberKey {
    Static(Symbol),
    Computed(Box<Spanned<Expression>>),
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
    test: Option<Spanned<Expression>>,
    consequent: Spanned<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CatchClause {
    param: Option<Symbol>,
    block: Vec<Spanned<Statement>>,
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
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        let mut lexer = Lexer::new(source);
        crate::SESSION
            .set(Session::new(SourceMap::from_src(source.to_string())))
            .unwrap();
        let token = lexer.next_token();
        Self {
            lexer,
            prev_token: Token::default(),
            token,
        }
    }

    pub fn src(&self) -> &str {
        self.lexer.src()
    }

    pub fn parse(mut self) -> Spanned<Program> {
        let span = Span::new(0, self.src().len());
        let body = std::iter::from_fn(|| self.parse_statement()).collect::<Vec<_>>();
        Spanned::new(Program { body }, span)
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
            eprintln!(
                "expected `{:?}` but found `{:?}`:\n{}",
                kind,
                self.token.kind(),
                crate::SESSION
                    .get()
                    .unwrap()
                    .sourcemap()
                    .render_source_span(dbg!(self.token.span()))
            );
            std::process::exit(1);
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

    fn peek_ident(&mut self) -> Option<Symbol> {
        matches!(self.token.kind(), TokenKind::Ident).then(|| self.intern(self.token))
    }

    #[track_caller]
    fn expect_ident(&mut self) -> Symbol {
        self.eat_ident().expect("expected an identifier")
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

    fn parse_statement(&mut self) -> Option<Spanned<Statement>> {
        if matches!(self.token.kind(), TokenKind::Eof | TokenKind::RBrace) {
            return None;
        }
        let span = self.prev_token.span().shrink_to_hi();
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
            let block = std::iter::from_fn(|| self.parse_statement()).collect();
            self.expect(TokenKind::RBrace);
            Statement {
                kind: StatementKind::Block(block),
            }
        } else if self.eat_symbol(kw::If) {
            self.expect(TokenKind::LParen);
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            let content = self
                .parse_statement()
                .expect("if statements must have a block");
            let alternate = self
                .eat_symbol(kw::Else)
                .then(|| self.parse_statement())
                .flatten();
            Statement {
                kind: StatementKind::If(expr, Box::new(content), alternate.map(Box::new)),
            }
        } else if self.eat_symbol(kw::While) {
            self.expect(TokenKind::LParen);
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            let content = self
                .parse_statement()
                .expect("while loops must have a body");

            Statement {
                kind: StatementKind::While(expr, Box::new(content)),
            }
        } else if self.eat_symbol(kw::Do) {
            let content = self
                .parse_statement()
                .expect("do/while statements must have a body");
            assert!(self.eat_symbol(kw::While), "expected `while` after `do`");
            self.expect(TokenKind::LParen);
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            Statement {
                kind: StatementKind::DoWhile(Box::new(content), expr),
            }
        } else if self.eat_symbol(kw::For) {
            self.expect(TokenKind::LParen);
            // TODO: support for (i = 1; ...)
            let init = if self.eat(TokenKind::Semicolon) {
                None
            } else {
                Some(if let Some(decl) = self.parse_variable_declaration() {
                    let (decl, span) = decl.consume();
                    Spanned::new(ForInit::VariableDeclaration(decl), span)
                } else {
                    let (expr, span) = self.parse_expression().consume();
                    Spanned::new(ForInit::Expression(expr), span)
                })
            };

            let get_target = |loopkind| match init
                .as_deref()
                .unwrap_or_else(|| panic!("{loopkind} must have a target variable"))
            {
                ForInit::VariableDeclaration(VariableDeclaration {
                    declarations, kind, ..
                }) => {
                    assert!(
                        declarations.len() == 1,
                        "{loopkind} loops may only have one target variable"
                    );
                    let decl = &declarations[0];
                    assert!(
                        decl.init.is_none(),
                        "{loopkind} target variables must not have an initializer"
                    );
                    ForTarget::Declaration(*kind, decl.name)
                }
                ForInit::Expression(Expression {
                    kind: ExpressionKind::Identifier(ident),
                    ..
                }) => ForTarget::Variable(*ident),
                _ => panic!("invalid target variable in {loopkind}"),
            };

            let kind = if self.eat_symbol(kw::Of) {
                let expr = self.parse_expression();
                self.expect(TokenKind::RParen);
                let content = self
                    .parse_statement()
                    .expect("for/of loops must have a body");
                StatementKind::ForOf(get_target("for/of"), expr, Box::new(content))
            } else if self.eat_symbol(kw::In) {
                let expr = self.parse_expression();
                self.expect(TokenKind::RParen);
                let content = self
                    .parse_statement()
                    .expect("for/in loops must have a body");
                StatementKind::ForIn(get_target("for/each"), expr, Box::new(content))
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
                let content = self.parse_statement().expect("for loops must have a body");
                StatementKind::For(init, test, update, Box::new(content))
            };

            Statement { kind }
        } else if let Some(tokens) = self.eat_many(&[TokenKind::Ident, TokenKind::Colon]) {
            let stmt = self
                .parse_statement()
                .expect("label must be followed by a statement");
            assert_eq!(tokens.len(), 2);
            let ident = Symbol::intern(tokens[1].source_string(self.src()));
            Statement {
                kind: StatementKind::Labeled(ident, Box::new(stmt)),
            }
        } else if self.eat_symbol(kw::Break) {
            let ident = self.eat_ident();
            self.eat(TokenKind::Semicolon);

            Statement {
                kind: StatementKind::Break(ident),
            }
        } else if self.eat_symbol(kw::Continue) {
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
            let name = self.expect_ident();
            self.expect(TokenKind::LParen);
            let (params, rest) = self.parse_function_params();
            self.expect(TokenKind::RParen);
            self.expect(TokenKind::LBrace);
            let body = std::iter::from_fn(|| self.parse_statement()).collect();
            self.expect(TokenKind::RBrace);
            let span = span.to(self.prev_token.span());
            Statement {
                kind: StatementKind::FunctionDeclaration(Spanned::new(
                    Function {
                        name: Some(name),
                        params,
                        rest,
                        body,
                    },
                    span,
                )),
            }
        } else if let Some(decl) = self.parse_variable_declaration() {
            self.eat(TokenKind::Semicolon);
            Statement {
                kind: StatementKind::VariableDeclaration(decl),
            }
        } else if matches!(self.token.kind(), TokenKind::Ident)
            && matches!(self.intern(self.token), kw::Var | kw::Let | kw::Const)
        {
            let kind = [
                (kw::Var, VariableKind::Var),
                (kw::Let, VariableKind::Let),
                (kw::Const, VariableKind::Const),
            ]
            .iter()
            .find_map(|&(kw, kind)| self.eat_symbol(kw).then_some(kind))
            .unwrap();

            let mut decls = vec![];
            loop {
                let sp = self.token.span();
                let name = self.expect_ident();
                let init = self
                    .eat(TokenKind::Equals)
                    .then(|| self.parse_expression_precedence(Precedence::COMMA));
                decls.push(Spanned::new(
                    VariableDeclarator { name, init },
                    sp.to(self.prev_token.span()),
                ));
                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
            let decl_span = span.to(self.prev_token.span());
            self.eat(TokenKind::Semicolon);
            Statement {
                kind: StatementKind::VariableDeclaration(Spanned::new(
                    VariableDeclaration {
                        declarations: decls,
                        kind,
                    },
                    decl_span,
                )),
            }
        } else if self.eat_symbol(kw::Switch) {
            self.expect(TokenKind::LParen);
            let scrutinee = self.parse_expression();
            self.expect(TokenKind::RParen);
            self.expect(TokenKind::LBrace);
            let mut cases = vec![];
            let mut default = false;
            loop {
                let span = self.token.span();
                let test = if self.eat_symbol(kw::Default) {
                    assert!(
                        !default,
                        "more than one `default` clause in a switch statement"
                    );
                    default = true;
                    None
                } else if self.eat_symbol(kw::Case) {
                    Some(self.parse_expression())
                } else {
                    break;
                };
                self.expect(TokenKind::Colon);
                // Special case for empty cases
                let consequent = if matches!(self.peek_ident(), Some(kw::Case | kw::Default)) {
                    Spanned::new(
                        Statement {
                            kind: StatementKind::Empty,
                        },
                        self.token.span().shrink_to_lo(),
                    )
                } else {
                    self.parse_statement().unwrap_or_else(|| {
                        Spanned::new(
                            Statement {
                                kind: StatementKind::Empty,
                            },
                            self.token.span().shrink_to_lo(),
                        )
                    })
                };
                cases.push(Spanned::new(
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
            let block = std::iter::from_fn(|| self.parse_statement()).collect();
            self.expect(TokenKind::RBrace);
            let mut catch = None;
            let mut finally = None;

            if self.eat_symbol(kw::Catch) {
                let span = self.prev_token.span();
                let param = self.eat(TokenKind::LParen).then(|| {
                    let sym = self.eat_ident().expect("expected an identifier");
                    self.expect(TokenKind::RParen);
                    sym
                });
                self.expect(TokenKind::LBrace);
                let block = std::iter::from_fn(|| self.parse_statement()).collect();
                self.expect(TokenKind::RBrace);
                catch = Some(Spanned::new(
                    CatchClause { param, block },
                    span.to(self.prev_token.span()),
                ));
            }

            if self.eat_symbol(kw::Finally) {
                self.expect(TokenKind::LBrace);
                let block = std::iter::from_fn(|| self.parse_statement()).collect();
                self.expect(TokenKind::RBrace);
                finally = Some(block);
            }

            Statement {
                kind: StatementKind::Try(block, catch, finally),
            }
        } else {
            let kind = self
                .try_parse_expression()
                .map_or(StatementKind::Empty, StatementKind::Expression);
            self.eat(TokenKind::Semicolon);

            Statement { kind }
        };
        Some(Spanned::new(statement, span.to(self.prev_token.span())))
    }

    #[track_caller]
    fn parse_expression(&mut self) -> Spanned<Expression> {
        self.parse_expression_precedence(Precedence::TERNARY)
    }

    fn try_parse_expression(&mut self) -> Option<Spanned<Expression>> {
        self.try_parse_expression_precedence(Precedence::TERNARY)
    }

    #[track_caller]
    fn parse_expression_precedence(&mut self, prec: Precedence) -> Spanned<Expression> {
        self.try_parse_expression_precedence(prec)
            .expect("expected expression")
    }

    fn try_parse_expression_precedence(&mut self, prec: Precedence) -> Option<Spanned<Expression>> {
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
                kw::Function => r!(Self::parse_function_expression, _),
                kw::Case | kw::Default => r!(),
                kw::Of => r!(),
                kw::In => r!(_, Self::parse_binary, COMPARE),
                #[cfg(debug_assertions)]
                sym if kw::KEYWORD_NAMES.contains(&sym.as_str()) => {
                    panic!("parsing keyword {sym:?} as identifier")
                }
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

    fn parse_variable_declaration(&mut self) -> Option<Spanned<VariableDeclaration>> {
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
                let name = self.expect_ident();
                let init = self
                    .eat(TokenKind::Equals)
                    .then(|| self.parse_expression_precedence(Precedence::COMMA));
                decls.push(Spanned::new(
                    VariableDeclarator { name, init },
                    sp.to(self.prev_token.span()),
                ));
                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
            let decl_span = span.to(self.prev_token.span());
            self.eat(TokenKind::Semicolon);
            return Some(Spanned::new(
                VariableDeclaration {
                    declarations: decls,
                    kind,
                },
                decl_span,
            ));
        }
        None
    }

    fn parse_unary(&mut self) -> Spanned<Expression> {
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
        Spanned::new(
            Expression {
                kind: ExpressionKind::Unary(op, Box::new(expr)),
            },
            span,
        )
    }

    fn parse_binary(&mut self, left: Spanned<Expression>) -> Spanned<Expression> {
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
        Spanned::new(
            Expression {
                kind: ExpressionKind::Binary(Box::new(left), op, Box::new(right)),
            },
            span,
        )
    }

    fn parse_logical(&mut self, left: Spanned<Expression>) -> Spanned<Expression> {
        let op = match self.prev_token.kind() {
            TokenKind::BarBar => LogicalOperator::Or,
            TokenKind::AndAnd => LogicalOperator::And,
            _ => unreachable!(),
        };
        let rule = self.get_rule(self.prev_token);
        let right = self.parse_expression_precedence(rule.precedence.next());
        let span = left.span().to(right.span());
        Spanned::new(
            Expression {
                kind: ExpressionKind::Logical(Box::new(left), op, Box::new(right)),
            },
            span,
        )
    }

    fn parse_postfix(&mut self, left: Spanned<Expression>) -> Spanned<Expression> {
        let op = match self.prev_token.kind() {
            TokenKind::PlusPlus => UpdateOperator::PlusPlus,
            TokenKind::MinusMinus => UpdateOperator::MinusMinus,
            _ => unreachable!(),
        };
        let span = left.span().to(self.prev_token.span());
        Spanned::new(
            Expression {
                kind: ExpressionKind::Update(Box::new(left), op, false),
            },
            span,
        )
    }

    fn parse_identifier(&mut self) -> Spanned<Expression> {
        // could be a single-arg arrow function
        if let Some(arrow) = self.try_parse_arrow_expression() {
            return arrow;
        }
        Spanned::new(
            Expression {
                kind: ExpressionKind::Identifier(Symbol::intern(
                    self.prev_token.source_string(self.src()),
                )),
            },
            self.prev_token.span(),
        )
    }

    fn parse_this(&mut self) -> Spanned<Expression> {
        Spanned::new(
            Expression {
                kind: ExpressionKind::This,
            },
            self.prev_token.span(),
        )
    }

    fn parse_literal(&mut self) -> Spanned<Expression> {
        let TokenKind::Literal(lit) = self.prev_token.kind() else {
            unreachable!()
        };
        Spanned::new(
            Expression {
                kind: ExpressionKind::Literal(lit),
            },
            self.prev_token.span(),
        )
    }

    fn parse_array(&mut self) -> Spanned<Expression> {
        let start = self.prev_token.span();
        let exprs = self.parse_delimited_list(TokenKind::Comma, Self::try_parse_expression);
        self.expect(TokenKind::RBracket);
        Spanned::new(
            Expression {
                kind: ExpressionKind::Array(exprs),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_object(&mut self) -> Spanned<Expression> {
        let start = self.prev_token.span();
        let props = self.parse_delimited_list(TokenKind::Comma, Self::parse_object_prop);

        self.expect(TokenKind::RBrace);
        Spanned::new(
            Expression {
                kind: ExpressionKind::Object(props),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_object_prop(&mut self) -> Option<(Symbol, Option<Spanned<Expression>>)> {
        let ident = self.eat_ident()?;
        Some((
            ident,
            self.eat(TokenKind::Colon).then(|| self.parse_expression()),
        ))
    }

    fn parse_member(&mut self, left: Spanned<Expression>) -> Spanned<Expression> {
        let start = self.prev_token.span();
        let key = match self.prev_token.kind() {
            TokenKind::Dot => MemberKey::Static(self.expect_ident()),
            TokenKind::LBracket => {
                let rule = self.get_rule(self.prev_token);
                let right = self.parse_expression_precedence(rule.precedence.next());
                MemberKey::Computed(Box::new(right))
            }
            _ => unreachable!(),
        };
        Spanned::new(
            Expression {
                kind: ExpressionKind::Member(Box::new(left), key),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_call(&mut self, left: Spanned<Expression>) -> Spanned<Expression> {
        let start = left.span();
        let args = self.parse_delimited_list(TokenKind::Comma, Self::try_parse_expression);
        self.expect(TokenKind::RParen);
        Spanned::new(
            Expression {
                kind: ExpressionKind::Call(Box::new(left), args),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_new(&mut self) -> Spanned<Expression> {
        let start = self.prev_token.span();
        let target = self.parse_expression_precedence(Precedence::CALL);

        Spanned::new(
            Expression {
                kind: ExpressionKind::New(Box::new(target)),
            },
            start.to(self.prev_token.span()),
        )
    }

    fn parse_function_expression(&mut self) -> Spanned<Expression> {
        let start = self.prev_token.span();
        let name = self.eat_ident();
        self.expect(TokenKind::LParen);
        let (params, rest) = self.parse_function_params();
        self.expect(TokenKind::RParen);
        self.expect(TokenKind::LBrace);
        let body = std::iter::from_fn(|| self.parse_statement()).collect();
        self.expect(TokenKind::RBrace);
        let span = start.to(self.prev_token.span());
        Spanned::new(
            Expression {
                kind: ExpressionKind::Function(Spanned::new(
                    Function {
                        name,
                        params,
                        rest,
                        body,
                    },
                    span,
                )),
            },
            span,
        )
    }

    fn parse_grouping(&mut self) -> Spanned<Expression> {
        if let Some(expr) = self.try_parse_arrow_expression() {
            return expr;
        }
        let expr = self.parse_expression_precedence(Precedence::COMMA);
        self.expect(TokenKind::RParen);
        expr
    }

    fn try_parse_arrow_expression(&mut self) -> Option<Spanned<Expression>> {
        // non-parenthesised form
        let span = self.prev_token.span();
        let (params, rest) = if let TokenKind::Ident = self.prev_token.kind() {
            let arg = self.intern(self.prev_token);
            if !self.eat(TokenKind::Arrow) {
                return None;
            }
            (vec![FunctionParam::Normal(arg)], None)
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
        let body = self
            .parse_statement()
            .expect("arrow functions require a body");
        let span = span.to(self.prev_token.span());
        Some(Spanned::new(
            Expression {
                kind: ExpressionKind::Arrow(Spanned::new(
                    Function {
                        name: None,
                        params,
                        rest,
                        body: vec![body],
                    },
                    span,
                )),
            },
            span,
        ))
    }

    /// Parses function params without parens. Also returns the `rest` param if there was one
    fn parse_function_params(&mut self) -> (Vec<FunctionParam>, Option<Symbol>) {
        let params = self.parse_delimited_list(TokenKind::Comma, Self::parse_function_param);
        let rest = self.eat(TokenKind::DotDotDot).then(|| self.expect_ident());
        (params, rest)
    }

    fn parse_function_param(&mut self) -> Option<FunctionParam> {
        let ident = self.eat_ident()?;
        if self.eat(TokenKind::Equals) {
            Some(FunctionParam::Defaulted(ident, self.parse_expression()))
        } else {
            Some(FunctionParam::Normal(ident))
        }
    }

    fn parse_assign_expression(&mut self, left: Spanned<Expression>) -> Spanned<Expression> {
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
            panic!("left-hand side of assignment must be variable")
        };
        let right = self.parse_expression_precedence(Precedence::ASSIGNMENT.next());
        let span = left.span().to(right.span());
        Spanned::new(
            Expression {
                kind: ExpressionKind::Assignment(var, op, Box::new(right)),
            },
            span,
        )
    }

    fn parse_ternary(&mut self, left: Spanned<Expression>) -> Spanned<Expression> {
        let consequent = Box::new(self.parse_expression_precedence(Precedence::TERNARY.next()));
        self.expect(TokenKind::Colon);
        let alternate = Box::new(self.parse_expression_precedence(Precedence::TERNARY.next()));
        let span = left.span().to(self.prev_token.span());
        Spanned::new(
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
    const CALL: Self = Self(17);
    const GROUPING: Self = Self(18);
}

type PrefixFn<'b> = for<'a> fn(&'a mut Parser<'b>) -> Spanned<Expression>;
type InfixFn<'b> = for<'a> fn(&'a mut Parser<'b>, Spanned<Expression>) -> Spanned<Expression>;
#[derive(Default, Debug)]
struct ParseRule<'b> {
    prefix: Option<PrefixFn<'b>>,
    infix: Option<InfixFn<'b>>,
    precedence: Precedence,
}
