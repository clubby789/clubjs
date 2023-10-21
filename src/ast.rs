use crate::{
    intern::Symbol,
    lex::{kw, Lexer, Token, TokenKind},
    HalfSpan, Span,
};

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Program {
    span: Span,
    body: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Function {
    span: Span,
    name: Symbol,
    params: Vec<Symbol>,
    defaults: Vec<Expression>,
    rest: Option<Symbol>,
    body: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclaration {
    span: Span,
    declarations: Vec<VariableDeclarator>,
    kind: VariableKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VariableKind {
    Var,
    Let,
    Const,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct VariableDeclarator {
    span: Span,
    name: Symbol,
    init: Option<Expression>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Statement {
    span: Span,
    kind: StatementKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum StatementKind {
    Empty,
    Debugger,
    Block(Vec<Statement>),
    Expression(Expression),
    If(Expression, Box<Statement>, Option<Box<Statement>>),
    Labeled(Symbol, Box<Statement>),
    Break(Option<Symbol>),
    Continue(Option<Symbol>),
    Switch(Expression, Vec<SwitchCase>),
    Return(Option<Expression>),
    Throw(Expression),
    Try(Vec<Statement>, Option<CatchClause>, Option<Vec<Statement>>),
    While(Expression, Box<Statement>),
    DoWhile(Box<Statement>, Expression),
    For(Option<ForInit>, Option<Expression>, Option<Expression>),
    ForOf(Option<ForInit>, Expression, Box<Statement>, bool),
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(Function),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Expression {
    span: Span,
    kind: ExpressionKind,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ExpressionKind {
    This,
    Array(Vec<Expression>),
    Object(Vec<(Symbol, Option<Expression>)>),
    Function {
        name: Option<Symbol>,
        params: Vec<Symbol>,
        defaults: Vec<Expression>,
        rest: Option<Symbol>,
        body: Vec<Statement>,
    },
    Arrow {
        params: Vec<Symbol>,
        defaults: Vec<Expression>,
        rest: Option<Symbol>,
        body: Vec<Statement>,
    },
    Unary(UnaryOperator, Box<Expression>),
    Binary(Box<Expression>, BinaryOperator, Box<Expression>),
    Assignment(Symbol, AssignmentOperator, Box<Expression>),
    /// the bool is true if the operator is a prefix
    Update(Box<Expression>, UpdateOperator, bool),
    Logical(Box<Expression>, LogicalOperator, Box<Expression>),
    Ternary(Box<Expression>, Box<Expression>, Box<Expression>),
    New(Box<Expression>),
    Call(Box<Expression>, Vec<Expression>),
    Member(Box<Expression>, MemberKey),
    Yield(Option<Box<Expression>>),
    Literal(crate::lex::Literal),
    Identifier(Symbol),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum MemberKey {
    Static(Symbol),
    Computed(Box<Expression>),
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum UnaryOperator {
    Plus,
    Minus,
    Bang,
    Tilde,
    Typeof,
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
    DotDot,
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
    span: Span,
    test: Option<Expression>,
    consequent: Statement,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct CatchClause {
    span: Span,
    param: Option<Symbol>,
    block: Vec<Statement>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ForInit {
    VariableDeclaration(VariableDeclaration),
    Expression(Expression),
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
        let token = lexer.next_token().unwrap_or_default();
        Self {
            lexer,
            prev_token: Token::default(),
            token,
        }
    }

    pub fn src(&self) -> &str {
        self.lexer.src()
    }

    pub fn parse(mut self) -> Program {
        let span = Span(0, self.src().len());
        let body = std::iter::from_fn(|| self.parse_statement()).collect();
        Program { span, body }
    }

    fn last_token_end(&self) -> usize {
        self.prev_token.span().hi()
    }

    /// Advance to the next token, replacing prev_token with token
    /// Returns the old value of `prev_token`
    fn advance(&mut self) -> Token {
        let tmp = std::mem::replace(&mut self.token, self.lexer.next_token().unwrap_or_default());
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
    /// If all were consumed, returns each prev_token value
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
        assert!(
            self.eat(kind),
            "expected `{kind:?}`, found `{:?}`",
            self.token.kind()
        );
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

    fn parse_statement(&mut self) -> Option<Statement> {
        if matches!(self.token.kind(), TokenKind::Eof | TokenKind::RBrace) {
            return None;
        }
        let span = HalfSpan(self.last_token_end());
        if self.eat(TokenKind::Semicolon) {
            return Some(Statement {
                span: span.finish(self.last_token_end()),
                kind: StatementKind::Empty,
            });
        }
        if self.eat_symbol(kw::Debugger) {
            self.eat(TokenKind::Semicolon);
            return Some(Statement {
                span: span.finish(self.last_token_end()),
                kind: StatementKind::Debugger,
            });
        }

        if self.eat(TokenKind::LBrace) {
            let block = std::iter::from_fn(|| self.parse_statement()).collect();
            self.expect(TokenKind::RBrace);
            return Some(Statement {
                span: span.finish(self.last_token_end()),
                kind: StatementKind::Block(block),
            });
        }

        if self.eat_symbol(kw::If) {
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
            return Some(Statement {
                span: span.finish(self.last_token_end()),
                kind: StatementKind::If(expr, Box::new(content), alternate.map(Box::new)),
            });
        }

        if self.eat_symbol(kw::While) {
            self.expect(TokenKind::LParen);
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            let content = self
                .parse_statement()
                .expect("while statements must have a block");

            return Some(Statement {
                span: span.finish(self.last_token_end()),
                kind: StatementKind::While(expr, Box::new(content)),
            });
        }

        if self.eat_symbol(kw::Do) {
            let content = self
                .parse_statement()
                .expect("do/while statements must have a body");
            if !self.eat_symbol(kw::While) {
                panic!("expected `while` after `do`")
            }
            self.expect(TokenKind::LParen);
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            return Some(Statement {
                span: span.finish(self.last_token_end()),
                kind: StatementKind::DoWhile(Box::new(content), expr),
            });
        }

        if self.eat_symbol(kw::For) {
            self.expect(TokenKind::LParen);
            let expr = self.parse_expression();
            self.expect(TokenKind::RParen);
            let content = self
                .parse_statement()
                .expect("while statements must have a block");

            return Some(Statement {
                span: span.finish(self.last_token_end()),
                kind: StatementKind::While(expr, Box::new(content)),
            });
        }

        if let Some(tokens) = self.eat_many(&[TokenKind::Ident, TokenKind::Colon]) {
            let stmt = self
                .parse_statement()
                .expect("label must be followed by a statement");
            assert_eq!(tokens.len(), 2);
            let ident = Symbol::intern(tokens[1].source_string(self.src()));
            return Some(Statement {
                span: span.finish(stmt.span.hi()),
                kind: StatementKind::Labeled(ident, Box::new(stmt)),
            });
        }

        if self.eat_symbol(kw::Break) {
            let ident = self.eat_ident();
            self.eat(TokenKind::Semicolon);
            return Some(Statement {
                span: span.finish(self.prev_token.span().hi()),
                kind: StatementKind::Break(ident),
            });
        }

        if self.eat_symbol(kw::Continue) {
            let ident = self.eat_ident();
            self.eat(TokenKind::Semicolon);
            return Some(Statement {
                span: span.finish(self.prev_token.span().hi()),
                kind: StatementKind::Continue(ident),
            });
        }

        if self.eat_symbol(kw::Return) {
            let expr = self.try_parse_expression();
            self.eat(TokenKind::Semicolon);
            return Some(Statement {
                span: span.finish(self.prev_token.span().hi()),
                kind: StatementKind::Return(expr),
            });
        }

        if self.eat_symbol(kw::Throw) {
            let expr = self.parse_expression();
            self.eat(TokenKind::Semicolon);
            return Some(Statement {
                span: span.finish(self.prev_token.span().hi()),
                kind: StatementKind::Throw(expr),
            });
        }

        if self.eat_symbol(kw::Function) {
            let name = self.expect_ident();
            self.expect(TokenKind::LParen);
            let mut params = vec![];
            while let Some(ident) = self.eat_ident() {
                params.push(ident);
                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
            self.expect(TokenKind::RParen);
            self.expect(TokenKind::LBrace);
            let body = std::iter::from_fn(|| self.parse_statement()).collect();
            self.expect(TokenKind::RBrace);
            let span = span.finish(self.last_token_end());
            return Some(Statement {
                span,
                kind: StatementKind::FunctionDeclaration(Function {
                    span,
                    name,
                    params,
                    defaults: vec![],
                    rest: None,
                    body,
                }),
            });
        }

        for (kw, kind) in [
            (kw::Var, VariableKind::Var),
            (kw::Let, VariableKind::Let),
            (kw::Const, VariableKind::Const),
        ] {
            if !self.eat_symbol(kw) {
                continue;
            }
            let mut decls = vec![];
            loop {
                let sp = self.token.span();
                let name = self.expect_ident();
                let init = self
                    .eat(TokenKind::Equals)
                    .then(|| self.parse_expression_precedence(Precedence::COMMA));
                decls.push(VariableDeclarator {
                    span: sp.to(self.prev_token.span()),
                    name,
                    init,
                });
                if !self.eat(TokenKind::Comma) {
                    break;
                }
            }
            let decl_span = span.finish(self.last_token_end());
            self.eat(TokenKind::Semicolon);
            let stmt_span = decl_span.to(self.prev_token.span());
            return Some(Statement {
                span: stmt_span,
                kind: StatementKind::VariableDeclaration(VariableDeclaration {
                    span: decl_span,
                    declarations: decls,
                    kind,
                }),
            });
        }

        if self.eat_symbol(kw::Switch) {
            self.expect(TokenKind::LParen);
            let scrutinee = self.parse_expression();
            self.expect(TokenKind::RParen);
            self.expect(TokenKind::LBrace);
            let mut cases = vec![];
            let mut default = false;
            loop {
                let span = self.token.span();
                let test = if self.eat_symbol(kw::Default) {
                    if default {
                        panic!("more than one `default` clause in a switch statement");
                    }
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
                    Statement {
                        span: self.token.span().shrink_to_lo(),
                        kind: StatementKind::Empty,
                    }
                } else {
                    self.parse_statement().unwrap_or_else(|| Statement {
                        span: self.token.span().shrink_to_lo(),
                        kind: StatementKind::Empty,
                    })
                };
                cases.push(SwitchCase {
                    span: span.to(self.prev_token.span()),
                    test,
                    consequent,
                });
            }
            self.expect(TokenKind::RBrace);
            return Some(Statement {
                span: span.finish(self.last_token_end()),
                kind: StatementKind::Switch(scrutinee, cases),
            });
        }

        if self.eat_symbol(kw::Try) {
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
                catch = Some(CatchClause {
                    span: span.to(self.prev_token.span()),
                    param,
                    block,
                })
            }

            if self.eat_symbol(kw::Finally) {
                self.expect(TokenKind::LBrace);
                let block = std::iter::from_fn(|| self.parse_statement()).collect();
                self.expect(TokenKind::RBrace);
                finally = Some(block);
            }

            return Some(Statement {
                span: span.finish(self.last_token_end()),
                kind: StatementKind::Try(block, catch, finally),
            });
        }

        let kind = self
            .try_parse_expression()
            .map_or(StatementKind::Empty, StatementKind::Expression);
        self.eat(TokenKind::Semicolon);
        Some(Statement {
            span: span.finish(self.prev_token.span().hi()),
            kind,
        })
    }

    fn parse_expression(&mut self) -> Expression {
        self.parse_expression_precedence(Precedence::TERNARY)
    }

    fn try_parse_expression(&mut self) -> Option<Expression> {
        self.try_parse_expression_precedence(Precedence::TERNARY)
    }

    #[track_caller]
    fn parse_expression_precedence(&mut self, prec: Precedence) -> Expression {
        self.try_parse_expression_precedence(prec)
            .expect("expected expression")
    }

    fn try_parse_expression_precedence(&mut self, prec: Precedence) -> Option<Expression> {
        self.advance();
        let rule = self.get_rule(self.prev_token);
        let mut expr = rule.prefix?(self);
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
            TokenKind::Bang | TokenKind::Tilde /*| TokenKind::Keyword(Keyword::TypeOf)*/ => {
                r!(Self::parse_unary, _, UNARY)
            }
            TokenKind::Plus | TokenKind::Minus => r!(Self::parse_unary, Self::parse_binary, UNARY),
            TokenKind::Slash => r!(_, Self::parse_binary, FACTOR),
            TokenKind::Period => r!(_, Self::parse_member, MEMBER),
            TokenKind::Ident => match self.intern(token) {
                kw::TypeOf => r!(Self::parse_unary, _, UNARY),
                kw::This => r!(Self::parse_this, _),
                kw::New => r!(Self::parse_new, _, NEW),
                kw::Function => r!(Self::parse_function_expression, _),
                kw::Case|kw::Default => r!(),
                #[cfg(debug_assertions)]
                sym if kw::KEYWORD_NAMES.contains(&sym.as_str()) => panic!("parsing keyword {sym:?} as identifier"),
                _ => r!(Self::parse_identifier, _),
            }
            TokenKind::Literal(_) => r!(Self::parse_literal, _),
            TokenKind::LBracket => r!(Self::parse_array, Self::parse_member, MEMBER),
            TokenKind::LBrace => r!(Self::parse_object, _),
            TokenKind::Comma | TokenKind::RBracket | TokenKind::RBrace => r!(),
            TokenKind::LParen => r!(Self::parse_expression, Self::parse_call, GROUPING),
            TokenKind::RParen => r!(),
            TokenKind::Semicolon | TokenKind::Colon | TokenKind::Eof => r!(),
            k => todo!("`{k:?}`"),
        }
    }

    fn parse_unary(&mut self) -> Expression {
        let op = match self.prev_token.kind() {
            TokenKind::Plus => UnaryOperator::Plus,
            TokenKind::Minus => UnaryOperator::Minus,
            c => todo!("`{c:?}`"),
        };
        let op_span = self.prev_token.span();
        let expr = self.parse_expression_precedence(Precedence::UNARY);
        Expression {
            span: op_span.to(expr.span),
            kind: ExpressionKind::Unary(op, Box::new(expr)),
        }
    }

    fn parse_binary(&mut self, left: Expression) -> Expression {
        let op = match self.prev_token.kind() {
            TokenKind::Plus => BinaryOperator::Plus,
            TokenKind::Minus => BinaryOperator::Minus,
            TokenKind::Slash => BinaryOperator::Divide,
            t => todo!("{t:?}"),
        };
        let rule = self.get_rule(self.prev_token);
        let right = self.parse_expression_precedence(rule.precedence.next());
        Expression {
            span: left.span.to(right.span),
            kind: ExpressionKind::Binary(Box::new(left), op, Box::new(right)),
        }
    }

    fn parse_identifier(&mut self) -> Expression {
        Expression {
            span: self.prev_token.span(),
            kind: ExpressionKind::Identifier(Symbol::intern(
                self.prev_token.source_string(self.src()),
            )),
        }
    }

    fn parse_this(&mut self) -> Expression {
        Expression {
            span: self.prev_token.span(),
            kind: ExpressionKind::This,
        }
    }

    fn parse_literal(&mut self) -> Expression {
        let TokenKind::Literal(lit) = self.prev_token.kind() else {
            unreachable!()
        };
        Expression {
            span: self.prev_token.span(),
            kind: ExpressionKind::Literal(lit),
        }
    }

    fn parse_array(&mut self) -> Expression {
        let start = self.prev_token.span();
        let mut exprs = vec![];
        while let Some(expr) = self.try_parse_expression_precedence(Precedence::TERNARY) {
            exprs.push(expr);
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::RBracket);
        Expression {
            span: start.to(self.prev_token.span()),
            kind: ExpressionKind::Array(exprs),
        }
    }

    fn parse_object(&mut self) -> Expression {
        let start = self.prev_token.span();
        let mut props = vec![];
        while let Some(ident) = self.eat_ident() {
            let value = self.eat(TokenKind::Colon).then(|| self.parse_expression());
            props.push((ident, value));
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::RBrace);
        Expression {
            span: start.to(self.prev_token.span()),
            kind: ExpressionKind::Object(props),
        }
    }

    fn parse_member(&mut self, left: Expression) -> Expression {
        let start = self.prev_token.span();
        let key = match self.prev_token.kind() {
            TokenKind::Period => MemberKey::Static(self.expect_ident()),
            TokenKind::LBracket => {
                let rule = self.get_rule(self.prev_token);
                let right = self.parse_expression_precedence(rule.precedence.next());
                MemberKey::Computed(Box::new(right))
            }
            _ => unreachable!(),
        };
        Expression {
            span: start.to(self.prev_token.span()),
            kind: ExpressionKind::Member(Box::new(left), key),
        }
    }

    fn parse_call(&mut self, left: Expression) -> Expression {
        let start = left.span;
        let mut args = vec![];
        while let Some(expr) = self.try_parse_expression_precedence(Precedence::TERNARY) {
            args.push(expr);
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::RParen);
        Expression {
            span: start.to(self.prev_token.span()),
            kind: ExpressionKind::Call(Box::new(left), args),
        }
    }

    fn parse_new(&mut self) -> Expression {
        let start = self.prev_token.span();
        let target = self.parse_expression_precedence(Precedence::CALL);

        Expression {
            span: start.to(self.prev_token.span()),
            kind: ExpressionKind::New(Box::new(target)),
        }
    }

    fn parse_function_expression(&mut self) -> Expression {
        let start = self.prev_token.span();
        let name = self.eat_ident();
        self.expect(TokenKind::LParen);
        let mut params = vec![];
        while let Some(ident) = self.eat_ident() {
            params.push(ident);
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        self.expect(TokenKind::RParen);
        self.expect(TokenKind::LBrace);
        let body = std::iter::from_fn(|| self.parse_statement()).collect();
        self.expect(TokenKind::RBrace);
        Expression {
            span: start.to(self.prev_token.span()),
            kind: ExpressionKind::Function {
                name,
                params,
                defaults: vec![],
                rest: None,
                body,
            },
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
    // const ASSIGNMENT: Self = Self(2);
    // const LOGICAL_OR: Self = Self(3);
    // const EQUALITY: Self = Self(8);
    // const LT_GT: Self = Self(9);
    // const SHIFT: Self = Self(10);
    // const ADDITION: Self = Self(11);
    const FACTOR: Self = Self(12);
    const UNARY: Self = Self(14);
    // const POSTFIX: Self = Self(15);
    const MEMBER: Self = Self(17);
    const NEW: Self = Self(17);
    const CALL: Self = Self(17);
    const GROUPING: Self = Self(18);
}

type PrefixFn<'b> = for<'a> fn(&'a mut Parser<'b>) -> Expression;
type InfixFn<'b> = for<'a> fn(&'a mut Parser<'b>, Expression) -> Expression;
#[derive(Default, Debug)]
struct ParseRule<'b> {
    prefix: Option<PrefixFn<'b>>,
    infix: Option<InfixFn<'b>>,
    precedence: Precedence,
}
