use std::str::Chars;

use crate::{intern::Symbol, Span};

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub enum TokenKind {
    Ident,
    Literal(Literal),
    LBracket,
    RBracket,
    LParen,
    RParen,
    LBrace,
    RBrace,
    Comma,
    Colon,
    Semicolon,
    Asterisk,
    Equals,
    EqualsEquals,
    Arrow,
    Gt,
    GtE,
    Lt,
    LtE,
    Bang,
    Tilde,
    BangEquals,
    Period,
    Plus,
    Minus,
    Slash,
    #[default]
    Eof,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(u128),
    String(Symbol),
}

#[allow(non_upper_case_globals)]
pub mod kw {
    use crate::intern::Symbol;
    pub const Function: Symbol = Symbol(0);
    pub const Return: Symbol = Symbol(1);
    pub const If: Symbol = Symbol(2);
    pub const Else: Symbol = Symbol(3);
    pub const While: Symbol = Symbol(4);
    pub const For: Symbol = Symbol(5);
    pub const Break: Symbol = Symbol(6);
    pub const Continue: Symbol = Symbol(7);
    pub const Var: Symbol = Symbol(8);
    pub const Let: Symbol = Symbol(9);
    pub const Const: Symbol = Symbol(10);
    pub const Debugger: Symbol = Symbol(11);
    pub const This: Symbol = Symbol(12);
    pub const TypeOf: Symbol = Symbol(13);
    pub const New: Symbol = Symbol(14);
    pub const Switch: Symbol = Symbol(15);
    pub const Case: Symbol = Symbol(16);
    pub const Default: Symbol = Symbol(17);
    pub static KEYWORD_NAMES: &[&str] = &[
        "function", "return", "if", "else", "while", "for", "break", "continue", "var", "let",
        "const", "debugger", "this", "typeof", "new", "switch", "case", "default",
    ];
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl Token {
    pub fn source_string<'a>(&self, src: &'a str) -> &'a str {
        &src[self.span.lo()..self.span.hi()]
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn kind(&self) -> TokenKind {
        self.kind
    }
}

#[derive(Clone)]
pub struct Lexer<'a> {
    src: &'a str,
    chars: Chars<'a>,
}

impl<'a> Lexer<'a> {
    pub fn new(src: &'a str) -> Self {
        Self {
            src,
            chars: src.chars(),
        }
    }

    pub fn src(&self) -> &str {
        self.src
    }

    fn position(&self) -> usize {
        self.src.len() - self.chars.as_str().len()
    }

    fn peek(&self) -> Option<char> {
        self.chars.clone().next()
    }

    /// Attempts to consume the character 'c' - returns true if it was
    /// consumed.
    fn try_eat(&mut self, c: char) -> bool {
        if self.chars.clone().next() == Some(c) {
            self.chars.next();
            true
        } else {
            false
        }
    }

    /// Attempts to consume a character matching the predicate f
    fn try_eat_fn<F: FnOnce(char) -> bool>(&mut self, f: F) -> Option<char> {
        let c = self.chars.clone().next()?;
        if f(c) {
            self.chars.next();
            Some(c)
        } else {
            None
        }
    }

    pub fn next_token(&mut self) -> Option<Token> {
        while self.peek().map_or(false, char::is_whitespace) {
            self.chars.next();
        }
        let start = self.position();
        let kind = match self.chars.next()? {
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            '*' => TokenKind::Asterisk,
            '=' if self.try_eat('=') => TokenKind::EqualsEquals,
            '=' if self.try_eat('>') => TokenKind::Arrow,
            '=' => TokenKind::Equals,
            '>' if self.try_eat('=') => TokenKind::GtE,
            '>' => TokenKind::Gt,
            '<' if self.try_eat('=') => TokenKind::LtE,
            '<' => TokenKind::Lt,
            '!' if self.try_eat('=') => TokenKind::BangEquals,
            '!' => TokenKind::Bang,
            '~' => TokenKind::Tilde,
            '.' => TokenKind::Period,
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '/' => TokenKind::Slash,
            c if c.is_ascii_digit() => {
                let mut n = (c as u8 - b'0') as u128;
                while let Some(c) = self.try_eat_fn(|c| c.is_ascii_digit()) {
                    n = (n * 10) + (c as u8 - b'0') as u128;
                }
                TokenKind::Literal(Literal::Integer(n))
            }
            c if c.is_ascii_alphabetic() || c == '_' => {
                while self
                    .try_eat_fn(|c| c.is_ascii_alphanumeric() || c == '_')
                    .is_some()
                {}
                TokenKind::Ident
            }
            c @ ('"' | '\'') => {
                // TODO: handle escaped quotes
                let mut content = String::new();
                while let Some(chr) = self.try_eat_fn(|chr| chr != c) {
                    content.push(chr);
                }
                if !self.try_eat(c) {
                    panic!("unterminated string literal");
                }
                TokenKind::Literal(Literal::String(Symbol::intern(&content)))
            }

            p => unreachable!("couldn't parse at `{p}`"),
        };
        Some(Token {
            kind,
            span: Span(start, self.position()),
        })
    }
}
