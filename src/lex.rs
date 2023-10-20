use std::str::Chars;

use crate::Span;

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum TokenKind {
    Ident,
    Keyword(Keyword),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Literal {
    Integer(u128),
    String(String),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Keyword {
    Function,
    Return,
    If,
    Else,
    While,
    For,
    Break,
    Continue,
    Var,
    Let,
    Const,
    Debugger,
    This,
    TypeOf,
    New,
    Yield,
}

fn string_to_kw(s: &str) -> Option<Keyword> {
    use Keyword::*;
    let kw = match s {
        "function" => Function,
        "return" => Return,
        "if" => If,
        "while" => While,
        "for" => For,
        "break" => Break,
        "continue" => Continue,
        "var" => Var,
        "let" => Let,
        "const" => Const,
        "debugger" => Debugger,
        "this" => This,
        "typeof" => TypeOf,
        "new" => New,
        "yield" => Yield,
        "else" => Else,
        _ => {
            return None;
        }
    };
    Some(kw)
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
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

    pub fn kind(&self) -> &TokenKind {
        &self.kind
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
                let mut ident = c.to_string();
                while let Some(c) = self.try_eat_fn(|c| c.is_ascii_alphanumeric() || c == '_') {
                    ident.push(c);
                }
                string_to_kw(&ident).map_or(TokenKind::Ident, TokenKind::Keyword)
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
                TokenKind::Literal(Literal::String(content))
            }

            p => unreachable!("couldn't parse at `{p}`"),
        };
        Some(Token {
            kind,
            span: Span(start, self.position()),
        })
    }
}
