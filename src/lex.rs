use std::str::Chars;

use crate::{intern::Symbol, session::report_fatal_error, span::Span};

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
    AsteriskEquals,
    Equals,
    EqualsEquals,
    EqualsEqualsEquals,
    BangEquals,
    BangEqualsEquals,
    Arrow,
    Gt,
    GtEquals,
    GtGtEquals,
    GtGt,
    GtGtGt,
    GtGtGtEquals,
    Lt,
    LtEquals,
    LtLtEquals,
    LtLt,
    Bang,
    Tilde,
    Dot,
    DotDot,
    DotDotDot,
    Plus,
    PlusPlus,
    PlusEquals,
    Minus,
    MinusMinus,
    MinusEquals,
    Slash,
    SlashEquals,
    Percent,
    PercentEquals,
    Caret,
    CaretEquals,
    Bar,
    BarBar,
    BarEquals,
    And,
    AndAnd,
    AndEquals,
    Question,
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

    clubjs_macros::preintern! {
        keywords {
            Function: "function",
            Return: "return",
            If: "if",
            Else: "else",
            While: "while",
            For: "for",
            Break: "break",
            Continue: "continue",
            Var: "var",
            Let: "let",
            Const: "const",
            Debugger: "debugger",
            This: "this",
            TypeOf: "typeof",
            New: "new",
            Switch: "switch",
            Case: "case",
            Default: "default",
            Throw: "throw",
            Try: "try",
            Catch: "catch",
            Finally: "finally",
            Do: "do",
            Of: "of",
            In: "in",
            InstanceOf: "instanceof",
            Delete: "delete",
        }
        globalThis,
        Infinity,
        NaN,
        undefined,
        console_log,
    }
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

    fn try_eat_str(&mut self, s: &str) -> bool {
        let mut c = self.chars.clone();
        for chr in s.chars() {
            if c.next() != Some(chr) {
                return false;
            }
        }
        self.chars = c;
        true
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

    fn skip_whitespace(&mut self) -> bool {
        let mut skipped = false;
        while self.peek().map_or(false, char::is_whitespace) {
            skipped = true;
            self.chars.next();
        }
        skipped
    }

    fn skip_comments(&mut self) -> bool {
        let start = self.position();
        let mut skipped = false;
        if self.try_eat_str("//") {
            skipped = true;
            loop {
                match self.chars.next() {
                    Some('\n') | None => break,
                    _ => (),
                }
            }
        } else if self.try_eat_str("/*") {
            skipped = true;
            loop {
                match self.chars.next() {
                    Some('*') if self.try_eat('/') => break,
                    None => {
                        report_fatal_error(
                            "expected a `*/`",
                            Span::new(start, self.position() - start),
                        );
                    }
                    _ => (),
                }
            }
        }
        skipped
    }

    pub fn next_token(&mut self) -> Token {
        while self.skip_whitespace() || self.skip_comments() {}

        let start = self.position();
        let Some(next) = self.chars.next() else {
            return Token {
                kind: TokenKind::Eof,
                span: Span::new(start, 0),
            };
        };
        let kind = match next {
            '[' => TokenKind::LBracket,
            ']' => TokenKind::RBracket,
            '(' => TokenKind::LParen,
            ')' => TokenKind::RParen,
            '{' => TokenKind::LBrace,
            '}' => TokenKind::RBrace,
            ',' => TokenKind::Comma,
            ':' => TokenKind::Colon,
            ';' => TokenKind::Semicolon,
            '*' if self.try_eat('=') => TokenKind::AsteriskEquals,
            '*' => TokenKind::Asterisk,
            '.' if self.try_eat_str("==") => TokenKind::EqualsEqualsEquals,
            '=' if self.try_eat('=') => TokenKind::EqualsEquals,
            '=' if self.try_eat('>') => TokenKind::Arrow,
            '=' => TokenKind::Equals,
            '>' if self.try_eat_str(">>=") => TokenKind::GtGtGtEquals,
            '>' if self.try_eat_str(">=") => TokenKind::GtGtEquals,
            '>' if self.try_eat_str(">>") => TokenKind::GtGtGt,
            '>' if self.try_eat('>') => TokenKind::GtGt,
            '>' if self.try_eat('=') => TokenKind::GtEquals,
            '>' => TokenKind::Gt,
            '<' if self.try_eat_str("<=") => TokenKind::LtLtEquals,
            '<' if self.try_eat('<') => TokenKind::LtLt,
            '<' if self.try_eat('=') => TokenKind::LtEquals,
            '<' => TokenKind::Lt,
            '!' if self.try_eat_str("==") => TokenKind::BangEqualsEquals,
            '!' if self.try_eat('=') => TokenKind::BangEquals,
            '!' => TokenKind::Bang,
            '?' => TokenKind::Question,
            '~' => TokenKind::Tilde,
            '.' if self.try_eat_str("..") => TokenKind::DotDotDot,
            '.' if self.try_eat('.') => TokenKind::DotDot,
            '.' => TokenKind::Dot,
            '+' if self.try_eat('+') => TokenKind::PlusPlus,
            '+' if self.try_eat('=') => TokenKind::PlusEquals,
            '+' => TokenKind::Plus,
            '-' if self.try_eat('-') => TokenKind::MinusMinus,
            '-' if self.try_eat('=') => TokenKind::MinusEquals,
            '-' => TokenKind::Minus,
            '/' if self.try_eat('=') => TokenKind::SlashEquals,
            '/' => TokenKind::Slash,
            '%' if self.try_eat('=') => TokenKind::PercentEquals,
            '%' => TokenKind::Percent,
            '^' if self.try_eat('=') => TokenKind::CaretEquals,
            '^' => TokenKind::Caret,
            '|' if self.try_eat('|') => TokenKind::BarBar,
            '|' if self.try_eat('=') => TokenKind::BarEquals,
            '|' => TokenKind::Bar,
            '&' if self.try_eat('&') => TokenKind::AndAnd,
            '&' if self.try_eat('=') => TokenKind::AndEquals,
            '&' => TokenKind::And,
            c if c.is_ascii_digit() => {
                let mut n = u128::from(c as u8 - b'0');
                while let Some(c) = self.try_eat_fn(|c| c.is_ascii_digit()) {
                    n = (n * 10) + u128::from(c as u8 - b'0');
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
                assert!(self.try_eat(c), "unterminated string literal");
                TokenKind::Literal(Literal::String(Symbol::intern(&content)))
            }

            p => {
                report_fatal_error(
                    format!("could not parse `{}`", p.escape_unicode()),
                    Span::new(start, self.position() - start),
                );
            }
        };
        Token {
            kind,
            span: Span::new(start, self.position() - start),
        }
    }
}
