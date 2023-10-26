use bumpalo::Bump;
use core::fmt;
use fxhash::FxBuildHasher;
use once_cell::sync::Lazy;
use std::{
    collections::HashMap,
    fmt::{Debug, Display},
    sync::Mutex,
};

use crate::lex::kw::SYMBOL_VALUES;

pub static INTERNER: Lazy<Mutex<Interner>> = Lazy::new(|| Mutex::new(Interner::new()));

#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(pub usize);

impl PartialOrd for Symbol {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Symbol {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.as_str().cmp(other.as_str())
    }
}

impl Symbol {
    pub fn as_str(self) -> &'static str {
        let Ok(interner) = INTERNER.lock() else {
            return "?";
        };
        interner.strings.get(self.0).copied().unwrap_or("?")
    }

    pub fn intern(s: &str) -> Symbol {
        INTERNER.lock().unwrap().intern(s)
    }

    pub fn is_keyword(self) -> bool {
        self.0 < crate::lex::kw::N_KEYWORDS
    }
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

impl Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(self.as_str(), f)
    }
}

#[derive(Default)]
pub struct Interner<'a> {
    arena: Bump,
    strings: Vec<&'a str>,
    names: HashMap<&'a str, Symbol, FxBuildHasher>,
}

impl<'a> Interner<'a> {
    /// Private as it does not initialize the preinterned strings
    /// Allocates cap * 8 bytes for the arena
    fn with_capacity(cap: usize) -> Self {
        Self {
            arena: Bump::with_capacity(cap * 8),
            strings: Vec::with_capacity(cap),
            names: HashMap::with_capacity_and_hasher(cap, FxBuildHasher::default()),
        }
    }

    pub fn new() -> Self {
        let mut s = Self::with_capacity(SYMBOL_VALUES.len());
        for kw in SYMBOL_VALUES {
            s.intern(kw);
        }
        s
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&sym) = self.names.get(s) {
            return sym;
        }
        let s = self.arena.alloc_str(s);
        // SAFETY: We will only hand out &'a string references which cannot be used
        // by the time the internet and therefore arena have been deallocated
        let s: &str = unsafe { &*(s as *const _) };
        let sym = Symbol(self.strings.len());
        self.strings.push(s);
        self.names.insert(s, sym);
        sym
    }
}
