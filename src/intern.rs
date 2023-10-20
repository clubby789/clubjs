use core::fmt;
use once_cell::sync::Lazy;
use std::{collections::HashMap, fmt::Debug, sync::Mutex};
use typed_arena::Arena;

pub static INTERNER: Lazy<Mutex<Interner>> = Lazy::new(|| Mutex::new(Interner::new()));

#[derive(Clone, Copy)]
pub struct Symbol(usize);

impl Symbol {
    pub fn as_str(self) -> &'static str {
        let Ok(interner) = INTERNER.lock() else {
            return "?";
        };
        interner.strings.get(self.0).copied().unwrap_or("?")
    }
    //pub fn as_str_specific<'a>(self, interner: &'a Interner) -> &'a str {
    //    interner.strings.get(self.0).unwrap()
    //}
}

impl Debug for Symbol {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        fmt::Debug::fmt(self.as_str(), f)
    }
}

#[derive(Default)]
pub struct Interner<'a> {
    arena: Arena<u8>,
    strings: Vec<&'a str>,
    names: HashMap<&'a str, Symbol>,
}

impl<'a> Interner<'a> {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&sym) = self.names.get(s) {
            return sym;
        }
        let s = self.arena.alloc_str(s);
        // SAFETY: We will only hand out &'a string references which cannot be used
        // by the time the internet and therefore arena have been deallocated
        let s: &str = unsafe { std::mem::transmute(s) };
        let sym = Symbol(s.len());
        self.strings.push(s);
        self.names.insert(s, sym);
        sym
    }
}
