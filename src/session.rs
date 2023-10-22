use crate::span::SourceMap;
use std::sync::OnceLock;

pub static SESSION: OnceLock<Session> = OnceLock::new();

#[derive(Debug)]
pub struct Session {
    sourcemap: SourceMap,
}

impl Session {
    pub fn new(sourcemap: SourceMap) -> Self {
        Session { sourcemap }
    }

    pub fn sourcemap(&self) -> &SourceMap {
        &self.sourcemap
    }
}
