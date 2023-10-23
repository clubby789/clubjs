use crate::span::{SourceMap, Span};
use std::{fmt::Display, sync::OnceLock};

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

    /// Reports an error message, including a highltighted snippet
    /// of the offending code, then exits.
    pub fn report_fatal_error<M: Display>(&self, msg: M, span: Span) -> ! {
        let sm = self.sourcemap();
        let src = sm.render_source_span(span, 1);
        eprintln!(
            "fatal error: {msg}:\n --> {}:{}:{}\n{src}",
            sm.path().display(),
            sm.lookup_line(span.lo()),
            sm.lookup_col(span.lo()),
        );
        std::process::exit(1);
    }
}

/// Reports an error message, including a highltighted snippet
/// of the offending code, then exits.
pub fn report_fatal_error<M: Display>(msg: M, span: Span) -> ! {
    if let Some(sess) = crate::SESSION.get() {
        sess.report_fatal_error(msg, span)
    } else {
        eprintln!("fatal error: {msg}");
        std::process::exit(1);
    }
}
