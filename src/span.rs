use std::fmt::Debug;

#[derive(Copy, Clone, PartialEq, Eq, Default)]
pub struct Span {
    start: usize,
    len: usize,
}

impl Span {
    pub fn lo(self) -> usize {
        self.start
    }

    pub fn hi(self) -> usize {
        self.start + self.len()
    }

    pub fn len(self) -> usize {
        self.len
    }

    pub fn to(self, other: Span) -> Self {
        debug_assert!(other.start >= self.start);
        Self {
            start: self.start,
            len: (other.start - self.start) + other.len,
        }
    }

    pub fn shrink_to_lo(self) -> Self {
        Self { len: 0, ..self }
    }

    pub fn shrink_to_hi(self) -> Self {
        Self {
            start: self.hi(),
            len: 0,
        }
    }

    pub fn new(start: usize, len: usize) -> Self {
        Self { start, len }
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(sm) = crate::SOURCE_MAP.get() {
            write!(
                f,
                "({}:{} - {}:{})",
                sm.lookup_line(self.lo()),
                sm.lookup_col(self.lo()),
                sm.lookup_line(self.hi()),
                sm.lookup_col(self.hi())
            )
        } else {
            f.debug_struct("Span")
                .field("start", &self.start)
                .field("len", &self.len)
                .finish()
        }
    }
}

#[derive(Debug)]
pub struct SourceMap {
    /// Byte positions of newlines in source file, in order
    lines: Vec<usize>,
}

impl SourceMap {
    pub fn from_src(src: &str) -> SourceMap {
        Self {
            lines: src
                .bytes()
                .enumerate()
                .filter_map(|(i, c)| (c == b'\n').then_some(i))
                .collect(),
        }
    }

    pub fn lookup_line(&self, bp: usize) -> usize {
        self.lines.partition_point(|&p| p < bp) + 1
    }

    pub fn lookup_col(&self, bp: usize) -> usize {
        let line = self.lines.partition_point(|&p| p < bp);
        let line_start = if line > 0 {
            self.lines[line - 1] + 1
        } else {
            0
        };
        (bp - line_start) + 1
    }
}
