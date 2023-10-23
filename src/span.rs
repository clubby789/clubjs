use std::fmt::{Debug, Write};

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

    pub fn is_empty(self) -> bool {
        self.len() == 0
    }
}

impl Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(sess) = crate::SESSION.get() {
            let sm = sess.sourcemap();
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
    source: String,
}

impl SourceMap {
    pub fn from_src(source: String) -> Self {
        Self {
            lines: source
                .bytes()
                .enumerate()
                .filter_map(|(i, c)| (c == b'\n').then_some(i))
                .collect(),
            source,
        }
    }

    /// Get the line number of a given byte position in the source, starting at 1
    pub fn lookup_line(&self, bp: usize) -> usize {
        self.lines.partition_point(|&p| p < bp) + 1
    }

    /// Get the column number of a given byte position in the source, starting at 1
    pub fn lookup_col(&self, bp: usize) -> usize {
        let line = self.lines.partition_point(|&p| p < bp);
        let line_start = if line > 0 {
            self.lines[line - 1] + 1
        } else {
            0
        };
        (bp - line_start) + 1
    }

    /*
    /// Get the source line containing the given byte position
    pub fn get_line_with_pos(&self, bp: usize) -> &str {
        let line = self.lookup_line(bp);
        let start = self.lines[line];
        let end = self
            .lines
            .get(line + 1)
            .copied()
            .unwrap_or(self.source.len());
        &self.source[start..end]
    }
    */

    /// Get the nth source line, starting at 1
    pub fn get_nth_line(&self, line: usize) -> &str {
        let idx = line.checked_sub(1).expect("source lines start at 1");
        let start = if let Some(prev) = idx.checked_sub(1) {
            self.lines[prev] + 1
        } else {
            0
        };
        let end = self.lines.get(idx).copied().unwrap_or(self.source.len());
        &self.source[start..end]
    }

    /// Returns the lines of source containing the selected span,
    /// with the span itself highlighted
    pub fn render_source_span(&self, sp: Span) -> String {
        let start_line = self.lookup_line(sp.lo());
        let end_line = self.lookup_line(sp.hi());

        if start_line == end_line {
            let content = self.get_nth_line(start_line);
            let start = self
                .lookup_col(sp.lo())
                .checked_sub(1)
                .expect("columns start at 1");
            if sp.is_empty() {
                return format!("{content}\n{}^", " ".repeat(start));
            } else {
                let len = self.source[sp.lo()..sp.hi()].chars().count();
                return format!("{content}\n{}{}", " ".repeat(start), "~".repeat(len));
            }
        }
        let mut rendered = String::new();
        for line in start_line..=end_line {
            if !rendered.is_empty() {
                rendered.push('\n');
            }
            let content = self.get_nth_line(line);
            rendered.push_str(content);
            if line == start_line {
                let skip = self
                    .lookup_col(sp.lo())
                    .checked_sub(1)
                    .expect("columns start at 1");
                let len = content[skip..].chars().count();
                write!(rendered, "\n{}{}", " ".repeat(skip), "~".repeat(len)).unwrap();
            } else if line == end_line {
                let len = self
                    .lookup_col(sp.hi())
                    .checked_sub(1)
                    .expect("columns start at 1");
                let len = content[..len].chars().count();
                write!(rendered, "\n{}", "~".repeat(len)).unwrap();
            } else {
                write!(rendered, "\n{}", "~".repeat(content.chars().count())).unwrap();
            }
        }
        rendered
    }
}

/// Generic wrapper to add spans and IDs to types to avoid adding a getter to every node
#[derive(Clone, Copy, Debug, Eq)]
pub struct Node<T> {
    id: usize,
    item: T,
    span: Span,
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl<T> Node<T> {
    pub fn new(id: usize, item: T, span: Span) -> Self {
        Self { id, item, span }
    }

    pub fn item(&self) -> &T {
        &self.item
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn map<F, U>(self, f: F) -> Node<U>
    where
        F: FnOnce(T) -> U,
    {
        Node {
            id: self.id,
            span: self.span,
            item: f(self.item),
        }
    }
}

impl<T> std::ops::Deref for Node<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        self.item()
    }
}
