#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
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
