mod ast;
mod lex;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Default)]
struct Span(usize, usize);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
struct HalfSpan(usize);

impl HalfSpan {
    pub fn finish(self, end: usize) -> Span {
        Span(self.0, end)
    }
}

impl Span {
    pub fn lo(self) -> usize {
        self.0
    }

    pub fn hi(self) -> usize {
        self.1
    }

    pub fn to(self, other: Span) -> Self {
        Self(self.0, self.1.max(other.hi()))
    }
}

fn main() {
    let src = std::fs::read_to_string("src.js").unwrap();
    let p = ast::Parser::new(&src);
    println!("{:#?}", p.parse());
}
