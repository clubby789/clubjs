use std::{
    cell::OnceCell,
    fmt::{Debug, Display, Formatter, Write},
};

use super::Opcode;

pub type BasicBlockIndex = usize;

#[derive(Debug)]
pub struct BasicBlock<TemporaryKind> {
    ops: Vec<Opcode<TemporaryKind>>,
    terminator: OnceCell<Terminator>,
}

impl<TemporaryKind> BasicBlock<TemporaryKind> {
    pub fn new() -> Self {
        Self {
            ops: vec![],
            terminator: OnceCell::new(),
        }
    }

    pub fn set_terminator(&self, terminator: Terminator) {
        self.terminator.set(terminator).unwrap_or_else(|t| {
            panic!(
                "terminator already set ({} -> {t})",
                self.terminator.get().unwrap()
            )
        })
    }

    pub fn try_set_terminator(&self, terminator: Terminator) -> Result<(), Terminator> {
        self.terminator.set(terminator)
    }

    pub fn add_op(&mut self, op: Opcode<TemporaryKind>) {
        self.ops.push(op)
    }
}

impl<TemporaryKind: Display + Debug> Display for BasicBlock<TemporaryKind> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        for op in &self.ops {
            write!(f, "    {op};\n")?;
        }
        if let Some(t) = self.terminator.get() {
            write!(f, "    {t};")
        } else {
            write!(f, "    <no terminator>;")
        }
    }
}

#[derive(Debug)]
pub enum Terminator {
    /// Always jumps to the given [`BasicBlock`]
    Unconditional(BasicBlockIndex),
    /// Jumps to [`yes`] if the accumulator value is truthy,
    /// otherwise to [`no`]
    Conditional {
        yes: BasicBlockIndex,
        no: BasicBlockIndex,
    },
    /// Returns from the current function - if [`value`] is set, return the contents
    /// of the accumulator
    Return {
        value: bool,
    },
    Unreachable,
}

impl Display for Terminator {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Terminator::Unconditional(block) => write!(f, "goto {block}"),
            Terminator::Conditional { yes, no } => {
                write!(f, "if (acc) {{ goto {yes} }} else {{ goto {no} }}")
            }
            Terminator::Return { value: true } => f.write_str("return acc"),
            Terminator::Return { value: false } => f.write_str("return"),
            Terminator::Unreachable => f.write_str("unreachable"),
        }
    }
}

#[derive(Debug)]
pub struct ControlFlowGraph<TemporaryKind> {
    blocks: Vec<BasicBlock<TemporaryKind>>,
}

impl<TemporaryKind> Default for ControlFlowGraph<TemporaryKind> {
    fn default() -> Self {
        Self { blocks: vec![] }
    }
}

impl<TemporaryKind: Display + Debug> Display for ControlFlowGraph<TemporaryKind> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        f.write_str("{\n")?;
        for (i, bb) in self.blocks.iter().enumerate() {
            write!(f, "block_{i}:\n{bb}\n")?;
        }
        f.write_char('}')
    }
}

impl<TemporaryKind> ControlFlowGraph<TemporaryKind> {
    pub fn new() -> Self {
        Self {
            blocks: Default::default(),
        }
    }

    pub fn new_block(&mut self) -> BasicBlockIndex {
        let idx = self.blocks.len();
        self.blocks.push(BasicBlock::new());
        idx
    }

    /// Same as [`new_block`], but adds a `goto` terminator to the
    /// previous block
    pub fn new_successor_block(&mut self) -> BasicBlockIndex {
        let idx = self.blocks.len();
        self.current_block()
            .set_terminator(Terminator::Unconditional(idx));
        self.blocks.push(BasicBlock::new());
        idx
    }

    pub fn add_op(&mut self, op: Opcode<TemporaryKind>) {
        self.current_block().ops.push(op);
    }

    pub fn get_block(&mut self, index: BasicBlockIndex) -> &mut BasicBlock<TemporaryKind> {
        &mut self.blocks[index]
    }

    pub fn current_block(&mut self) -> &mut BasicBlock<TemporaryKind> {
        self.blocks.last_mut().expect("no current blocks")
    }

    pub fn current_idx(&mut self) -> BasicBlockIndex {
        self.blocks.len().checked_sub(1).expect("no current blocks")
    }
}
