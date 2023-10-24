/*
pub struct NonEmptyStack<T> {
    inner: Vec<T>,
}

impl<T> NonEmptyStack<T> {
    pub fn new(initial: T) -> Self {
        Self {
            inner: vec![initial],
        }
    }

    pub fn size(&self) -> usize {
        debug_assert!(!self.inner.is_empty());
        self.inner.len()
    }

    pub fn push(&mut self, value: T) {
        self.inner.push(value)
    }

    pub fn try_pop(&mut self) -> Option<T> {
        if self.size() > 1 {
            self.inner.pop()
        } else {
            None
        }
    }

    #[track_caller]
    pub fn pop(&mut self) -> T {
        self.try_pop().expect("pop would make stack empty")
    }

    pub fn last(&self) -> &T {
        debug_assert!(self.size() >= 1);
        // SAFETY: Constructor creates with one value, and `try_pop`
        // won't remove a value if there's only 1 in the stack.
        unsafe { self.inner.last().unwrap_unchecked() }
    }

    pub fn last_mut(&mut self) -> &mut T {
        debug_assert!(self.size() >= 1);
        // SAFETY: Constructor creates with one value, and `try_pop`
        // won't remove a value if there's only 1 in the stack.
        unsafe { self.inner.last_mut().unwrap_unchecked() }
    }
}
*/
