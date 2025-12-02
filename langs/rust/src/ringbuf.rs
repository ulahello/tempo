use core::{
    fmt,
    mem::{self, MaybeUninit},
};

pub type Idx = u16;

// https://www.snellman.net/blog/archive/2016-12-13-ring-buffers
#[derive(Clone)]
pub struct RingBuf<T: Copy, const CAP: usize> {
    mem: [MaybeUninit<T>; CAP],
    start: Idx,
    end: Idx,
}

impl<T: Copy, const CAP: usize> RingBuf<T, CAP> {
    pub const fn new() -> Self {
        assert!(CAP.is_power_of_two(), "capacity must be a power of two");
        assert!(
            CAP <= Idx::MAX as usize / 2,
            "capacity must be at most half of the max index"
        );
        Self {
            mem: [MaybeUninit::uninit(); CAP],
            start: 0,
            end: 0,
        }
    }

    pub const fn len(&self) -> Idx {
        self.end.wrapping_sub(self.start)
    }

    pub const fn is_empty(&self) -> bool {
        self.start == self.end
    }

    pub const fn is_full(&self) -> bool {
        self.len() as usize == CAP
    }

    pub fn try_push(&mut self, elem: T) -> Result<(), T> {
        if self.is_full() {
            return Err(elem);
        }
        /* SAFETY: we checked that the buffer is not full before pushing */
        unsafe { self.push_unchecked(elem) };
        Ok(())
    }

    pub fn push(&mut self, elem: T) {
        if self.is_full() {
            /* SAFETY: we checked that the buffer is not empty before
             * popping (full implies not empty, because capacity of 0 is not
             * valid) */
            unsafe { self.pop_unchecked() };
        }
        /* SAFETY: if the buffer was full, it's not anymore because we
         * popped an element */
        unsafe { self.push_unchecked(elem) };
    }

    #[allow(dead_code)] // used by tests
    pub fn pop(&mut self) -> Option<T> {
        if self.is_empty() {
            return None;
        }
        /* SAFETY: we checked that the buffer is not empty before popping */
        let elem = unsafe { self.pop_unchecked() };
        Some(elem)
    }

    pub fn clear(&mut self) {
        // @drop
        self.end = self.start;
    }

    #[allow(dead_code)] // used by tests
    pub fn truncate_front(&mut self, new_len: Idx) {
        if new_len < self.len() {
            // @drop
            self.end = self.start.wrapping_add(new_len);
        }
    }

    pub fn truncate_back(&mut self, new_len: Idx) {
        if new_len < self.len() {
            // @drop
            self.start = self.end.wrapping_sub(new_len);
        }
    }

    pub const fn iter(&self) -> Iter<'_, T, CAP> {
        Iter {
            mem: &self.mem,
            start: self.start,
            end: self.end,
        }
    }
}

impl<T: Copy, const CAP: usize> RingBuf<T, CAP> {
    /// Push `elem` to the back of the ring buffer.
    ///
    /// # Safety
    ///
    /// The buffer must not be full.
    unsafe fn push_unchecked(&mut self, elem: T) {
        debug_assert!(!self.is_full());
        /* SAFETY: Self::mask returns a valid index into self.mem */
        let dst: &mut MaybeUninit<T> = self.mem.get_unchecked_mut(Self::mask(self.end));
        dst.write(elem);
        self.end = self.end.wrapping_add(1);
    }

    /// Pop `elem` from the front of the ring buffer.
    ///
    /// # Safety
    ///
    /// The buffer must not be empty.
    unsafe fn pop_unchecked(&mut self) -> T {
        debug_assert!(!self.is_empty());
        /* SAFETY: Self::mask returns a valid index into self.mem */
        let src: &mut MaybeUninit<T> = self.mem.get_unchecked_mut(Self::mask(self.start));
        self.start = self.start.wrapping_add(1);
        /* SAFETY: element is initialized under the assumption that self.start
         * and self.end are correctly manipulated */
        mem::replace(src, MaybeUninit::uninit()).assume_init()
    }
}

impl<T: Copy, const CAP: usize> RingBuf<T, CAP> {
    const fn mask(val: Idx) -> usize {
        val as usize % CAP
    }
}

impl<T: Copy + fmt::Debug, const CAP: usize> fmt::Debug for RingBuf<T, CAP> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut dbg = f.debug_list();
        for elem in self.iter() {
            dbg.entry(elem);
        }
        dbg.finish()
    }
}

impl<T: Copy, const CAP: usize> FromIterator<T> for RingBuf<T, CAP> {
    fn from_iter<I: IntoIterator<Item = T>>(iter: I) -> Self {
        let mut buf = Self::new();
        for elem in iter {
            let result = buf.try_push(elem);
            assert!(result.is_ok(), "iterator exceeds RingBuf capacity");
        }
        buf
    }
}

impl<const N: usize, T: Copy + PartialEq, const CAP: usize> PartialEq<[T; N]> for RingBuf<T, CAP> {
    fn eq(&self, other: &[T; N]) -> bool {
        self.len() as usize == other.len() && self.iter().zip(other).all(|(a, b)| a == b)
    }
}

pub struct Iter<'a, T: Copy, const CAP: usize> {
    mem: &'a [MaybeUninit<T>; CAP],
    start: Idx,
    end: Idx,
}

impl<'a, T: Copy, const CAP: usize> Iterator for Iter<'a, T, CAP> {
    type Item = &'a T;

    fn next(&mut self) -> Option<Self::Item> {
        if self.start == self.end {
            return None;
        }
        /* SAFETY: RingBuf::mask returns a valid index into self.mem */
        let elem: &MaybeUninit<T> =
            unsafe { self.mem.get_unchecked(RingBuf::<T, CAP>::mask(self.start)) };
        self.start = self.start.wrapping_add(1);
        /* SAFETY: element is initialized under the assumption that self.start
         * and self.end are correctly manipulated */
        unsafe { Some(elem.assume_init_ref()) }
    }
}

impl<T: Copy, const CAP: usize> DoubleEndedIterator for Iter<'_, T, CAP> {
    fn next_back(&mut self) -> Option<Self::Item> {
        if self.start == self.end {
            return None;
        }
        self.end = self.end.wrapping_sub(1);
        /* SAFETY: RingBuf::mask returns a valid index into self.mem */
        let elem: &MaybeUninit<T> =
            unsafe { self.mem.get_unchecked(RingBuf::<T, CAP>::mask(self.end)) };
        /* SAFETY: element is initialized under the assumption that self.start
         * and self.end are correctly manipulated */
        unsafe { Some(elem.assume_init_ref()) }
    }
}
