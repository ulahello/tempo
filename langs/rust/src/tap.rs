use core::fmt;
use std::time::Instant;

use crate::ringbuf::RingBuf;

#[derive(Clone, Debug)]
pub struct Tapper {
    pub(crate) buf: RingBuf<f32, { Self::MAX_CAP as usize }>,
    cap: u16,
    bounded: bool,
    last_tap: Option<Instant>,
}

impl Tapper {
    pub const MAX_CAP: u16 = 0x1000;

    pub const fn new(cap: u16, bounded: bool) -> Self {
        Self {
            buf: RingBuf::new(),
            cap,
            bounded,
            last_tap: None,
        }
    }

    pub fn tap(&mut self) {
        let now = Instant::now();

        // update bpm
        if let Some(last) = self.last_tap.replace(now) {
            let elapsed = now.saturating_duration_since(last).as_secs_f32();

            // push a new bpm
            let bpm = 60.0 / elapsed;
            self.buf.push(bpm);

            // remove old elements
            self.sync_cap();
        }
    }

    pub fn clear(&mut self) {
        self.buf.clear();
        self.last_tap = None;
    }

    pub fn resize(&mut self, new_cap: u16) {
        self.cap = new_cap;
        self.sync_cap();
    }

    pub fn toggle_bounded(&mut self) {
        self.bounded ^= true;
        self.sync_cap();
    }

    #[allow(clippy::cast_precision_loss)]
    pub fn bpm(&self) -> f32 {
        // https://www.nu42.com/2015/03/how-you-average-numbers.html
        let mut avg = 0.0;
        for (idx, bpm) in self.buf.iter().enumerate() {
            avg += (bpm - avg) / (idx + 1) as f32;
        }
        avg
    }

    pub const fn count(&self) -> u16 {
        self.buf.len()
    }

    /// Return the capacity of the buffer when bounded.
    pub fn capacity(&self) -> u16 {
        self.cap.min(Self::MAX_CAP)
    }

    pub const fn is_recording(&self) -> bool {
        self.last_tap.is_some()
    }

    pub const fn is_bounded(&self) -> bool {
        self.bounded
    }

    pub(crate) fn sync_cap(&mut self) {
        if self.bounded {
            self.buf.truncate_back(self.capacity());
        }
    }
}

impl fmt::Display for Tapper {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let mut dbg = f.debug_list();
        for bpm in self.buf.iter().rev() {
            dbg.entry(bpm);
        }
        dbg.finish()
    }
}
