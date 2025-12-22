use fmt;
use io;
use math;
use ringbuf;
use strconv;
use time;

def CLOCK = time::clock::MONOTONIC;
def BPM_PRECISION: uint = 1;
def MAX_CAPACITY: u16 = 0x1000;

export type tapper = struct {
	buffer: ringbuf::buffer,
	capacity: u16,
	bounded: bool,
	prev_tap: (time::instant | void),
};

export fn new(capacity: u16, bounded: bool) tapper = {
	return tapper {
		buffer = ringbuf::new(MAX_CAPACITY),
		capacity = capacity,
		bounded = bounded,
		prev_tap = void,
	};
};

export fn tapper_free(self: *tapper) void = {
	ringbuf::buffer_free(&self.buffer);
};

export fn tap(self: *tapper) void = {
	const now = time::now(CLOCK);

	match (self.prev_tap) {
	case void => void;
	case let prev: time::instant =>
		let elapsed_nanos = time::diff(prev, now);
		let elapsed_secs: f32 = (elapsed_nanos: f32) / (time::SECOND: f32);

		let bpm = 60.0 / elapsed_secs;
		ringbuf::push(&self.buffer, bpm);

		sync_cap(self);
	};

	self.prev_tap = now;
};

export fn clear(self: *tapper) void = {
	ringbuf::clear(&self.buffer);
	self.prev_tap = void;
};

export fn resize(self: *tapper, new_cap: u16) void = {
	self.capacity = new_cap;
	sync_cap(self);
};

export fn toggle_bounded(self: *tapper) void = {
	self.bounded ^^= true;
	sync_cap(self);
};

export fn bpm(self: *const tapper) f32 = {
	let avg: f32 = 0.0;
	for (let idx: u16 = 0; idx < ringbuf::length(&self.buffer); idx += 1) {
		const sample = ringbuf::get(&self.buffer, idx);
		const div = idx + 1;
		avg += (sample - avg) / (div: f32);
	};
	return avg;
};

export fn count(self: *const tapper) u16 = ringbuf::length(&self.buffer);

export fn capacity(self: *const tapper) u16 = {
	if (self.capacity < ringbuf::capacity(&self.buffer))
		return self.capacity
	else
		return ringbuf::capacity(&self.buffer);
};

export fn is_recording(self: *const tapper) bool = self.prev_tap is time::instant;
export fn is_bounded(self: *const tapper) bool = self.bounded;

export fn format_sample(h: io::handle, s: f32) (void | io::error) = {
	strconv::fftosf(h, s, strconv::ffmt::F, BPM_PRECISION, strconv::fflags::NONE)?;
};

export fn format(h: io::handle, self: *const tapper) (void | io::error) = {
	let length = ringbuf::length(&self.buffer);
	fmt::fprint(h, "[")?;
	for (let idx: u16 = 1; idx <= length; idx += 1) {
		const sample = ringbuf::get(&self.buffer, length - idx);
		format_sample(h, sample)?;
		if (idx < length) {
			fmt::fprint(h, ", ")?;
		};
	};
	fmt::fprint(h, "]")?;
};

fn sync_cap(self: *tapper) void = {
	if (self.bounded)
		ringbuf::truncate_back(&self.buffer, capacity(self));
};
