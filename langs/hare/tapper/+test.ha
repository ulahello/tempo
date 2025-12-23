use math;
use io;
use memio;
use ringbuf;

def EPS: f32 = 0.001;

fn eps_eq(x: f32, y: f32) bool = {
	return math::absf32(x - y) < EPS;
};

fn eps_eq_buf(lhs: const *ringbuf::buffer, rhs: []const f32) bool = {
	if (ringbuf::length(lhs) != len(rhs)) {
		return false;
	};
	for (let i: u16 = 0; i < ringbuf::length(lhs); i += 1) {
		if (!eps_eq(ringbuf::get(lhs, i), rhs[i])) {
			return false;
		};
	};
	return true;
};

fn tostr(t: *const tapper) str = {
	static let buf: [1024]u8 = [0...];
	let h = memio::fixed(&buf);
	format(&h, t)!;
	return memio::string(&h)!;
};

@test fn display() void = {
	let t = new(10, true);
	for (let bpm .. [120.05f32, 112.41, 121.105]) {
		ringbuf::push(&t.buffer, bpm);
	};

        assert("[121.1, 112.4, 120.1]" == tostr(&t));

	ringbuf::clear(&t.buffer);
	ringbuf::push(&t.buffer, 112.76);
        assert("[112.8]" == tostr(&t));

	ringbuf::clear(&t.buffer);
        assert("[]" == tostr(&t));
};

@test fn is_recording() void = {
	let t = new(10, true);
	assert(!is_recording(&t));
	tap(&t);
	assert(is_recording(&t));
	tap(&t);
	assert(is_recording(&t));
	clear(&t);
	assert(!is_recording(&t));
};

@test fn bpm() void = {
	let t = new(10, true);
	assert(eps_eq(0.0, bpm(&t)));
	for (let (elem, new_avg) .. [
		(23.0f32, 23.0f32),
		(26.0, 24.5),
		(29.0, 26.0),
		(61.0, 34.75)]) {
		ringbuf::push(&t.buffer, elem);
		assert(eps_eq(new_avg, bpm(&t)));
	};
};

@test fn tap() void = {
	let t = new(3, true);
	assert(count(&t) == 0);
	tap(&t);
	assert(count(&t) == 0);
	tap(&t);
	assert(count(&t) == 1);
	tap(&t);
	assert(count(&t) == 2);
	tap(&t);
	assert(count(&t) == 3);
	tap(&t);
	assert(count(&t) == 3);
	toggle_bounded(&t);
	tap(&t);
	assert(count(&t) == 4);
	tap(&t);
	assert(count(&t) == 5);
	toggle_bounded(&t);
	assert(count(&t) == 3);
};

@test fn tap() void = {
	let t = new(3, true);
	for (let bpm .. [80.0f32, 70.0, 60.0]) {
		ringbuf::push(&t.buffer, bpm);
	};
	assert(eps_eq_buf(&t.buffer, [80.0, 70.0, 60.0]));

	ringbuf::push(&t.buffer, 50.0);
	assert(eps_eq_buf(&t.buffer, [80.0, 70.0, 60.0, 50.0]));

	sync_cap(&t);
	assert(eps_eq_buf(&t.buffer, [70.0, 60.0, 50.0]));

	toggle_bounded(&t);
	ringbuf::push(&t.buffer, 40.0);
	assert(eps_eq_buf(&t.buffer, [70.0, 60.0, 50.0, 40.0]));

	sync_cap(&t);
	assert(eps_eq_buf(&t.buffer, [70.0, 60.0, 50.0, 40.0]));

	toggle_bounded(&t);
	assert(eps_eq_buf(&t.buffer, [60.0, 50.0, 40.0]));
};

@test fn tap() void = {
	let t = new(3, true);
	for (let bpm .. [80.0f32, 70.0, 60.0]) {
		ringbuf::push(&t.buffer, bpm);
	};
	assert(eps_eq_buf(&t.buffer, [80.0, 70.0, 60.0]));
	resize(&t, 2);
	assert(eps_eq_buf(&t.buffer, [70.0, 60.0]));
	assert(capacity(&t) == 2);
};
