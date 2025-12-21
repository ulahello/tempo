use math;

export def MAX_CAP: u16 = 0x1000;

export type buffer = struct {
	items: [MAX_CAP]f32,
	start: u16,
	end: u16,
};

export fn new() buffer = {
	return buffer {
		items = [math::NAN...],
		start = 0,
		end = 0,
	};
};

export fn length(buf: *const buffer) u16 = buf.end - buf.start;
export fn is_empty(buf: *const buffer) bool = buf.start == buf.end;
export fn is_full(buf: *const buffer) bool = length(buf) == MAX_CAP;

export fn get(buf: *const buffer, index: u16) f32 = {
	assert(index < length(buf), "index out of bounds");
	return buf.items[mask(buf.start + index)];
};

export fn push(buf: *buffer, elem: f32) void = {
	if (is_full(buf)) {
		pop_unchecked(buf);
	};
	push_unchecked(buf, elem);
};

export fn clear(buf: *buffer) void = {
	buf.end = buf.start;
};

export fn truncate_back(buf: *buffer, new_len: u16) void = {
	if (new_len < length(buf)) {
		buf.start = buf.end - new_len;
	};
};

fn push_unchecked(buf: *buffer, elem: f32) void = {
	assert(!is_full(buf));
	buf.items[mask(buf.end)] = elem;
	buf.end += 1;
};

fn pop_unchecked(buf: *buffer) f32 = {
	assert(!is_empty(buf));
	const src = mask(buf.start);
	const elem = buf.items[src];
	buf.items[src] = math::NAN;
	buf.start += 1;
	return elem;
};

fn mask(idx: u16) u16 = idx % MAX_CAP;
