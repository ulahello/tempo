use math;

export type buffer = struct {
	items: []f32,
	start: u16,
	end: u16,
};

export fn new(capacity: u16) buffer = {
	return buffer {
		items = alloc([math::NAN...], capacity),
		start = 0,
		end = 0,
	};
};

export fn buffer_free(buf: *buffer) void = {
	free(buf.items);
};

export fn length(buf: *const buffer) u16 = buf.end - buf.start;
export fn capacity(buf: *const buffer) u16 = len(buf.items): u16;
export fn is_empty(buf: *const buffer) bool = buf.start == buf.end;
export fn is_full(buf: *const buffer) bool = length(buf) == capacity(buf);

export fn get(buf: *const buffer, index: u16) f32 = {
	assert(index < length(buf), "index out of bounds");
	return buf.items[mask(buf, buf.start + index)];
};

export fn push(buf: *buffer, elem: f32) void = {
	if (is_full(buf)) {
		pop(buf);
	};
	push_strict(buf, elem);
};

export fn push_strict(buf: *buffer, elem: f32) void = {
	assert(!is_full(buf));
	buf.items[mask(buf, buf.end)] = elem;
	buf.end += 1;
};

export fn pop(buf: *buffer) f32 = {
	assert(!is_empty(buf));
	const src = mask(buf, buf.start);
	const elem = buf.items[src];
	buf.items[src] = math::NAN;
	buf.start += 1;
	return elem;
};

export fn clear(buf: *buffer) void = {
	buf.end = buf.start;
};

export fn truncate_back(buf: *buffer, new_len: u16) void = {
	if (new_len < length(buf)) {
		buf.start = buf.end - new_len;
	};
};

fn mask(buf: *const buffer, idx: u16) u16 = idx % capacity(buf);
