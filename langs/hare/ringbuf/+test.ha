use types;

fn eq_slice(lhs: *const buffer, rhs: []f32) bool = {
	if (length(lhs) != len(rhs)) {
		return false;
	};
	for (let i: u16 = 0; i < length(lhs); i += 1) {
		if (get(lhs, i) != rhs[i]) {
			return false;
		};
	};
	return true;
};

@test fn push_pop() void = {
	let buf = new(8);
	for (let i: u16 = 0; i < capacity(&buf); i += 1) {
		push(&buf, i: f32);
	};
	assert(eq_slice(&buf, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]));
	assert(is_full(&buf));

	assert(pop_unchecked(&buf) == 0.0);
	assert(pop_unchecked(&buf) == 1.0);
	assert(pop_unchecked(&buf) == 2.0);
	assert(eq_slice(&buf, [3.0, 4.0, 5.0, 6.0, 7.0]));
};

@test fn push_clobber() void = {
	let buf = new(8);
	for (let i: u16 = 0; i < capacity(&buf); i += 1) {
		push(&buf, i: f32);
	};
	assert(eq_slice(&buf, [0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0]));

	push(&buf, 24.0);
	assert(eq_slice(&buf, [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 24.0]));
};

@test fn overflow() void = {
	let buf = new(8);
	for (let i: u16 = 0; i < types::U16_MAX; i += 1) {
		push(&buf, 4.0);
		push(&buf, 9.0);
		push(&buf, 16.0);
		assert(length(&buf) == 3);
		assert(pop_unchecked(&buf) == 4.0);
		assert(pop_unchecked(&buf) == 9.0);
		assert(pop_unchecked(&buf) == 16.0);
	};
	assert(is_empty(&buf));
};

@test fn clear() void = {
	let buf = new(8);
	for (let i: u16 = 0; i < 100; i += 1) {
		push(&buf, i: f32 * 1.0);
		push(&buf, i: f32 * 2.0);
		push(&buf, i: f32 * 3.0);
		pop_unchecked(&buf);
	};
	assert(!is_empty(&buf));
	clear(&buf);
	assert(is_empty(&buf));
};

@test fn truncate_back() void = {
	let buf = new(8);
	for (let i: u16 = 0; i < capacity(&buf); i += 1) {
		push(&buf, i: f32);
	};
	truncate_back(&buf, 3);
	assert(eq_slice(&buf, [5.0, 6.0, 7.0]));
	truncate_back(&buf, 100);
	assert(eq_slice(&buf, [5.0, 6.0, 7.0]));
};
