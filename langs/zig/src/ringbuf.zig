// https://www.snellman.net/blog/archive/2016-12-13-ring-buffers/

const std = @import("std");

const assert = std.debug.assert;
const math = std.math;

pub const Idx = u16;

fn validateCap(comptime cap: Idx) error{ CapacityNotPowerOfTwo, CapacityTooLarge }!void {
    if (cap == 0 or !math.isPowerOfTwo(cap)) {
        return error.CapacityNotPowerOfTwo;
    }
    if (math.maxInt(Idx) / 2 < cap) {
        return error.CapacityTooLarge;
    }
}

pub fn RingBuf(comptime T: type, comptime cap: Idx) error{ CapacityNotPowerOfTwo, CapacityTooLarge }!type {
    try validateCap(cap);

    return struct {
        mem: [cap]T,
        start: Idx,
        end: Idx,

        const Self = @This();

        pub fn init() Self {
            validateCap(cap) catch unreachable;
            const rb: Self = .{
                .mem = undefined,
                .start = 0,
                .end = 0,
            };
            assert(rb.isEmpty());
            return rb;
        }

        pub fn len(self: *const Self) Idx {
            const length = self.end -% self.start;
            assert(length <= cap);
            return length;
        }

        pub fn isEmpty(self: *const Self) bool {
            return self.start == self.end;
        }

        pub fn isFull(self: *const Self) bool {
            const is_full = self.len() == cap;
            // isFull and isEmpty are mutually exclusive
            assert(!(is_full and self.isEmpty()));
            return is_full;
        }

        pub fn get(self: *const Self, idx: Idx) ?*const T {
            if (self.len() <= idx) return null;
            // SAFETY: Self.mask return a valid index into self.mem,
            // and we check that index makes sense according to the
            // length
            assert(idx < self.len());
            return &self.mem[Self.mask(self.start +% idx)];
        }

        pub fn read(self: *const Self, idx: Idx) ?T {
            return if (self.get(idx)) |item| item.* else null;
        }

        pub fn tryPush(self: *Self, elem: T) error{Full}!void {
            if (self.isFull()) return error.Full;
            // SAFETY: we checked that the buffer is not full before
            // pushing
            self.pushUnchecked(elem);
        }

        pub fn push(self: *Self, elem: T) void {
            if (self.isFull()) {
                // SAFETY: we checked that the buffer is not empty
                // before popping (full implies not empty, because
                // capacity of 0 is not valid)
                _ = self.popUnchecked();
            }
            // SAFETY: if the buffer was full, it's not anymore
            // because we popped an element
            self.pushUnchecked(elem);
        }

        pub fn pop(self: *Self) ?T {
            if (self.isEmpty()) return null;
            // SAFETY: we checked that the buffer is not empty before
            // popping
            return self.popUnchecked();
        }

        pub fn clear(self: *Self) void {
            self.end = self.start;
            assert(self.isEmpty());
        }

        pub fn truncateFront(self: *Self, trunc_to: Idx) void {
            const new_len = @min(self.len(), trunc_to);
            self.end = self.start +% new_len;
            assert(self.len() <= new_len);
        }

        pub fn truncateBack(self: *Self, trunc_to: Idx) void {
            const new_len = @min(self.len(), trunc_to);
            self.start = self.end -% new_len;
            assert(self.len() <= new_len);
        }

        pub fn eql(self: *const Self, other: []const T) bool {
            if (self.len() != other.len) return false;
            var idx: Idx = 0;
            while (idx < self.len()) : (idx += 1) {
                const self_elem = self.read(idx).?;
                const other_elem = other[idx];
                if (self_elem != other_elem) return false;
            }
            return true;
        }

        pub fn fromSlice(self: *Self, slice: []const T) error{InsufficientCapacity}!void {
            if (cap < slice.len) {
                return error.InsufficientCapacity;
            }
            self.clear();
            for (slice) |elem| {
                self.pushUnchecked(elem);
            }
            assert(self.len() == slice.len);
        }

        /// Push `elem` to the back of the ring buffer.
        ///
        /// The buffer must not be full.
        fn pushUnchecked(self: *Self, elem: T) void {
            assert(!self.isFull());
            const old_len = self.len();
            // SAFETY: Self.mask returns a valid index into self.mem
            self.mem[Self.mask(self.end)] = elem;
            self.end +%= 1;
            assert(old_len + 1 == self.len());
        }

        /// Pop `elem` from the front of the ring buffer.
        ///
        /// The buffer must not be empty.
        fn popUnchecked(self: *Self) T {
            assert(!self.isEmpty());
            const old_len = self.len();
            // SAFETY: Self.mask returns a valid index into self.mem,
            // and element is initialized under the assumption that
            // self.start and self.end are correctly manipulated
            const elem = self.mem[Self.mask(self.start)];
            self.start +%= 1;
            assert(old_len - 1 == self.len());
            return elem;
        }

        fn mask(val: Idx) usize {
            return val % cap;
        }
    };
}

const testing = std.testing;

test "init" {
    const NOT_POW2 = [_]Idx{ 0, 3, 9, 11, 29, 65, 1023 };
    const TOO_BIG = [_]Idx{32768};
    comptime for (NOT_POW2) |cap| {
        try testing.expectError(error.CapacityNotPowerOfTwo, RingBuf(void, cap));
    };
    comptime for (TOO_BIG) |cap| {
        try testing.expectError(error.CapacityTooLarge, RingBuf(void, cap));
    };
}

test "push pop" {
    const CAP: usize = 8;
    const T = i32;
    var buf = (try RingBuf(T, CAP)).init();

    {
        var i: T = 0;
        while (i < CAP) : (i += 1) {
            try buf.tryPush(i);
        }
    }
    try testing.expect(buf.eql(&[_]T{ 0, 1, 2, 3, 4, 5, 6, 7 }));
    try testing.expectError(error.Full, buf.tryPush(24));

    try testing.expectEqual(@as(T, 0), buf.pop().?);
    try testing.expectEqual(@as(T, 1), buf.pop().?);
    try testing.expectEqual(@as(T, 2), buf.pop().?);
    try testing.expect(buf.eql(&[_]T{ 3, 4, 5, 6, 7 }));
}

test "push clobber" {
    const CAP: usize = 8;
    const T = i32;
    var buf = (try RingBuf(T, CAP)).init();

    {
        var i: T = 0;
        while (i < CAP) : (i += 1) {
            buf.push(i);
        }
    }
    try testing.expect(buf.eql(&[_]T{ 0, 1, 2, 3, 4, 5, 6, 7 }));

    buf.push(24);
    try testing.expect(buf.eql(&[_]T{ 1, 2, 3, 4, 5, 6, 7, 24 }));
}

test "overflow" {
    const CAP: usize = 8;
    const T = usize;
    var buf = (try RingBuf(T, CAP)).init();

    {
        var i: T = 0;
        while (i < math.maxInt(Idx)) : (i += 1) {
            try buf.tryPush(4);
            try buf.tryPush(9);
            try buf.tryPush(16);
            try testing.expectEqual(@as(T, 4), buf.pop().?);
            try testing.expectEqual(@as(T, 9), buf.pop().?);
            try testing.expectEqual(@as(T, 16), buf.pop().?);
        }
        try testing.expect(buf.isEmpty());
    }
}

test "clear" {
    const CAP: usize = 8;
    const T = usize;
    var buf = (try RingBuf(T, CAP)).init();

    {
        var i: T = 0;
        while (i < 100) : (i += 1) {
            buf.push(i);
            buf.push(i * 2);
            buf.push(i * 3);
            _ = buf.pop().?;
        }
    }
    try testing.expect(!buf.isEmpty());
    buf.clear();
    try testing.expect(buf.isEmpty());
    try testing.expectEqual(@as(?T, null), buf.pop());
}

test "from slice" {
    const CAP: usize = 8;
    const T = usize;
    const slice = [_]T{ 1, 2, 3, 4, 5, 6, 7, 8 };
    var buf = (try RingBuf(T, CAP)).init();
    try buf.fromSlice(&slice);
    try testing.expect(buf.isFull());
    try testing.expect(buf.eql(&slice));
}

test "from slice overflow" {
    const CAP: usize = 8;
    const T = usize;
    const slice = [_]T{ 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    var buf = (try RingBuf(T, CAP)).init();
    try testing.expectError(error.InsufficientCapacity, buf.fromSlice(&slice));
}

test "truncate front" {
    const CAP: usize = 8;
    const T = usize;
    var buf = (try RingBuf(T, CAP)).init();
    try buf.fromSlice(&[_]T{ 1, 2, 3, 4, 5, 6, 7, 8 });
    try testing.expect(buf.eql(&[_]T{ 1, 2, 3, 4, 5, 6, 7, 8 }));
    buf.truncateFront(3);
    try testing.expect(buf.eql(&[_]T{ 1, 2, 3 }));
    buf.truncateFront(3);
    try testing.expect(buf.eql(&[_]T{ 1, 2, 3 }));
    buf.truncateFront(100);
    try testing.expect(buf.eql(&[_]T{ 1, 2, 3 }));
}

test "truncate back" {
    const CAP: usize = 8;
    const T = usize;
    var buf = (try RingBuf(T, CAP)).init();
    try buf.fromSlice(&[_]T{ 1, 2, 3, 4, 5, 6, 7, 8 });
    try testing.expect(buf.eql(&[_]T{ 1, 2, 3, 4, 5, 6, 7, 8 }));
    buf.truncateBack(3);
    try testing.expect(buf.eql(&[_]T{ 6, 7, 8 }));
    buf.truncateBack(100);
    try testing.expect(buf.eql(&[_]T{ 6, 7, 8 }));
}

test "eql" {
    const CAP: usize = 8;
    const T = usize;
    const slice = [_]T{ 1, 2, 3, 4, 5, 6, 7, 8 };
    var buf = (try RingBuf(T, CAP)).init();
    try buf.fromSlice(&slice);
    try testing.expect(buf.eql(&slice));
    try testing.expect(!buf.eql(&[_]T{ 1, 2, 3, 4, 5, 6, 7, 8, 9 }));
    try testing.expect(!buf.eql(&[_]T{ 1, 2, 3, 4, 5, 6, 8, 8 }));
}

test "get" {
    const CAP: usize = 8;
    const T = usize;
    const slice = [_]T{ 1, 2, 3, 4, 5, 6, 7, 8 };
    var buf = (try RingBuf(T, CAP)).init();
    try buf.fromSlice(&slice);
    var idx: Idx = 0;
    while (idx < slice.len + CAP * 2) : (idx += 1) {
        const elem = buf.get(idx);
        if (idx < slice.len) {
            try testing.expectEqual(slice[idx], elem.?.*);
        } else {
            try testing.expect(elem == null);
        }
    }
}
