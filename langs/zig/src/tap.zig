const std = @import("std");

const assert = std.debug.assert;
const Io = std.Io;
const math = std.math;
const testing = std.testing;
const time = std.time;

const rb = @import("ringbuf.zig");

pub const Bpm = f32;

/// Tempo tapper which measures the average BPM between taps.
pub const Tapper = struct {
    const BUF_TYPE = rb.RingBuf(Bpm, MAX_CAP) catch unreachable;
    buf: BUF_TYPE,
    cap: rb.Idx,
    bounded: bool,
    last_tap: ?time.Instant,

    const Self = @This();

    pub const MAX_CAP: rb.Idx = 0x1000;
    pub const Error = error{ClockUnsupported};

    /// Return a new `Tapper` with its buffer capped to `cap`.
    pub fn init(
        cap: rb.Idx,
        bounded: bool,
    ) Self {
        const self: Self = .{
            .buf = BUF_TYPE.init(),
            .cap = cap,
            .bounded = bounded,
            .last_tap = null,
        };
        self.checkCount();
        return self;
    }

    pub fn checkSupported() Error!void {
        _ = time.Instant.now() catch return Error.ClockUnsupported;
    }

    /// Record the interval since the last tap.
    pub fn tap(self: *Self) Error!void {
        defer self.checkCount();
        defer assert(self.isRecording());

        const now = time.Instant.now() catch return Error.ClockUnsupported;

        // update bpm
        const old_last = self.last_tap;
        self.last_tap = now;
        if (old_last) |last| {
            // TODO: Instant.since assumes difference (nanos) fits in a u64, but we don't check this
            const nanos: u64 = switch (now.order(last)) {
                math.Order.lt => 0,
                else => now.since(last),
            };
            const secs: Bpm = @as(Bpm, @floatFromInt(nanos)) / @as(Bpm, time.ns_per_s);

            // push a new bpm
            const new_bpm: Bpm = 60.0 / secs;
            Self.checkBpm(new_bpm);
            self.buf.push(new_bpm);

            // remove old elements
            self.syncCap();
        }
    }

    /// Clear the buffer of all samples and forget the last tap.
    pub fn clear(self: *Self) void {
        defer self.checkCount();
        self.buf.clear();
        self.last_tap = null;
    }

    /// Restrict the buffer to at most `new_cap` samples.
    pub fn resize(self: *Self, new_cap: rb.Idx) void {
        self.cap = new_cap;
        self.syncCap();
    }

    /// Toggle whether the buffer is restricted to `capacity()`
    /// samples.
    pub fn toggleBounded(self: *Self) void {
        self.bounded = !self.bounded;
        self.syncCap();
    }

    /// Return the average BPM between recorded taps.
    pub fn bpm(self: *const Self) Bpm {
        const len = self.count();
        // https://www.nu42.com/2015/03/how-you-average-numbers.html
        var avg: Bpm = 0.0;
        var idx: rb.Idx = 0;
        while (idx < len) : (idx += 1) {
            avg += (self.buf.read(idx).? - avg) /
                @as(Bpm, @floatFromInt(idx + 1));
        }
        Self.checkBpm(avg);
        return avg;
    }

    /// Return the number of samples held by the buffer.
    pub fn count(self: *const Self) rb.Idx {
        return self.buf.len();
    }

    /// Return the capacity of the buffer when bounded.
    pub fn capacity(self: *const Self) rb.Idx {
        return @min(self.cap, MAX_CAP);
    }

    /// Return `true` if a tap has been recorded.
    pub fn isRecording(self: *const Self) bool {
        return self.last_tap != null;
    }

    /// Return `true` if the buffer is bounded.
    pub fn isBounded(self: *const Self) bool {
        return self.bounded;
    }

    fn syncCap(self: *Self) void {
        defer self.checkCount();
        if (self.isBounded()) {
            self.buf.truncateBack(self.capacity());
        }
    }

    fn checkCount(self: *const Self) void {
        const len = self.count();
        const effective_cap = if (self.isBounded()) self.capacity() else MAX_CAP;
        assert(len <= effective_cap);
    }

    fn checkBpm(it: Bpm) void {
        assert(!math.isNan(it));
        assert(math.sign(it) != -1);
    }
};

pub fn formatBpm(fmt: *Io.Writer, bpm: Bpm) !void {
    try fmt.print("{d:.1}", .{bpm});
}

pub fn formatTapper(fmt: *Io.Writer, tapper: *const Tapper) !void {
    try fmt.print("[", .{});
    const len = tapper.buf.len();
    var idx: rb.Idx = 1;
    while (idx <= len) : (idx += 1) {
        const bpm = tapper.buf.read(len - idx).?;
        try formatBpm(fmt, bpm);
        if (idx < len) {
            try fmt.print(", ", .{});
        }
    }
    try fmt.print("]", .{});
}

test "formatTapper" {
    var buf = Io.Writer.Allocating.init(testing.allocator);
    defer buf.deinit();

    var tapper = Tapper.init(10, true);
    tapper.buf.push(120.05);
    tapper.buf.push(112.41);
    tapper.buf.push(121.105);

    try formatTapper(
        &buf.writer,
        &tapper,
    );
    try testing.expect(std.mem.eql(u8, buf.written(), "[121.1, 112.4, 120.1]"));

    buf.clearRetainingCapacity();
    tapper.buf.clear();
    tapper.buf.push(112.76);
    try formatTapper(
        &buf.writer,
        &tapper,
    );
    try testing.expect(std.mem.eql(u8, buf.written(), "[112.8]"));

    buf.clearRetainingCapacity();
    tapper.buf.clear();
    try formatTapper(
        &buf.writer,
        &tapper,
    );
    try testing.expect(std.mem.eql(u8, buf.written(), "[]"));
}

test "count" {
    var tapper = Tapper.init(10, true);
    for ([_]void{{}} ** 10) |_| {
        tapper.buf.push(23.0);
    }
    try testing.expectEqual(tapper.buf.len(), tapper.count());
}

test "isRecording" {
    var tapper = Tapper.init(10, true);
    try testing.expect(!tapper.isRecording());
    try tapper.tap();
    try testing.expect(tapper.isRecording());
    try tapper.tap();
    try testing.expect(tapper.isRecording());
    tapper.clear();
    try testing.expect(!tapper.isRecording());
}

test "isBounded" {
    var tapper = Tapper.init(10, true);
    try testing.expect(tapper.isBounded());
    tapper.toggleBounded();
    try testing.expect(!tapper.isBounded());
    tapper.toggleBounded();
    try testing.expect(tapper.isBounded());
}

test "bpm" {
    var tapper = Tapper.init(10, true);
    const e = 0.001;
    try testing.expect(math.approxEqAbs(Bpm, 0.0, tapper.bpm(), e));
    tapper.buf.push(23.0);
    try testing.expect(math.approxEqAbs(Bpm, 23.0, tapper.bpm(), e));
    tapper.buf.push(26.0);
    try testing.expect(math.approxEqAbs(Bpm, 24.5, tapper.bpm(), e));
    tapper.buf.push(29.0);
    try testing.expect(math.approxEqAbs(Bpm, 26.0, tapper.bpm(), e));
    tapper.buf.push(61.0);
    try testing.expect(math.approxEqAbs(Bpm, 34.75, tapper.bpm(), e));
}

test "tap" {
    var tapper = Tapper.init(3, true);
    try testing.expectEqual(@as(rb.Idx, 0), tapper.count());
    try tapper.tap();
    try testing.expectEqual(@as(rb.Idx, 0), tapper.count());
    try tapper.tap();
    try testing.expectEqual(@as(rb.Idx, 1), tapper.count());
    try tapper.tap();
    try testing.expectEqual(@as(rb.Idx, 2), tapper.count());
    try tapper.tap();
    try testing.expectEqual(@as(rb.Idx, 3), tapper.count());
    try tapper.tap();
    try testing.expectEqual(@as(rb.Idx, 3), tapper.count());
    tapper.toggleBounded();
    try tapper.tap();
    try testing.expectEqual(@as(rb.Idx, 4), tapper.count());
    try tapper.tap();
    try testing.expectEqual(@as(rb.Idx, 5), tapper.count());
    tapper.toggleBounded();
    try testing.expectEqual(@as(rb.Idx, 3), tapper.count());
}

test "truncate" {
    var tapper = Tapper.init(3, true);
    try tapper.buf.fromSlice(&[_]Bpm{ 80, 70, 60 });
    try testing.expect(tapper.buf.eql(&[_]Bpm{ 80, 70, 60 }));
    tapper.buf.push(50);
    try testing.expect(tapper.buf.eql(&[_]Bpm{ 80, 70, 60, 50 }));
    tapper.syncCap();
    try testing.expect(tapper.buf.eql(&[_]Bpm{ 70, 60, 50 }));
    tapper.toggleBounded();
    tapper.buf.push(40);
    try testing.expect(tapper.buf.eql(&[_]Bpm{ 70, 60, 50, 40 }));
    tapper.syncCap();
    try testing.expect(tapper.buf.eql(&[_]Bpm{ 70, 60, 50, 40 }));
    tapper.toggleBounded();
    try testing.expect(tapper.buf.eql(&[_]Bpm{ 60, 50, 40 }));
}

test "resize" {
    var tapper = Tapper.init(3, true);
    try tapper.buf.fromSlice(&[_]Bpm{ 80, 70, 60 });
    try testing.expect(tapper.buf.eql(&[_]Bpm{ 80, 70, 60 }));
    tapper.resize(2);
    try testing.expect(tapper.buf.eql(&[_]Bpm{ 70, 60 }));
    try testing.expectEqual(@as(rb.Idx, 2), tapper.capacity());
}
