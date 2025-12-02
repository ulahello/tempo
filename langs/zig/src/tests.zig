const std = @import("std");

test "ringbuf" {
    std.testing.refAllDecls(@import("ringbuf.zig"));
}

test "tap" {
    std.testing.refAllDecls(@import("tap.zig"));
}
