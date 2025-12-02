// TODO: we kinda assume ascii or utf8 but we also operate on strings as bytes

const std = @import("std");

const ascii = std.ascii;
const enums = std.enums;
const fmt = std.fmt;
const heap = std.heap;
const Io = std.Io;
const math = std.math;
const mem = std.mem;
const File = std.fs.File;

const meta = @import("meta.zig");
const rb = @import("ringbuf.zig");
const tap = @import("tap.zig");

const BadUsage = error{BadUsage};
const TempoError = BadUsage || Io.Reader.Error || Io.Writer.Error || tap.Tapper.Error;

const ARGS_ALLOC_BUF_SIZE: usize = 1024;
const INPUT_BUF_SIZE: usize = 1024;
const STDOUT_BUF_SIZE: usize = 256;
const STDERR_BUF_SIZE: usize = 64;

const DEFAULT_BUF_CAP: rb.Idx = 10;
const DEFAULT_BOUNDED: bool = true;

pub fn main() void {
    var stdin_buf: [INPUT_BUF_SIZE]u8 = undefined;
    var stdout_buf: [STDOUT_BUF_SIZE]u8 = undefined;
    var stderr_buf: [STDERR_BUF_SIZE]u8 = undefined;
    var stdin_reader = File.stdin().reader(&stdin_buf);
    var stdout_writer = File.stdout().writer(&stdout_buf);
    var stderr_writer = File.stderr().writer(&stderr_buf);
    const stdin = &stdin_reader.interface;
    var stdout = &stdout_writer.interface;
    var stderr = &stderr_writer.interface;

    var exit_code: u8 = 0;

    tryMain(stdin, stdout, stderr) catch |err| {
        const try_repr = switch (err) {
            TempoError.BadUsage => null, // tryMain already took care of error message
            TempoError.ClockUnsupported => "a suitable clock was not detected for this platform",
            else => "input/output error",
        };
        if (try_repr) |repr| {
            stderr.print("fatal error: {s} ({})\n", .{ repr, err }) catch {};
        }
        exit_code = 1;
    };

    stdout.flush() catch {};
    stderr.flush() catch {};
    std.process.exit(exit_code);
}

fn tryMain(stdin: *Io.Reader, stdout: *Io.Writer, stderr: *Io.Writer) TempoError!void {
    // ensure we're not being passed any arguments
    try checkUsage(stderr);
    try stderr.flush();

    // check that the clock is supported (at least currently)
    try tap.Tapper.checkSupported();

    var tapper = tap.Tapper.init(DEFAULT_BUF_CAP, DEFAULT_BOUNDED);

    try splashText(stdout);

    while (true) {
        try stdout.print("\n", .{});

        // print the bpm and buffer stats
        try stdout.print("{}/{}{s} samples in buffer\n", .{
            tapper.count(),
            tapper.capacity(),
            if (tapper.isBounded()) "" else "+",
        });
        try tap.formatBpm(stdout, tapper.bpm());
        try stdout.print(" bpm\n", .{});

        const try_cmd = if (try readln(
            stdin,
            stdout,
            if (tapper.isRecording()) " * " else " ; ",
        )) |input|
            Command.fromStr(input)
        else
            // quit on EOF by convention
            .Quit;

        if (try_cmd) |cmd| {
            switch (cmd) {
                .Help => {
                    try stdout.print("\n", .{});
                    for (enums.values(Command)) |c| {
                        try stdout.print(" {s} or {s}. {s}.\n", .{ c.longName(), c.shortName(), c.description() });
                    }
                },

                .Tap => try tapper.tap(),

                .Clear => tapper.clear(),

                .Size => {
                    try stdout.print("\n", .{});

                    const input = try readln(stdin, stdout, " new buffer size? ") orelse "";
                    if (input.len == 0) continue;

                    var try_cap: ?rb.Idx = null;
                    if (fmt.parseUnsigned(rb.Idx, input, 10)) |cap| {
                        try_cap = cap;
                    } else |err| {
                        switch (err) {
                            fmt.ParseIntError.Overflow => try_cap = math.maxInt(rb.Idx),
                            fmt.ParseIntError.InvalidCharacter => {
                                try stdout.print(" invalid character in integer\n", .{});
                            },
                        }
                    }
                    if (try_cap) |cap| {
                        tapper.resize(cap);
                        const reported = tapper.capacity();
                        if (reported < cap) {
                            try stdout.print(" size too large, clamped to {}\n", .{reported});
                        }
                    }
                },

                .Bound => tapper.toggleBounded(),

                .Print => {
                    try stdout.print("\n", .{});
                    try stdout.print(" ", .{});
                    try tap.formatTapper(stdout, &tapper);
                    try stdout.print("\n", .{});
                },

                .Quit => {
                    try stdout.print("\n", .{});
                    try stdout.print(" goodbye\n", .{});
                    try stdout.print("\n", .{});
                    break;
                },
            }
        } else {
            try stdout.print("\n", .{});
            try stdout.print(" unrecognized command. try \"h\" for help.\n", .{});
        }
    }
}

fn checkUsage(stderr: *Io.Writer) (BadUsage || Io.Writer.Error)!void {
    var args_alloc_buf: [ARGS_ALLOC_BUF_SIZE]u8 = undefined;
    var args_alloc = heap.FixedBufferAllocator.init(&args_alloc_buf);
    var try_args = std.process.argsWithAllocator(args_alloc.allocator());
    if (try_args) |*args| {
        defer args.deinit();
        const name = args.next();
        if (args.skip()) {
            stderr.print("usage: {s}\n", .{name.?}) catch {};
            return TempoError.BadUsage;
        }
    } else |err| {
        // we can't check argc but that's not a fatal error
        try stderr.print("error: failed to allocate arguments: {any}\n", .{err});
    }
}

fn splashText(out: *Io.Writer) Io.Writer.Error!void {
    try out.print("{s} {f}: {s}\n", .{
        meta.BIN_NAME,
        meta.PKG_VERSION,
        meta.PKG_DESCRIPTION,
    });
    try out.print("type \"h\" for help\n", .{});
}

fn readln(
    in: *Io.Reader,
    out: *Io.Writer,
    prompt: []const u8,
) (Io.Writer.Error || Io.Reader.Error)!?[]const u8 {
    try out.print("{s}", .{prompt});
    try out.flush();
    const read = in.takeDelimiter('\n') catch |err| switch (err) {
        error.StreamTooLong => in.buffered(),
        else => |e| return e,
    } orelse return null;
    in.tossBuffered();
    const line = mem.trim(u8, read, &ascii.whitespace);
    return line;
}

const Command = enum {
    Help,
    Tap,
    Clear,
    Size,
    Bound,
    Print,
    Quit,

    const Self = @This();

    pub fn fromStr(s: []const u8) ?Self {
        for (enums.values(Self)) |cmd| {
            inline for (.{ cmd.literal(), cmd.longName() }) |repr| {
                if (ascii.eqlIgnoreCase(s, repr))
                    return cmd;
            }
        }
        return null;
    }

    pub fn literal(self: Self) []const u8 {
        return switch (self) {
            .Help => "h",
            .Tap => "",
            .Clear => "c",
            .Size => "s",
            .Bound => "b",
            .Print => "p",
            .Quit => "q",
        };
    }

    pub fn shortName(self: Self) []const u8 {
        return switch (self) {
            .Tap => "<enter>",
            else => |other| other.literal(),
        };
    }

    pub fn longName(self: Self) []const u8 {
        return switch (self) {
            .Help => "help",
            .Tap => "tap",
            .Clear => "clear",
            .Size => "size",
            .Bound => "bound",
            .Print => "print",
            .Quit => "quit",
        };
    }

    pub fn description(self: Self) []const u8 {
        return switch (self) {
            .Help => "describe commands",
            .Tap => "register a beat",
            .Clear => "clear buffer contents",
            .Size => "adjust buffer size",
            .Bound => "toggle whether buffer is bounded to size",
            .Print => "print buffer contents in order from newest to oldest",
            .Quit => "quit",
        };
    }
};
