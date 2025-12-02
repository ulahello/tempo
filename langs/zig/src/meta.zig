// as zig package management develops this will become more hacky

const std = @import("std");

pub const BIN_NAME = "tempo";
pub const PKG_VERSION: std.SemanticVersion = .{ .major = 0, .minor = 1, .patch = 0 };
pub const PKG_DESCRIPTION = "terminal tempo tapper";
