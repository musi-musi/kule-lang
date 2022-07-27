const std = @import("std");
const kule = @import("../kule.zig");

const SrcLoc = kule.SourceLocation;
const SrcMsg = kule.diagnostics.SourceMessage;

pub const int = i32;
pub const uint = u32;

pub const Position = struct {
    line: usize,
    character: usize,

    pub fn fromSourceLoc(loc: SrcLoc) Position {
        return .{
            .line = loc.line,
            .character = loc.column,
        };
    }
};

pub const Range = struct {
    start: Position,
    end: Position,

    pub fn fromSourceLocLen(loc: SrcLoc, len: usize) Range {
        return .{
            .start = .{ .line = loc.line, .character = loc.column, },
            .end = .{ .line = loc.line, .character = loc.column + len, },
        };
    }
};

pub const Diagnostic = struct {
    range: Range,
    severity: Severity = .err,
    code: []const u8 = "code",
    codeDescription: []const u8 = "description",
    source: []const u8 = "source",
    message: []const u8 = "",

    pub fn fromSrcMsg(msg: SrcMsg) Diagnostic {
        return .{
            .range = Range.fromSourceLocLen(msg.location, msg.token.len),
            .severity = switch (msg.message_type) {
                .err => .err,
                .info => .info,
            },
            .message = msg.message,
        };
    }
};

pub const Severity = enum(int) {
    err = 1,
    warn = 2,
    info = 3,
    hint = 4,
};