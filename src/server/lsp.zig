const std = @import("std");
const kule = @import("../kule.zig");
const compiler = kule.compiler;

const Allocator = std.mem.Allocator;

const SrcLoc = compiler.SourceLocation;
const Message = kule.diagnostics.Message;

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

pub const Location = struct {
    uri: []const u8,
    range: Range,
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
    relatedInformation: []const Related = &.{},

    pub fn fromSrcMsg(msg: Message) Diagnostic {
        return .{
            .range = Range.fromSourceLocLen(msg.location, msg.token.len),
            .severity = switch (msg.message_type) {
                .err => .err,
                .info => .info,
            },
            .message = msg.message,
        };
    }

    pub fn fromSrcMsgWithRelated(allocator: Allocator, uri: []const u8, msg: Message) !Diagnostic {
        var diagnostic = fromSrcMsg(msg);
        if (msg.related_information.len > 0) {
            const related_info = try allocator.alloc(Related, msg.related_information.len);
            for (msg.related_information) |related, i| {
                related_info[i] = Related {
                    .location = Location {
                        .uri = uri,
                        .range = Range.fromSourceLocLen(related.location, related.token.len),
                    },
                    .message = related.message,
                };
            }
            diagnostic.relatedInformation = related_info;
        }
        return diagnostic;
    }

    pub const Related = struct {
        location: Location,
        message: []const u8,
    };

};

pub const Severity = enum(int) {
    err = 1,
    warn = 2,
    info = 3,
    hint = 4,
};