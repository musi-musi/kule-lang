const std = @import("std");

const source = @import("source.zig");
const log = @import("log.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Source = source.Source;
const SourceLocation = source.SourceLocation;
const LogType = log.LogType;


pub const Diagnostics = struct {

    arena: ArenaAllocator,
    messages: MessageList,
    err_count: usize = 0,

    const MessageList  = std.ArrayListUnmanaged(SourceMessage);


    pub fn init(allocator: Allocator) Diagnostics {
        return Diagnostics{
            .messages = .{},
            .arena = ArenaAllocator.init(allocator),
        };
    }

    pub fn deinit(self: Diagnostics) void {
        self.arena.deinit();
    }

    pub fn sourceError(self: *Diagnostics, src: *const Source, token: []const u8, comptime format: []const u8, args: anytype) !void {
        try self.sourceMessage(.err, src, token, format, args);
    }

    pub fn sourceInfo(self: *Diagnostics, src: *const Source, token: []const u8, comptime format: []const u8, args: anytype) !void {
        try self.sourceMessage(.info, src, token, format, args);
    }

    pub fn sourceMessage(self: *Diagnostics, message_type: LogType, src: *const Source, token: []const u8, comptime format: []const u8, args: anytype) !void {
        if (src.tokenLocation(token)) |location| {
            const allocator = self.arena.allocator();
            const message = try std.fmt.allocPrint(allocator, format, args);
            try self.messages.append(allocator, SourceMessage {
                .source = src,
                .location = location,
                .message_type = message_type,
                .token = token,
                .message = message,
            });
            if (message_type == .err) {
                self.err_count += 1;
            }
        }
        else {
            return error.MessageTokenNotFromMessageSource;
        }
    }
    
    pub fn sourceErrorErrorPanic(self: *Diagnostics, src: *const Source, token: []const u8, comptime format: []const u8, args: anytype) void {
        self.sourceMessageErrorPanic(.err, src, token, format, args);
    }

    pub fn sourceInfoErrorPanic(self: *Diagnostics, src: *const Source, token: []const u8, comptime format: []const u8, args: anytype) void {
        self.sourceMessage(.info, src, token, format, args);
    }

    pub fn sourceMessageErrorPanic(self: *Diagnostics, message_type: LogType, src: *const Source, token: []const u8, comptime format: []const u8, args: anytype) void {
        self.sourceMessage(message_type, src, token, format, args) catch |err| {
            const source_name: []const u8 = src.name orelse "(???)";
            std.debug.panic(
                \\could not add diagnostic: {s}
                \\ type: {s}
                \\ source: {x} {s}
                \\ token: {s}
                \\ format: {s}
                , .{ @errorName(err), @tagName(message_type), @ptrToInt(&src), source_name, token, format}
            );
        };
    }

    pub fn logMessages(self: *Diagnostics) !void {
        var arena = ArenaAllocator.init(self.arena.child_allocator);
        defer arena.deinit();
        const allocator = arena.allocator();

        const SourceMap = std.AutoArrayHashMapUnmanaged(*const Source, MessageList);

        var source_map = SourceMap{};
        for (self.messages.items) |message| {
            var list = source_map.get(message.source) orelse MessageList{};
            try list.append(allocator, message);
            try source_map.put(allocator, message.source, list);
        }


        const lists = try allocator.alloc(SourceMap.Entry, source_map.count());
        var iter = source_map.iterator();
        for (lists) |*list| {
            list.* = iter.next().?;
        }

        const sort = std.sort;


        sort.sort(SourceMap.Entry, lists, {}, struct {
            fn f(_: void, a: SourceMap.Entry, b: SourceMap.Entry) bool {
                const as = a.key_ptr.*;
                const bs = b.key_ptr.*;
                if (as == bs) {
                    return false;
                }
                else {
                    if (as.name == null and bs.name == null) {
                        return @ptrToInt(as) < @ptrToInt(bs);
                    }
                    if (as.name == null) {
                        return true;
                    }
                    if (bs.name == null) {
                        return false;
                    }
                    return std.mem.order(u8, as.name.?, bs.name.?) == .lt;
                }
            }
        }.f);

        for (lists) |entry| {
            const messages = entry.value_ptr.items;
            sort.sort(SourceMessage, messages, {}, struct {
                fn f(_: void, a: SourceMessage, b: SourceMessage) bool {
                    const al = a.location;
                    const bl = b.location;
                    switch (std.math.order(al.line, bl.line)) {
                        .lt => return true,
                        .gt => return false,
                        .eq => return std.math.order(al.column, bl.column) == .lt,
                    }
                }
            }.f);

            for (messages) |message| {
                log.sourceLog(message.message_type, message.source.*, message.token, "{s}", .{message.message});
            }
        }
    }


};

pub const SourceMessage = struct {

    source: *const Source,
    location: SourceLocation,
    message_type: LogType,
    token: []const u8,
    message: []const u8,

};