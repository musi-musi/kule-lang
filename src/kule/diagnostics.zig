const std = @import("std");


const compiler = @import("compiler.zig");
const logger = @import("logger.zig");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Source = compiler.Source;
const SourceLocation = compiler.SourceLocation;
const LogType = logger.LogType;


pub const Diagnostics = struct {

    arena: ArenaAllocator,
    source: *const Source,
    messages: MessageList,
    error_count: usize = 0,

    const MessageList  = std.ArrayListUnmanaged(*Message);


    pub fn init(allocator: Allocator, source: *const Source) Diagnostics {
        return Diagnostics{
            .messages = .{},
            .arena = ArenaAllocator.init(allocator),
            .source = source,
        };
    }

    pub fn deinit(self: Diagnostics) void {
        self.arena.deinit();
    }

    pub fn logInfo(self: *Diagnostics, token: []const u8, comptime format: []const u8, args: anytype) void
        { self.log(.info, token, format, args); }
    pub fn logError(self: *Diagnostics, token: []const u8, comptime format: []const u8, args: anytype) void
        { self.log(.err, token, format, args); }

    pub fn log(self: *Diagnostics, message_type: LogType, token: []const u8, comptime format: []const u8, args: anytype) void {
        _ = self.start(message_type, token, format, args);
    }

    pub fn startInfo(self: *Diagnostics, token: []const u8, comptime format: []const u8, args: anytype) *Message
        { return self.start(.info, token, format, args); }
    pub fn startError(self: *Diagnostics, token: []const u8, comptime format: []const u8, args: anytype) *Message
        { return self.start(.err, token, format, args); }

    pub fn start(self: *Diagnostics, message_type: LogType, token: []const u8, comptime format: []const u8, args: anytype) *Message {
        const message = self.create(message_type, token, format, args);
        self.append(message);
        return message;
    }

    pub fn append(self: *Diagnostics, message: *Message) void {
        self.messages.append(self.arena.allocator(), message) catch panicOutOfMemory();
        if (message.message_type == .err) {
            self.error_count += 1;
        }
    }



    pub fn createInfo(self: *Diagnostics, token: []const u8, comptime format: []const u8, args: anytype) *Message
        { return self.create(.info, token, format, args); }
    pub fn createError(self: *Diagnostics, token: []const u8, comptime format: []const u8, args: anytype) *Message
        { return self.create(.err, token, format, args); }

    pub fn create(self: *Diagnostics, message_type: LogType, token: []const u8, comptime format: []const u8, args: anytype) *Message {
        if (self.source.tokenLocation(token)) |location| {
            const allocator = self.arena.allocator();
            const message_text = std.fmt.allocPrint(allocator, format, args) catch panicOutOfMemory();
            const message = allocator.create(Message) catch panicOutOfMemory();
            message.* = Message {
                .diagnostics = self,
                .location = location,
                .message_type = message_type,
                .token = token,
                .message = message_text,
            };
            return message;
        }
        else {
            std.debug.panic("token '{s}' ({x:0>16}..{x:0>16}) is not from source '{s}' ({x:0>16}..{x:0>16})", .{
                token, @ptrToInt(token.ptr), @ptrToInt(token.ptr) + token.len,
                self.source.displayName(), @ptrToInt(self.source.text.ptr), @ptrToInt(self.source.text.ptr) + self.source.text.len,
            });
        }
    }

    pub fn logMessages(self: *Diagnostics) void {

        const sort = std.sort;


        const messages = self.messages.items;
        sort.sort(*Message, messages, {}, struct {
            fn f(_: void, a: *Message, b: *Message) bool {
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
            var msg: ?*Message = message;
            while (msg) |m| : (msg = m.next_in_group) {
                logger.sourceLog(message.message_type, self.source.*, message.token, "{s}", .{message.message});
            }
        }
    }

    fn panicOutOfMemory() noreturn {
        @panic("diagnostics: out of memory");
    }


};

pub const Message = struct {
    diagnostics: *Diagnostics,
    location: SourceLocation,
    message_type: LogType,
    token: []const u8,
    message: []const u8,
    next_in_group: ?*Message = null,

    pub fn add(self: *Message, message_type: LogType, token: []const u8, comptime format: []const u8, args: anytype) *Message {
        const next = self.diagnostics.create(message_type, token, format, args);
        self.next_in_group = next;
        return next;
    }

    pub fn addError(self: *Message, token: []const u8, comptime format: []const u8, args: anytype) *Message {
        return self.add(.err, token, format, args);
    }

    pub fn addInfo(self: *Message, token: []const u8, comptime format: []const u8, args: anytype) *Message {
        return self.add(.info, token, format, args);
    }
};