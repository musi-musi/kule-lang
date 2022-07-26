const std = @import("std");
const rpc = @import("rpc.zig");
const json = @import("json.zig");

const Allocator = std.mem.Allocator;

const File = std.fs.File;
const Reader = File.Reader;
const Writer = File.Writer;

pub const Server = struct {
    
    allocator: Allocator,
    reader: Reader,
    writer: Writer,
    log_file: File,
    
    state: State = .init,

    pub const State = enum {
        init,
        running,
        shutdown,
        exit,
    };


    const Self = @This();

    pub fn init(allocator: Allocator) !Self {
        return Self {
            .allocator = allocator,
            .reader = std.io.getStdIn().reader(),
            .writer = std.io.getStdOut().writer(),
            .log_file = try std.fs.cwd().createFile("server_err.txt", .{}),
        };
    }

    pub fn deinit(self: *Self) void {
        self.log_file.close();
    }

    pub fn run(self: *Self) !void {
        self.state = .running;
        const log = self.log_file.writer();
        while (self.state != .exit) {
            var message = rpc.Message.init(self.allocator, self.reader) catch |err| {
                try log.print("[[error]] {}\n", .{err});
                return;
            };
            defer message.deinit(self.allocator);
            
            try self.logMessage(message);
        }
    }

    fn logMessage(self: Self, message: rpc.Message) !void {
        const log = self.log_file.writer();
        if (message.id) |id| {
            try log.print("[{s: >6}] ", .{id.text});
        }
        else {
            try log.writeByteNTimes(' ', 8);

        }
        try log.print("{s}\n", .{message.method});

    }

};