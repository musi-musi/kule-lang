const std = @import("std");
const rpc = @import("rpc.zig");
const json = @import("json.zig");

const Allocator = std.mem.Allocator;

const File = std.fs.File;
const Reader = File.Reader;
const Writer = File.Writer;

const RpcStream = rpc.RpcStream;

pub const Method = fn(*Server, rpc.Request) anyerror!void;

const methods = struct {
    usingnamespace @import("methods/lifecycle.zig");
    usingnamespace @import("methods/textDocument.zig");
};

const method_map = blk: {
    const decls = std.meta.declarations(methods);
    const Pair = std.meta.Tuple(&.{[]const u8, Method});
    var pairs: [decls.len]Pair = undefined;
    var i: usize = 0;
    for (decls) |decl| {
        if (decl.is_pub) {
            pairs[i] = .{ decl.name, @field(methods, decl.name)};
            i += 1;
        }
    }
    break :blk std.ComptimeStringMap(Method, pairs[0..i]);
};

pub const Server = struct {
    
    allocator: Allocator,
    stream: RpcStream,
    
    state: State = .init,

    pub const State = enum {
        init,
        running,
        shutdown,
        exit,
    };


    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self {
            .allocator = allocator,
            .stream = RpcStream.init(
                allocator,
                std.io.getStdIn().reader(),
                std.io.getStdOut().writer(),
            ),
        };
    }

    pub fn deinit(self: *Self) void {
        _ = self;
    }

    pub fn run(self: *Self) !void {
        self.state = .running;
        while (self.state != .exit) {
            var message = try self.stream.listen();
            defer self.stream.messageFree(message);
            switch (message.content) {
                .request => |request| {
                    try self.log(.log, "[{s}] {s}", .{request.id().text, request.method});
                    if (method_map.get(request.method)) |method_handler| {
                        method_handler(self, request) catch |err| {
                            try self.log(.err, "[{s}] {}", .{request.id().text, err});
                        };
                    }
                    else {
                        try self.log(.log, " * no handler for ({s})", .{request.method});
                    }
                },
                .response => |response| {
                    if (response.@"error") |err| {
                        try self.log(.err, "[{s}] ({d}) {s}", .{response.id().text, @enumToInt(err.code), err.message});
                    }
                    else {
                        try self.log(.info, " * result for [{s}]", .{response.id().text});
                    }
                },
            }
        }



    }

    pub const LogType = enum(u32) {
        err = 1,
        warning = 2,
        info = 3,
        log = 4, 
    };

    pub fn log(self: *Self, log_type: LogType, comptime format: []const u8, args: anytype) !void {
        if (self.state == .running) {
            var jw = json.ValueWriter.init(self.allocator);
            defer jw.deinit();
            try jw.object();
            try jw.fieldName("type");
            try jw.number(@enumToInt(log_type));
            try jw.fieldName("message");
            try jw.stringFormat(format, args);
            try jw.objectEnd();
            try self.notifyClient("window/logMessage", jw.text());
        }
    }

    pub fn notifyClient(self: Self, method: []const u8, params: anytype) !void {
        try self.stream.notify(method, params);
    }

    pub fn initialized(self: *Self) void {
        self.state = .running;
    }

    pub fn shutdown(self: *Self) void {
        self.state = .shutdown;
    }

    pub fn exit(self: *Self) void {
        self.state = .exit;
    }

};