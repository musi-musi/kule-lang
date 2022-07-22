const std = @import("std");
const compiler = @import("../compiler.zig");

const source = @import("../source.zig");
const diagnostics = @import("../diagnostics.zig");

const json = @import("json.zig");

const Source = source.Source;
const Diagnostics = diagnostics.Diagnostics;

const Allocator = std.mem.Allocator;

const File = std.fs.File;

const Reader = File.Reader;
const Writer = File.Writer;


var err_file: ?File = null;


fn openErrFile() !void {
    err_file = try std.fs.cwd().createFile("server_err.txt", .{});
}

fn closeErrFile() void {
    if (err_file) |file| {
        err_file = null;
        file.close();
    }
}


pub fn log(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    if (err_file) |file| {
        const writer = file.writer();
        const prefix = comptime std.fmt.comptimePrint("[{s}] ", .{@tagName(level)[0..3]});
        writer.print(prefix ++ format ++ "\n", args) catch {};
    }
    else {
        std.log.defaultLog(level, scope, format, args);
    }
}


pub const Server = struct {
    
    allocator: Allocator,
    in: Reader,
    out: Writer,
    out_file: File,
    json_reader: json.JsonReader = undefined,
    incoming_buffer: []u8,
    outgoing_buffer: MessageBuffer,

    response_count: usize = 0,

    const MessageBuffer = std.ArrayListUnmanaged(u8);


    const Self = Server;

    pub fn init(allocator: Allocator) !Self {
        const out_file = try std.fs.cwd().createFile("server_out.txt", .{});
        var self = Self {
            .allocator = allocator,
            .in = std.io.getStdIn().reader(),
            .out = out_file.writer(),
            .out_file = out_file,
            .incoming_buffer = try allocator.alloc(u8, 512),
            .outgoing_buffer = try MessageBuffer.initCapacity(allocator, 512),
        };
        try openErrFile();
        return self;
    }

    pub fn deinit(self: *Self) void {
        closeErrFile();
        self.out_file.close();
        self.allocator.free(self.incoming_buffer);
        self.outgoing_buffer.deinit(self.allocator);
    }

    pub fn run(self: *Self) !void {
        while (true) {
            const msg = try self.readRpcMessage();
            self.json_reader.init(msg);
            try self.json_reader.beginObject();
            const header = try self.json_reader.readObject(RequestHeader);
            if (method_response_map.get(header.method)) |response| {
                try response(self, header);
            }
        }
    }


    const methods = struct{
        pub usingnamespace @import("system.zig");
    };

    const method_response_map: type = blk: {
        const Pair = std.meta.Tuple(&.{[]const u8, MethodResponse});
        var pairs: []const Pair = &.{};
        for (std.meta.declarations(methods)) |decl| {
            if (decl.is_pub) {
                const pair = Pair {
                    decl.name,
                    methodResponse(decl.data.Type),
                };
                pairs = pairs ++ &[_]Pair{ pair};
            }
        }
        break :blk std.ComptimeStringMap(MethodResponse, pairs);
    };

    fn methodResponse(comptime Params: type) MethodResponse {
        const respond_fn = @field(Params, "respond");
        const Request = struct {
            id: usize,
            method: []const u8,
            params: ?Params,
        };
        return struct {
            fn f(s: *Server, header: RequestHeader) !void {
                const params = try s.json_reader.readObjectToField("params", Params);
                try respond_fn(s, Request {
                    .id = header.id,
                    .method = header.method,
                    .params = params,
                });

            }
        }.f;
    }

    const RequestHeader = struct {
        id: usize,
        method: []const u8,
    };

    const MethodResponse = fn(*Server, RequestHeader) anyerror!void;

    fn readRpcMessage(self: *Self) ![]u8 {
        const in = self.in;
        var c = try in.readByte();
        while (!std.ascii.isDigit(c)) : (c = try in.readByte()) {}
        var len: usize = 0;
        while (std.ascii.isDigit(c)) : (c = try in.readByte()) {
            len = len * 10 + (c - '0');
        }
        while (c != '{') : (c = try in.readByte()) {}
        if (self.incoming_buffer.len < len) {
            self.allocator.free(self.incoming_buffer);
            self.incoming_buffer = try self.allocator.alloc(u8, len + 64);
        }
        self.incoming_buffer[0] = c;
        self.incoming_buffer[len] = 0;
        _ = try in.read(self.incoming_buffer[1..len]);
        return self.incoming_buffer;
    }

    pub fn readParams(self: *Self, comptime Params: type) !?Params {
        return try self.json_reader.readObjectToField("params", Params);
    }

    pub const ResponseType = enum {
        result,
        err,
    };

    pub fn send(self: *Self, comptime response_type: ResponseType, response: anytype) !void {
        const response_message = switch (response_type) {
            .result => .{
                .jsonrpc = "2.0",
                .id = self.response_count,
                .result = response,
            },
            .err => .{
                .jsonrpc = "2.0",
                .id = self.response_count,
                .@"error" = response,
            },
        };
        self.response_count += 1;
        self.outgoing_buffer.clearRetainingCapacity();
        const writer = self.outgoing_buffer.writer(self.allocator);
        try json.write(writer, response_message);
        const message = self.outgoing_buffer.items;
        try self.out.print("Content-Length: {d}\n\r\n\r{s}", .{message.len, message});
    }

    pub fn result(self: *Self, result_item: anytype) !void {
        try self.send(.result, result_item);
    }

    pub fn err(self: *Self, error_item: anytype) !void {
        try self.send(.err, error_item);
    }

};









fn dumpTypeFields(comptime name: []const u8, comptime T: type) void {
    inline for (comptime std.meta.fields(T)) |field| {
        const field_name = name ++ "." ++ field.name;
        std.log.info("{s}: {s}", .{field_name, @typeName(field.field_type)});
        if (@typeInfo(field.field_type) == .Struct) {
            dumpTypeFields(field_name, field.field_type);
        }
    }
}

pub fn dumpFields(comptime name: []const u8, value: anytype) void {
    const Value = @TypeOf(value);
    inline for (comptime std.meta.fields(Value)) |field| {
        const field_name = name ++ "." ++ field.name;
        if (@typeInfo(field.field_type) == .Struct) {
            dumpFields(field_name, @field(value, field.name));
        }
        else {
            switch (field.field_type) {
                []const u8 => std.log.info("{s} = {s}", .{field_name, @field(value, field.name)}),
                usize => std.log.info("{s} = {d}", .{field_name, @field(value, field.name)}),
                bool => std.log.info("{s} = {}", .{field_name, @field(value, field.name)}),
                else => std.log.info("{s} = ...", .{field_name}),
            }
        }
    }
}