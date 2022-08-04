const std = @import("std");

const ascii = std.ascii;

const json = @import("json.zig");
const Server = @import("server.zig").Server;

const Jw = json.Value;

const Allocator = std.mem.Allocator;
const File = std.fs.File;

const Reader = File.Reader;
const Writer = File.Writer;





pub const int = i32;
pub const uint = u32;

pub const initId = json.Value.init;

pub const RpcStream = struct {
    allocator: Allocator,
    reader: Reader,
    writer: Writer,

    const Self = @This();

    pub fn init(allocator: Allocator, reader: Reader, writer: Writer) Self {
        return Self {
            .allocator = allocator,
            .reader = reader,
            .writer = writer,
        };
    }

    pub const Error = error {
        EndOfStream,
        MissingJsonData,
        // MissingMethodName,
    } || Allocator.Error || Reader.Error;

    pub fn listen(self: Self) !Message {
        const r = self.reader;
        const isDigit = ascii.isDigit;
        var c: u8 = 0;
        while (!isDigit(c)) : (c = try r.readByte()) {}
        var len: usize = 0;
        while (isDigit(c)) : (c = try r.readByte()) {
            len = len * 10 + c - '0';
        }
        while (c != '{') : (c = try r.readByte()) {}
        const text = try self.allocator.alloc(u8, len);
        errdefer self.allocator.free(text);
        text[0] = c;
        _ = try r.readAll(text[1..]);
        if (Content.init(self, text)) |content| {
            return Message {
                .text = text,
                .content = content,
            };
        }
        else {
            return Error.MissingJsonData;
        }
    }

    pub fn messageFree(self: Self, message: Message) void {
        self.allocator.free(message.text);
    }

    fn sendMessageHeader(self: Self, rest_len: usize) !void {
        try self.writer.print(
            "Content-Length: {d}\r\n\r\n" ++
            \\{{"jsonrpc":"2.0",
            , .{17 + rest_len}
        );
    }

    fn sendResponseHeader(self: Self, id: json.Value, rest_len: usize) !void {
        try self.sendMessageHeader(6 + id.text.len + rest_len);
        try self.writer.print(
            \\"id":{s},
            ,.{id.text}
        );
    }

    pub fn sendResult(self: Self, id: json.Value, result: anytype) !void {
        const text = try json.serialize(self.allocator, result);
        defer self.allocator.free(text);
        try self.sendResponseHeader(id, 10 + text.len);
        try self.writer.print(
            \\"result":{s}}}
            , .{text}
        );
    }

    pub fn sendEmptyResult(self: Self, id: json.Value) !void {
        try self.sendResponseHeader(id, 14);
        try self.writer.writeAll(
            \\"result":null}
        );
    }

    pub fn notify(self: Self, method: []const u8, params: anytype) !void {
        var allocated_params: bool = false;
        const params_text: ?[]const u8 = blk: {
            if (comptime std.meta.trait.isZigString(@TypeOf(params))) {
                break :blk params;
            }
            else if (isSkipped(params)) {
                break :blk null;
            }
            else {
                allocated_params = true;
                break :blk try json.serialize(self.allocator, params);
            }
        };
        const params_len = (
            if (params_text) |text| text.len + 10 else 0
        );
        try self.sendMessageHeader(params_len + method.len + 12);
        try self.writer.writeAll(
            \\"method":"
        );
        try json.printEscaped(self.writer, "{s}", .{method});
        try self.writer.writeByte('"');
        if (params_text) |text| {
            try self.writer.print(
                \\,"params":{s}
                , .{text}
            );
            if (allocated_params) {
                self.allocator.free(text);
            }
        }
        try self.writer.writeByte('}');
    }

    pub fn sendError(self: Self, id: json.Value, code: ResponseError.Code, comptime format: []const u8, args: anytype) !void {
        var jw = json.ValueWriter.init(self.allocator);
        defer jw.deinit();
        try jw.object();
        try jw.fieldName("code");
        try jw.number(@enumToInt(code));
        try jw.fieldName("message");
        try jw.stringFormat(format, args);
        try jw.objectEnd();

        const error_text = jw.text();

        try self.sendResponseHeader(id, 9 + error_text.len);
        try self.writer.print(
            \\"error":{s}}}
            ,.{error_text}
        );
    }


};

fn isSkipped(item: anytype) bool {
    const trait = std.meta.trait;
    const T = @TypeOf(item);
    if (T == void) {
        return true;
    }
    if (comptime trait.is(.Null)(T)) {
        return true;
    }
    if (comptime trait.is(.Optional)(T)) {
        return item == null;
    }
    return false;
}

pub const Message = struct {
    text: []const u8,
    content: Content,
};

pub const Content = union(enum) {

    request: Request,
    response: Response,

    const Json = struct {
        id: ?json.Value,
        method: ?[]const u8,
        params: ?json.Value,
        result: ?json.Value,
        @"error": ?ResponseError,
    };

    fn init(stream: RpcStream, text: []const u8) ?Content {
        if (json.parse(text, Json)) |j| {
            if (j.method == null) {
                return Content{
                    .response = .{
                        .request_id = j.id,
                        .result = j.result,
                        .@"error" = j.@"error",
                    },
                };
            }
            else {
                return Content{
                    .request =  .{
                        .request_id = j.id,
                        .method = j.method.?,
                        .params_opt = j.params,
                        .stream = stream,
                    },
                };
            }
        }
        return null;
    }

};

const null_value = json.Value.init("null");

pub const Request = struct {
    request_id: ?json.Value,
    method: []const u8,
    params_opt: ?json.Value,
    stream: RpcStream,

    const Self = @This();

    pub fn id(self: Self) json.Value {
        return self.request_id orelse json.Value.init("null");
    }

    pub fn params(self: Self, comptime T: type) ?T {
        if (self.params_opt == null) {
            return null;
        }
        else {
            var p = self.params_opt.?;
            return p.parse(T);
        }
    }

    pub fn respond(self: Self, result: anytype) !void {
        try self.stream.sendResult(self.id(), result);
    }

    pub fn respondNull(self: Self) !void {
        try self.stream.sendEmptyResult(self.id());
    }

    pub fn respondError(self: Self, code: ResponseError.Code, comptime format: []const u8, args: anytype) !void {
        try self.stream.sendError(self.id(), code, format, args);
    }
};

pub const Response = struct {
    request_id: ?json.Value,
    result: ?json.Value,
    @"error": ?ResponseError,

    const Self = @This();

    pub fn id(self: Self) json.Value {
        return self.request_id orelse json.Value.init("null");
    }
};

pub const Params = struct {
    value: ?json.Value,

    pub fn parse(self: Params, comptime T: type) ?T {
        if (self.value == null) {
            return null;
        }
        else {
            var v = self.value.?;
            return v.parse(T);
        }
    }
};

pub const ResponseError = struct {
    code: Code,
    message: []const u8,
    // data: ?json.Value,

    pub const Code = enum(int) {
        parse_error = -32700,
        invalid_request = -32600,
        method_not_found = -32601,
        invalid_params = -32602,
        internal_error = -32603,

        server_not_intialized = -32002,
        unknown_error_code = -32001,

        request_failed = -32803,
        server_cancelled = -32802,
        content_modified = -32801,
        request_cancelled = -32800,
        
        _,

        pub usingnamespace json.enumAsIntMixin(@This());

    };

    const Self = @This();

};

