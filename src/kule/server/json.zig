const std = @import("std");

const Allocator = std.mem.Allocator;

pub fn writeAlloc(allocator: Allocator, value: anytype) ![]const u8 {
    var list = std.ArrayList(u8).init(allocator);
    try write(list.writer(), value);
    return list.toOwnedSlice();
}

pub fn write(writer: anytype, value: anytype) !void {
    const Writer = JsonWriter(@TypeOf(writer));
    var json_writer = Writer.init(writer);
    try json_writer.writeValue(value);
}

pub fn JsonWriter(comptime Writer: type) type {

    return struct {

        out: Writer,

        const Error = Writer.Error;

        const Self = @This();

        pub fn init(out: Writer) Self {
            return Self {
                .out = out,
            };
        }

        pub fn writeValue(self: Self, value: anytype) Error!void {
            const out = self.out;
            const Value = @TypeOf(value);
            if (comptime std.meta.trait.isNumber(Value)) {
                try out.print("{d}", .{value});
            }
            else if (comptime std.meta.trait.isZigString(Value)) {
                try self.writeString(value);
            }
            else switch (@typeInfo(Value)) {
                .Bool => {
                    if (value) {
                        try out.writeAll("true");
                    }
                    else {
                        try out.writeAll("false");
                    }
                },
                .Optional => {
                    if (value != null) {
                        try self.writeValue(value.?);
                    }
                },
                .Struct => |Struct| {
                    if (Struct.is_tuple) {
                        try self.writeTuple(value);
                    }
                    else {
                        try self.writeStruct(value);
                    }
                },
                .Array => |Array| {
                    try self.writeArray(Array.child, &value);
                },
                .Pointer => |Pointer| {
                    switch (Pointer.size)  {
                        .Slice => try self.writeArray(Pointer.child, value),
                        else => try self.writeValue(value.*),
                    }
                },
                else => @compileError("cant write " ++ @typeName(Value) ++ " yet"),
            }
        }

        fn writeString(self: Self, value: []const u8) Error!void {
            try self.out.print("\"{s}\"", .{value});
        }

        fn writeStruct(self: Self, value: anytype) Error!void{
            const out = self.out;
            const Value = @TypeOf(value);
            try out.writeByte('{');
            var i: usize = 0;
            inline for (comptime std.meta.fields(Value)) |field| {
                if (i > 0) {
                    try out.writeByte(',');
                }
                const item = @field(value, field.name);
                if (@typeInfo(field.field_type) != .Optional or item != null) {
                    try self.writeString(field.name);
                    try out.writeByte(':');
                    try self.writeValue(item);
                    i += 1;
                }
            }
            try out.writeByte('}');
        }

        fn writeTuple(self: Self, value: anytype) Error!void {
            const out = self.out;
            const Value = @TypeOf(value);
            try out.writeByte('[');
            var i: usize = 0;
            inline for (comptime std.meta.fields(Value)) |field| {
                if (i > 0) {
                    try out.writeByte(',');
                }
                const item = @field(value, field.name);
                if (@typeInfo(field.field_type) != .Optional or item != null) {
                    try self.writeValue(item);
                    i += 1;
                }
            }
            try out.writeByte(']');
        }

        fn writeArray(self: Self, comptime T: type, array: []const T) Error!void {
            const out = self.out;
            try out.writeByte('[');
            var i: usize = 0;
            for (array) |item| {
                if (i > 0) {
                    try out.writeByte(',');
                }
                if (@typeInfo(T) != .Optional or item != null) {
                    try self.writeValue(item);
                    i += 1;
                }
            }
            try out.writeByte(']');
        }

    };

}

pub const JsonReader = struct {
    allocator: Allocator,
    source: []const u8,
    i: usize = 0,
    stream: Stream,
    next_token: ?Token = null,

    const Stream = std.json.StreamingParser;
    const JsonToken = std.json.Token;
    const JsonTokenTag = std.meta.Tag(JsonToken);
    
    const Token = struct {
        
        json_token: JsonToken,
        end: usize,

    };

    fn tokenString(self: Self, token: Token) []const u8 {
        return token.json_token.String.slice(self.source, token.end-1);
    }

    fn tokenNumberString(self: Self, token: Token) []const u8 {
        return token.json_token.Number.slice(self.source, token.end-1);
    }

    fn tokenNumberUsize(self: Self, token: Token) usize {
        const text = self.tokenNumberString(token);
        var n: usize = 0;
        for (text) |c| {
            n = n * 10 + (c - '0');
        }
        return n;
    }

    const Error = error {
        UnexpectedToken,
        OutOfTokens,
    } || Stream.Error;

    const Self = @This();

    pub fn init(allocator: Allocator, src: []const u8) Self {
        return Self {
            .allocator = allocator,
            .source = src,
            .stream = Stream.init(),
        };
    }

    pub fn deinit(self: Self) void {
        _ = self;
    }

    pub fn reset(self: *Self, src: []const u8) void {
        self.source = src;
        self.stream.reset();
        self.i = 0;
        self.next_token = null;
    }

    fn nextToken(self: *Self) Error!?Token {
        const token = blk: {
            if (self.next_token) |token| {
                self.next_token = null;
                break :blk token;
            }
            else {
                const tokens = try self.readTokens();
                const token = tokens[0];
                self.next_token = tokens[1];
                break :blk token;
            }
        };
        // if (token) |t|{
        //     switch (t.json_token) {
        //         .String => std.log.debug("{s}: {s}", .{@tagName(t.json_token), self.tokenString(t)}),
        //         .Number => std.log.debug("{s}: {d}", .{@tagName(t.json_token), self.tokenNumberUsize(t)}),
        //         else => std.log.debug("{s}", .{@tagName(t.json_token)}),
        //     }
        // }
        return token;
    }

    fn readTokens(self: *Self) Error![2]?Token {
        var jts: [2]?JsonToken = .{null, null};
        if (self.i >= self.source.len) {
            return [2]?Token{ null, null };
        }
        while (jts[0] == null) : (self.i += 1) {
            try self.stream.feed(self.source[self.i], &jts[0], &jts[1]);
            if (self.i >= self.source.len) {
                break;
            }
        }
        return [2]?Token {
            if (jts[0]) |json_token| Token {
                .json_token = json_token,
                .end = self.i,
            } else null,
            if (jts[1]) |json_token| Token {
                .json_token = json_token,
                .end = self.i,
            } else null,
        };

    }

    fn expect(self: *Self, comptime tag: JsonTokenTag) Error!Token {
        if (try self.nextToken()) |token| {
            if (token.json_token != tag) {
                // std.log.debug("   expected {s}, found {s} {d}", .{ @tagName(tag), @tagName(token.json_token), token.end});
                return Error.UnexpectedToken;
            }
            else {
                return token;
            }
        }
        else {
            return Error.OutOfTokens;
        }
    }

    pub fn beginObject(self: *Self) Error!void {
        _ = try self.expect(.ObjectBegin);
    }

    fn FieldRead(comptime T: type) type{
        return fn(*T, *Self) Error!void;
    } 

    fn fieldRead(comptime T: type, comptime field_name: []const u8, comptime field_type: type) FieldRead(T) {
        switch (@typeInfo(field_type)) {
            .Optional => |Optional| return fieldRead(T, field_name, Optional.child),
            .Struct => return struct {
                fn f(v: *T, s: *Self) Error!void {
                    try s.beginObject();
                    @field(v.*, field_name) = try s.readObject(field_type);
                }
            }.f,
            else => {},
        }
        switch (field_type) {
            usize => return struct {
                fn f(v: *T, s: *Self) Error!void {
                    const token = try s.expect(.Number);
                    @field(v.*, field_name) = s.tokenNumberUsize(token);
                }
            }.f,
            []const u8 => return struct {
                fn f(v: *T, s: *Self) Error!void {
                    const token = try s.expect(.String);
                    @field(v.*, field_name) = s.tokenString(token);
                }
            }.f,
            bool => return struct {
                fn f(v: *T, s: *Self) Error!void {
                    if (try s.nextToken()) |token| {
                        switch (token.json_token) {
                            .True => @field(v.*, field_name) = true,
                            .False => @field(v.*, field_name) = false,
                            else => return Error.UnexpectedToken,
                        }
                    }
                }
            }.f,
            else => |F| {
                @compileError("cannot parse " ++ @typeName(F) ++ " yet");
            },
        }
    }

    fn fieldReadMap(comptime T: type) type {
        const Pair = std.meta.Tuple(&.{[]const u8, FieldRead(T)});
        var pairs: []const Pair = &.{};
        for (std.meta.fields(T)) |field| {
            const reader = fieldRead(T, field.name, field.field_type);
            const pair = Pair { field.name, reader };
            pairs = pairs ++ &[_]Pair { pair };
        }
        return std.ComptimeStringMap(FieldRead(T), pairs);
    }

    pub fn readObjectToField(self: *Self, comptime field_name: []const u8, comptime field_type: type) Error!?field_type {
        const T = @Type(.{
            .Struct = .{
                .layout = .Auto,
                .fields = &.{
                    .{
                        .name = field_name,
                        .field_type = field_type,
                        .default_value = null,
                        .is_comptime = false,
                        .alignment = @alignOf(field_type),
                    },
                },
                .decls = &.{},
                .is_tuple = false,
            },
        });
        const read = fieldRead(T, field_name, field_type);
        var value = comptime std.mem.zeroes(T);
        while (try self.nextToken()) |token| {
            switch (token.json_token) {
                .ObjectEnd => return null,
                .String => {
                    if (std.mem.eql(u8, self.tokenString(token), field_name)) {
                        // std.log.debug("    found " ++ field_name ++ " field. {s}", .{ @tagName(token.json_token)});
                        try read(&value, self);
                        return @field(value, field_name);
                    }
                    else {
                        try self.skipValue();
                    }
                },
                else => return Error.UnexpectedToken,
            }
        }
        return Error.OutOfTokens;
    }

    pub fn readObject(self: *Self, comptime T: type) Error!T {
        const map = comptime fieldReadMap(T);
        const field_count = comptime std.meta.fields(T).len;
        var value = comptime std.mem.zeroes(T);
        var n: usize = 0;
        while (try self.nextToken()) |token| {
            // std.log.debug("   name token: {s} {d}", .{ @tagName(token.json_token), token.end});
            switch (token.json_token) {
                .ObjectEnd => {
                    return value;
                },
                .String => {
                    const name = self.tokenString(token);
                    if (map.get(name)) |read| {
                        try read(&value, self);
                        n += 1;
                        if (n == field_count) {
                            return value;
                        }
                    }
                    else {
                        try self.skipValue();
                    }
                },
                else => {
                    // std.log.debug("   unexpected token {s} {d}", .{ @tagName(token.json_token), token.end});
                    return Error.UnexpectedToken;
                },
            }
        }
        return value;
    }

    fn skipValue(self: *Self) Error!void {
        if (try self.nextToken()) |token| {
            switch (token.json_token) {
                .ArrayBegin => try self.skipArrayBody(),
                .ObjectBegin => try self.skipObjectBody(),
                else => {},
            }
        }
    }

    fn skipArrayBody(self: *Self) Error!void {
        while (try self.nextToken()) |token| {
            switch (token.json_token) {
                .ArrayBegin => try self.skipArrayBody(),
                .ObjectBegin => try self.skipObjectBody(),
                .ArrayEnd => return,
                else => {},
            }
        }
    }

    fn skipObjectBody(self: *Self) Error!void {
        while (try self.nextToken()) |token| {
            switch (token.json_token) {
                .ObjectEnd => return,
                .String => try self.skipValue(),
                else => return error.UnexpectedToken,
            }
        }
    }

};