const std = @import("std");

const Allocator = std.mem.Allocator;
const File = std.fs.File;

const Tuple = std.meta.Tuple;

pub const Integer = i32;
pub const UInteger = u32;

pub const Id = union(enum) {
    integer: Integer,
    string: []const u8,
    null_value: void,
};

pub const MessageHeader = struct {
    id: ?usize = null,
    method: ?[]const u8 = null,
    has_params: bool = false,

    pub fn format(self: MessageHeader, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.writeAll("\n[header]\n");
        try writer.writeAll(" id: ");
        if (self.id) |id|{
            try writer.print("{d}\n", .{id});
        }
        else {
            try writer.writeAll("(none)\n");
        }
        try writer.writeAll(" method: ");
        if (self.method) |method| {
            try writer.print("{s}\n", .{method});
        }
        else {
            try writer.writeAll("(none)\n");
        }
        try writer.print(" has_params: {}\n", .{self.has_params});
    }
};

pub fn fileReader(file: File) RpcReader(File.Reader) {
    return reader(file.reader());
}

pub fn reader(input_reader: anytype) RpcReader(@TypeOf(input_reader)) {
    return RpcReader(@TypeOf(input_reader)).init(input_reader);
}

pub fn RpcReader(comptime Reader: type) type {
    return struct {
        c: u8 = 0,
        reader: Reader,
        
        pub const RpcError = Reader.Error || error {EndOfStream};

        const Self = @This();

        pub fn init(input: Reader) Self {
            return Self {
                .reader = input,
            };
        }

        pub fn readHeader(r: *Self, method_buf: []u8) !MessageHeader {
            var header = MessageHeader{};
            errdefer |err| {
                std.log.err("error parsing rpc header: {s}{}", .{@errorName(err), header});
            }
            try r.toNextSeq(&.{'{', '"'});
            while (true) {
                try r.adv();
                switch (r.c) {
                    // skip "jsonrpc": "2.0", to the first " of the adv field's name
                    'j' => {
                        try r.toNext(',');
                    },
                    // id (if present)
                    'i' => {
                        _ = try r.toNextOf("1234567890");
                        header.id = try r.readUnsigned();
                    },
                    // method name
                    'm' => {
                        try r.toNextSeq(&.{':', '"'});
                        if (try r.readStringBufRaw(method_buf)) |method| {
                            header.method = method;
                        }
                    },
                    // params
                    // dont actually parse params, the layout depends on the method
                    // leave the reader with its last character read being the opening '{'
                    'p' => {
                        header.has_params = true;
                        try r.toNext('{');
                        return header;
                    },
                    else => {
                        return error.UnexpectedSymbol;
                    },
                }
                if ((try r.toNextOf(&.{'"', '}'})) == '}') {
                    return header;
                }
            }
            return header;
        }

        fn adv(r: *Self) RpcError!void {
            r.c = try r.reader.readByte();
        }

        fn toNext(r: *Self, comptime c: u8) RpcError!void {
            while (r.c != c) : (try r.adv()) {}
        }

        fn toNextSeq(r: *Self, comptime seq: []const u8) RpcError!void {
            inline for (seq) |c| {
                try r.toNext(c);
            }
        }

        fn toNextOf(r: *Self, comptime chars: []const u8) RpcError!u8 {
            const table = comptime charTable(chars);
            while (!table[r.c]) : (try r.adv()) {}
            return r.c;
        }

        fn toNextNotOf(r: *Self, comptime chars: []const u8) RpcError!u8 {
            const table = comptime charTable(chars);
            while (table[r.c]) : (try r.adv()) {}
            return r.c;
        }

        fn charTable(comptime chars: []const u8) [256]bool {
            var table = std.mem.zeroes([256]bool);
            for (chars) |c| {
                table[c] = true;
            }
            return table;
        }

        fn charIsOf(r: *Self, comptime chars: []const u8) ?u8 {
            const table = comptime charTable(chars);
            if (table[r.c]) {
                return r.c;
            }
            else {
                return null;
            }
            return table[r.c];
        }

        pub fn dumpRemaining(r: *Self, out: anytype) !void {
            while (true) {
                try out.writeByte(r.c);
                r.adv() catch return;
            }
            return;
        }

        pub const ValueType = enum {
            object,
            array,
            string,
            number,
            boolean,
            null_value,
        };

        fn getNextValueType(r: *Self) RpcError!ValueType {
            return switch(try r.toNextValue()) {
                '{' => .object,
                '[' => .array,
                '"' => .string,
                '-', '0'...'9' => .number,
                't', 'f' => .boolean,
                'n' => .null_value,
                else => unreachable,
            };
        }

        fn toNextValue(r: *Self) RpcError!u8 {
            return r.toNextOf("{[\"-0123456789tfn");
        }

        fn toNextFieldEnd(r: *Self) RpcError!u8 {
            return r.toNextOf(&.{',', '}', ']' });
        }

        fn skipNextValue(r: *Self) RpcError!void {
            switch(try r.toNextValue()) {
                '{' => try r.skipObject(),
                '[' => try r.skipArray(),
                '"' => try r.skipString(),
                '-', '0'...'9' => try r.toNextNotOf("-+eE.0123456789"),
                't', 'f', 'n' => try r.toNextNotOf("truefalsn"),
                else => unreachable,
            }
        }

        fn skipString(r: *Self) RpcError!void {
            try r.adv();
            try r.exitString();
        }

        fn exitString(r: *Self) RpcError!void {
            while (r.c != '"') : (try r.adv()) {
                if (r.c == '\\') {
                    try r.adv();
                }
            }
            try r.adv();
        }

        pub fn skipObject(r: *Self) RpcError!void {
            try r.skipBalanced('{', '}');
        }

        pub fn skipArray(r: *Self) RpcError!void {
            try r.skipBalanced('[', ']');
        }

        pub fn exitObject(r: *Self) RpcError!void {
            try r.exitBalanced('{', '}');
        }

        pub fn exitArray(r: *Self) RpcError!void {
            try r.exitBalanced('[', ']');
        }

        fn skipBalanced(r: *Self, comptime lbrack: u8, comptime rbrack: u8) RpcError!void {
            try r.adv();
            try r.exitBalanced(lbrack, rbrack);
        }

        fn exitBalanced(r: *Self, comptime lbrack: u8, comptime rbrack: u8) RpcError!void {
            _ = lbrack;
            while (r.c != rbrack) {
                switch (r.c) {
                    '{' => try r.skipObject(),
                    '[' => try r.skipArray(),
                    '"' => try r.skipString(),
                    else => try r.adv(), 
                }
            }
            // std.log.info("{d: >3}: {c}", .{depth, rbrack});
            try r.adv();
        }

        fn readSigned(r: *Self) RpcError!Integer {
            if (r.c == '-') {
                try r.adv();
                return -@intCast(Integer, try r.readUnsigned());
            }
            else {
                return @intCast(Integer, try r.readUnsigned());
            }
        }

        fn readUnsigned(r: *Self) RpcError!UInteger {
            var value: UInteger = 0;
            while (std.ascii.isDigit(r.c)) : (try r.adv()) {
                value = value * 10 + (r.c - '0');
            }
            return value;
        }

        fn readBoolean(r: *Self) RpcError!?bool {
            const result: ?bool = switch (r.c) {
                't' => true,
                'f' => false,
                else => return null,
            };
            _ = try r.toNextFieldEnd();
            return result;
        }

        fn readNull(r: *Self) RpcError!bool {
            switch (r.c) {
                'n' => {
                    _ = try r.toNextFieldEnd();
                    return true;
                },
                else => return false,
            }
        }

        fn readStringBufRaw(r: *Self, buf: []u8) RpcError!?[]const u8 {
            return r.readStringBuf(buf, false);
        }

        fn readStringBufEscaped(r: *Self, buf: []u8) RpcError!?[]const u8 {
            return r.readStringBuf(buf, true);
        }

        fn readStringBuf(r: *Self, buf: []u8, comptime process_escapes: bool) RpcError!?[]const u8 {
            var esc = try r.startJsonString();
            var i: usize = 0;
            while (try r.nextJsonStringChar(&esc, process_escapes)) |c| {
                if (i < buf.len) {
                    buf[i] = c;
                    i += 1;
                }
                else {
                    break;
                }
            }
            if (i == buf.len) {
                if ((try r.nextJsonStringChar(&esc, process_escapes)) != null) {
                    while (try r.nextJsonStringChar(&esc, process_escapes)) |_| {}
                    return null;
                }
            }
            return buf[0..i];
        }

        const AllocatorRpcError = Allocator.Error || RpcError;

        fn readStringAllocRaw(r: *Self, allocator: Allocator) AllocatorRpcError![]const u8 {
            return r.readStringAlloc(allocator, false);
        }

        fn readStringAllocEscaped(r: *Self, allocator: Allocator) AllocatorRpcError![]const u8 {
            return r.readStringAlloc(allocator, true);
        }

        fn readStringAlloc(r: *Self, allocator: Allocator, comptime process_escapes: bool) AllocatorRpcError![]const u8 {
            var esc = try r.startJsonString();
            var list = std.ArrayListUnmanaged(u8){};
            while (try r.nextJsonStringChar(&esc, process_escapes)) |c| {
                try list.append(allocator, c);
            }
            return list.toOwnedSlice(allocator);
        }

        fn startJsonString(r: *Self) RpcError!bool {
            try r.adv();
            return r.c == '\\';
        }

        fn nextJsonStringChar(r: *Self, is_in_esc: *bool, comptime process_escapes: bool) RpcError!?u8 {
            var esc = is_in_esc.*;
            defer is_in_esc.* = esc;
            if (esc or r.c != '"') {
                esc = !esc and r.c == '\\';
                const result = blk: {
                    if (process_escapes) {
                        if (esc) {
                            try r.adv();
                            break :blk switch (r.c) {
                                '"', '\\', '/' => r.c,
                                'b' => 0x08,
                                'f' => 0x0c,
                                'n' => '\n',
                                'r' => '\r',
                                't' => '\t',
                                else => {
                                    std.log.err("invalid json escape character {c}", .{r.c});
                                    break :blk '?';
                                },
                            };
                        }
                    }
                    break :blk r.c;
                };
                try r.adv();
                return result;
            }
            else {
                try r.adv();
                return null;
            }
        }

        fn enterString(r: *Self) RpcError!bool {
            return (try r.toNextValue()) == '"';
        }

        fn enterObject(r: *Self) RpcError!bool {
            return (try r.toNextValue()) == '{';
        }

        fn enterArray(r: *Self) RpcError!bool {
            return (try r.toNextValue()) == '[';
        }

        fn enterField(r: *Self) RpcError!bool {
            if ((try r.toNextFieldEnd()) == ',') {
                try r.toNextValue();
                return true;
            }
            else {
                return false;
            }
        }

        fn comptimeJsonEscapeString(comptime string: []const u8) []const u8 {
            @setEvalBranchQuota(10000);
            var escaped: []const u8 = "";
            for (string) |c| {
                if (c >= 128) {
                    @compileError("comptime json unicode escapes are not supported");
                }
                const append: []const u8 = (
                    switch (c) {
                        '"', '\\', '/' => &[_]u8 {'\\', c},
                        0x08 => "\\b",
                        0x0c => "\\f",
                        '\n' => "\\n",
                        '\r' => "\\r",
                        '\t' => "\\t",
                        else => &[_]u8{c},
                    }
                );
                escaped = escaped ++ append;
            }
            return escaped;
        }

        fn matchStringValue(r: *Self, comptime string: []const u8) RpcError!bool {
            const escaped = comptime comptimeJsonEscapeString(string);
            try r.adv();
            for (escaped) |c| {
                if (c != r.c) {
                    try r.exitString();
                    return false;
                }
                try r.adv();
            }
            if (r.c != '"') {
                try r.exitString();
                return false;
            }
            try r.adv();
            return true;
        }


        fn MapPair(comptime V: type) type {
            return Tuple(&.{[]const u8, V});
        }

        fn matchStringValueMap(r: *Self, comptime V: type, comptime map_pairs: []const MapPair(V)) RpcError!?V {
            const Pair = MapPair(V);
            const escaped_pairs: []const Pair = blk: {
                var escaped: [map_pairs.len]Pair = undefined;
                for (map_pairs) |pair, i| {
                    escaped[i] = .{
                        comptimeJsonEscapeString(pair.@"0"),
                        pair.@"1",
                    };
                }
                break :blk escaped;
            };
            const map = std.ComptimeStringMap(V, escaped_pairs);
            const buf_len = map.kvs[map.kvs.len - 1].key.len;
            var buf: [buf_len]u8 = undefined;
            if (try r.readStringBufRaw(buf)) |string| {
                return map.get(string);
            }
            else {
                return null;
            }
        }

        fn readValue(r: *Self, comptime T: type, allocator: Allocator) AllocatorRpcError!?T {
            switch (T) {
                []const u8 => return try r.readStringAllocEscaped(allocator),
                Integer => return try r.readSigned(),
                UInteger => return try r.readUnsigned(),
                bool => return try r.readBoolean(),
                void => {
                    if (try r.readNull()) {
                        return {};
                    }
                    else {
                        return null;
                    }
                },
                else => {},
            }
            switch (@typeInfo(T)) {
                .Union => |Union| {
                        
                },
            }
        }


    };
}


const testing = std.testing;

const TestStream = std.io.FixedBufferStream([]const u8);
const TestReader = struct {
    stream: TestStream,
    fn rpcReader(tr: *TestReader) RpcReader(TestStream.Reader) {
        return reader(tr.stream.reader());
    }
};
fn testReader(text: []const u8) TestReader {
    return TestReader {
        .stream = TestStream {
            .buffer = text,
            .pos = 0,
        },
    };
}

fn test_nextJsonStringChar(comptime input: []const u8, comptime expecteds: []const []const u8, comptime process_escapes: bool) !void {
    var tr = testReader(input);
    var r = tr.rpcReader();
    for (expecteds) |expected| {
        try r.toNext('"');
        var esc = try r.startJsonString();
        var i: usize = 0;
        while (try r.nextJsonStringChar(&esc, process_escapes)) |c| {
            try testing.expectEqualStrings(expected[i..i+1], &[_]u8{c});
            i += 1;
        }
        try testing.expectEqual(i, expected.len);
    }

}

test "RpcReader.nextJsonStringChar (raw)" {
    try test_nextJsonStringChar(
        \\some stuff {}
        \\} "a string" 35
        \\some more things
        \\ { "a \\str\ning with \"escapes"}
        , &.{"a string", "a \\\\str\\ning with \\\"escapes"},
        false,
    );
}

test "RpcReader.nextJsonStringChar (escaped)" {
    try test_nextJsonStringChar(
        \\some stuff {}
        \\} "a string" 35
        \\some more things
        \\ { "a \\str\ning with \"escapes"}
        , &.{"a string", "a \\str\ning with \"escapes"},
        true,
    );
}


