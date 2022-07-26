const std = @import("std");

const assert = std.debug.assert;
// so i set out to make language server and i accidentally a whole json parser
// none of this has been tested yet lmao

pub fn comptimeEscapeString(comptime string: []const u8) []const u8 {
    comptime {
        var escaped: []const u8 = "";
        for (string) |c| {
            const append = switch (c) {
                '"' => "\\\"",
                '\\' => "\\\\",
                '/' => "\\/",
                0x08, => "\\b",
                0x0c, => "\\f",
                '\n' => "\\n",
                '\r' => "\\r",
                '\t' => "\\t",
                else => if (c < 128) &[_]u8{c} else "?",
            };
            escaped = escaped ++ append;
        }
        return escaped;
    }
}

pub const Tag = enum {
    object,
    array,
    string,
    number,
    boolean,
    null_value,

    const char_map: [256]?Tag = blk: {
        var map = std.mem.zeroes([256]?Tag);
        map['{'] = .object;
        map['['] = .array;
        map['"'] = .string;
        map['t'] = .boolean;
        map['f'] = .boolean;
        map['n'] = .null_value;
        for ("-0123456789") |c| {
            map[c] = .number;
        }
        break :blk map;
    };

};

pub const StringEscapeMode = enum {
    raw, unescaped,
};

const CharTable = [256]bool;

fn charTable(comptime class: []const u8) CharTable {
    comptime {
        var table = std.mem.zeroes(CharTable);
        for (class) |c| {
            table[c] = true;
        }
        return table;
    }
}


pub fn parse(json: []const u8, comptime T: type) ?Value.Read(T) {
    var value = Value.init(json);
    return value.parse(T);
}

/// walks a json source text in a static buffer, allowing for fast data extraction without allocations
pub const Value = struct {
    
    text: []const u8,
    tag: ?Tag = null,
    i: usize = 0,

    const Self = @This();

    pub fn init(text: []const u8) Self {
        var self = Self {
            .text = text,
        };
        _ = self.next();
        return self;
    }

    pub fn is(self: Self, tag: Tag) bool {
        return self.tag == tag;
    }

    fn fork(self: Self) Self {
        return init(self.text[self.i..]);
    }

    fn join(self: *Self, child: Value) void {
        const self_addr = @ptrToInt(&self.text[self.i]);
        const child_walker_addr = @ptrToInt(&child.text[self.i]);
        assert(child_walker_addr > self_addr);
        self.i += child_walker_addr - self_addr;
    }

    fn char(self: Self) ?u8 {
        if (self.i < self.text.len) {
            return self.text[self.i];
        }
        else {
            return null;
        }
    }

    pub fn next(self: *Self) ?Tag {
        if (self.i >= self.text.len) {
            self.i = self.text.len;
            return null;
        }
        self.tag = blk: {
            for (self.text[self.i..]) |c, len| {
                if (charTable("}]")[c]) {
                    self.i += len + 1;
                    break :blk null;
                }
                else if (Tag.char_map[c]) |tag| {
                    self.i += len;
                    switch (tag) {
                        .object, .array, .string => self.i += 1,
                        else => {},
                    }
                    break :blk tag;
                }
            }
            self.i = self.text.len;
            break :blk null;
        };
        return self.tag;
    }

    pub fn skipNext(self: *Self) void {
        if (self.next() != null) {
            self.skip();
        }
    }

    pub fn skip(self: *Self) void {
        if (self.tag) |tag| {
            switch (tag) {
                .object, .array => {
                    self.skipRest();
                },
                .string => {
                    while (self.nextCharRaw() != null) {}
                },
                .number => self.skipManyClass("-0123456789eE."),
                .boolean => self.skipManyClass("truefalse"),
                .null_value => self.skipManyClass("null"),
            }
        }
        self.tag = null;
    }

    pub fn skipRest(self: *Self) void {
        const text = self.text;
        var depth: usize = 1;
        while (depth > 0 and self.i < text.len) {
            self.skipUntilChars("\"[]{}");
            const c = self.char() orelse 0;
            if (c != '\"') {
                self.i += 1;
            }
            switch (c) {
                '[', '{' => depth += 1,
                ']', '}' => depth -= 1,
                '\"' => {
                    self.tag = .string;
                    self.skip();
                },
                else => {},
            }
        }
    }

    fn token(self: *Self) []const u8 {
        const start = switch (self.tag.?) {
            .object, .array, .string, => self.i - 1,
            else => self.i,
        };
        self.skip();
        return self.text[start..self.i];
    }

    fn Read(comptime T: type) type {
        if (comptime std.meta.trait.is(.Optional)(T)) {
            return std.meta.Child(T);
        }
        else {
            return T;
        }
    }


    pub fn parseNext(self: *Self, comptime T: type) ?Read(T) {
        if (self.next() != null) {
            return self.parse(T);
        }
        else {
            return null;
        }
    }

    pub fn parse(self: *Self, comptime T: type) ?Read(T) {
        comptime var is_parseable = false;
        const trait = std.meta.trait;
        if (comptime trait.is(.Optional)(T)) {
            is_parseable = true;
            return self.parse(Read(T));
        }
        if (self.tag) |tag| {
            switch (T) {
                Self => {
                    is_parseable = true;
                    const text = self.token();
                    var child = init(text);
                    return child;
                },
                else => {},
            }
            switch (tag) {
                .object => {
                    if (comptime trait.is(.Struct)(T) and !trait.isTuple(T)) {
                        is_parseable = true;
                        return self.parseStruct(T);
                    }
                    else {
                        return null;
                    }
                },
                .array => {
                    if (comptime trait.is(.Struct)(T) and trait.isTuple(T)) {
                        is_parseable = true;
                        return self.parseTuple(T);
                    }
                    else {
                        return null;
                    }
                },
                .string => if (comptime trait.isZigString(T)) {
                    is_parseable = true;
                    const str = self.token();
                    return str[1..str.len - 1];
                },
                .number => if (comptime trait.isNumber(T)) {
                    is_parseable = true;
                    switch (@typeInfo(T)) {
                        .Int => return std.fmt.parseInt(T, self.token(), 10) catch null,
                        .Float => return std.fmt.parseFloat(T, self.token()) catch null,
                        else => unreachable,
                    }
                },
                .boolean, => if (T == bool) {
                    is_parseable = true;
                    return self.token()[0] == 't';
                },
                .null_value => if (T == void) {
                    is_parseable = true;
                    self.skip();
                    return {};
                },
            }
        }
        if (!is_parseable) {
            @compileError("cannot parse " ++ @typeName(T) ++ ". consider using json.Value and parsing manually");
        }
        return null;
    }

    fn parseStruct(self: *Self, comptime S: type) S {
        const fields = std.meta.fields(S);
        const field_names: []const []const u8 = comptime blk: {
            var names: [fields.len][]const u8 = undefined;
            for (fields) |field, i| {
                names[i] = field.name;
            }
            break :blk &names;
        };
        const field_readers = fieldReaders(S);
        var value: S = .{};
        while (self.next() != null) {
            if (self.matchNames(field_names)) |i| {
                field_readers[i](self, &value);
            }
            else {
                self.skipNext();
            }
        }
        return value;
    }

    fn parseTuple(self: *Self, comptime T: type) T {
        const readers = FieldReaders(T);
        var value: T = .{};
        for (readers) |reader| {
            if (self.next() != null) {
                reader(self, &value);
            }
        }
        // self.skipRest();
        return value;
    }


    fn FieldReader(comptime S: type) type {
        return fn (*Self, *S) void;
    }

    fn FieldReaders(comptime S: type) type {
        return [std.meta.fields(S).len]FieldReader(S);
    }

    fn fieldReader(comptime S: type, comptime name: []const u8, comptime F: type) FieldReader(S) {
        return struct {
            fn f(self: *Self, value: *S) void {
                if (self.parseNext(F)) |field_value| {
                    @field(value, name) = field_value;
                }
            }
        }.f;
    }

    fn fieldReaders(comptime S: type) FieldReaders(S) {
        comptime {

            var readers: FieldReaders(S) = undefined;
            for (std.meta.fields(S)) |field, i| {
                readers[i] = fieldReader(S, field.name, field.field_type);
            }
            return readers;
        }
    }


    pub fn matchName(self: *Self, comptime name: []const u8) bool {
        const escaped_name = comptimeEscapeString(name);
        var i: usize = 0;
        while (self.nextCharRaw()) |c| {
            if (escaped_name[i] != c) {
                self.rest();
                return false;
            }
        }
        if (i == escaped_name.len) {
            return true;
        }
        else {
            return false;
        }
    }

    pub fn matchNames(self: *Self, comptime names: []const []const u8) ?usize {
        const map = comptime blk: {
            const Pair = std.meta.Tuple(&.{[]const u8, usize});
            var pairs: [names.len]Pair = undefined;
            for (names) |name, i| {
                pairs[i] = .{ comptimeEscapeString(name), i };
            }
            break :blk std.ComptimeStringMap(usize, pairs);
        };
        if (self.parse([]const u8)) |str| {
            if (map.get(str)) |index| {
                return index;
            }
            else {
                return null;
            }
        }
        else {
            return null;
        }

    }

    fn nextCharMode(self: *Self, comptime mode: StringEscapeMode) ?u8 {
        const text = self.text;
        if (self.i < text.len) {
            const c = text[self.i];
            self.i += 1;
            switch (mode) {
                .raw => {
                    if (c == '"' and text[self.i - 2] != '\\') {
                        return null;
                    }
                    else {
                        return c;
                    }
                },
                .unescaped => {
                    switch (c) {
                        '"' => {
                            return null;
                        },
                        '\\' => {
                            if (self.char()) |code| {
                                self.i += 1;
                                return switch (code) {
                                    '"', '\\', '/' => code,
                                    'b' => 0x08,
                                    'f' => 0x0c,
                                    'n' => '\n',
                                    'r' => '\r',
                                    't' => '\t',
                                    else => '?',
                                };
                            }
                            else {
                                return null;
                            }
                        },
                        else => return c,
                    }
                },
            }
        }
        else {
            return null;
        }
    }

    fn nextCharRaw(self: *Self) ?u8
        { return self.nextCharMode(.raw); }

    fn nextChar(self: *Self) ?u8
        { return self.nextCharMode(.unescaped); }

    fn stringRest(self: *Self) void {
        while (self.nextCharMode(.raw) != null) {}
    }




    pub fn lenManyClass(text: []const u8, comptime class: []const u8) usize {
        return lenManyTable(text, comptime charTable(class));
    }

    pub fn lenManyTable(text: []const u8, comptime table: CharTable) usize {
        for (text) |c, len| {
            if (!table[c]) {
                return len;
            }
        }
        return text.len;
    }

    pub fn lenUntilClass(text: []const u8, comptime class: []const u8) usize {
        return lenUntilTable(text, comptime charTable(class));
    }

    pub fn lenUntilTable(text: []const u8, comptime table: CharTable) usize {
        for (text) |c, len| {
            if (table[c]) {
                return len;
            }
        }
        return text.len;
    }

    pub fn skipManyClass(self: *Self, comptime class: []const u8) void {
        self.skipManyTable(comptime charTable(class));
    }
    pub fn skipManyTable(self: *Self, comptime table: CharTable) void {
        const len = lenManyTable(self.text[self.i..], table);
        self.i += len;
    }

    pub fn skipUntilChars(self: *Self, comptime class: []const u8) void {
        self.skipUntilTable(comptime charTable(class));
    }
    pub fn skipUntilTable(self: *Self, comptime table: CharTable) void {
        const len = lenUntilTable(self.text[self.i..], table);
        self.i += len;
    }

    pub fn readManyChars(self: *Self, comptime class: []const u8) []const u8 {
        return self.readManyTable(comptime charTable(class));
    }
    pub fn readManyTable(self: *Self, comptime table: CharTable) []const u8 {
        const start = self.i;
        const len = lenManyTable(self.text[self.i..], table);
        self.i += len;
        return self.text[start..self.i];
    }

    pub fn readUntilChars(self: *Self, comptime class: []const u8) []const u8 {
        return self.readUntilTable(comptime charTable(class));
    }
    pub fn readUntilTable(self: *Self, comptime table: CharTable) []const u8 {
        const start = self.i;
        const len = lenUntilTable(self.text[self.i..], table);
        self.i += len;
        return self.text[start..self.i];
    }

    pub fn dump(self: *Self, writer: anytype) !void {
        try self.dumpRecr(writer, 1);
        try writer.writeByte('\n');
    }

    fn dumpRecr(self: *Self, writer: anytype, depth: usize) @TypeOf(writer).Error!void {
        if (self.next()) |tag| {
            switch (tag) {
                .object => {
                    try writer.writeAll("{\n");
                    var i: usize = 0;
                    while (self.next() != null) : (i += 1) {
                        if (i > 0) try writer.writeAll(",\n");
                        try writer.writeByteNTimes(' ', depth * 2);
                        try self.dumpRecr(writer, depth + 1);
                        try writer.writeAll(": ");
                        try self.dumpRecr(writer, depth + 1);
                    }
                    try writer.writeByte('\n');
                    try writer.writeByteNTimes(' ', (depth - 1) * 2);
                    try writer.writeByte('}');
                },
                .array => {
                    try writer.writeAll("[\n");
                    var i: usize = 0;
                    while (self.next() != null) : (i += 1) {
                        if (i > 0) try writer.writeAll(",\n");
                        try writer.writeByteNTimes(' ', depth * 2);
                        try self.dumpRecr(writer, depth + 1);
                    }
                    try writer.writeByte('\n');
                    try writer.writeByteNTimes(' ', (depth - 1) * 2);
                    try writer.writeByte(']');
                },
                .string => {
                    try writer.writeByte('\"');
                    while (self.nextChar()) |c| {
                        try writer.writeByte(c);
                    }
                    try writer.writeByte('\"');
                },
                .number => {
                    if (self.parse(f64)) |n| {
                        try writer.print("{d}\n", .{n});
                    }
                },
                .boolean => {
                    if (self.parse(bool)) |b| {
                        try writer.print("{}\n", .{b});
                    }
                },
                .null_value => {
                    self.skip();
                    try writer.writeAll("null\n");
                },
            }
        }
    }

};