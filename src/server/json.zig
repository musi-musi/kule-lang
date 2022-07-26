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

const char_tables = struct {
    const numerical = charTable("-0123456789eE.");
};


pub fn walk(json: []const u8) Walker {
    return Walker.init(json);
}

/// walks a json source text in a static buffer, allowing for fast data extraction without allocations
pub const Walker = struct {
    
    text: []const u8 = "",
    tag: ?Tag = null,
    i: usize = 0,

    const Self = @This();

    pub fn init(text: []const u8) Self {
        return Self {
            .text = text,
        };
    }

    pub fn fork(self: Self) Self {
        return init(self.text[self.i..]);
    }

    pub fn forkFields(self: Self) Self {
        var child = self;
        child.fields();
        return child.fork();
    }

    pub fn join(self: *Self, child: Walker) void {
        const self_addr = @ptrToInt(&self.text[self.i]);
        const child_walker_addr = @ptrToInt(&child.text[self.i]);
        assert(child_walker_addr > self_addr);
        self.i += child_walker_addr - self_addr;
    }

    pub fn joinRest(self: *Self, child: Walker) void {
        self.join(child);
        self.rest();
    }

    pub fn char(self: Self) ?u8 {
        if (self.i < self.text.len) {
            return self.text[self.i];
        }
        else {
            return null;
        }
    }

    pub fn next(self: *Self) ?Tag {
        self.tag = blk: {
            for (self.text[self.i..]) |c, len| {
                if (charTable("}]")[c]) {
                    self.i += len + 1;
                    break :blk null;
                }
                else if (Tag.char_map[c]) |t| {
                    self.i += len;
                    break :blk t;
                }
            }
            self.i = self.text.len;
            break :blk null;
        };
        return self.tag;
    }

    pub fn skip(self: *Self) void {
        if (self.tag) |t| {
            switch (t) {
                .object, .array => {
                    self.fields();
                    self.rest();
                },
                .string => {
                    self.i += 1;
                    self.stringRest();
                },
                else => {},
            }
        }
        self.skipUntilChars(":,}]");
    }

    pub fn skipNext(self: *Self) void {
        if (self.next() != null) {
            self.skip();
        }
    }

    pub fn fields(self: *Self) void {
        self.i += 1;
    }

    pub fn nextFields(self: *Self) bool {
        switch (self.next() orelse .null_value) {
            .object, .array => {
                self.fields();
                return true;
            },
            else => {
                return false;
            },
        }
    }

    pub fn rest(self: *Self) void {
        const text = self.text;
        var depth: usize = 1;
        while (depth > 0 and self.i < text.len) {
            self.skipUntilChars("\"[]{}");
            switch (self.char() orelse 0) {
                '[', '{' => depth += 1,
                ']', '}' => depth -= 1,
                '\"' => {
                    self.i += 1;
                    self.stringRest();
                },
                else => {},
            }
            self.i += 1;
        }
    }

    pub fn readNext(self: *Self, comptime T: type) ?T {
        if (self.next() != null)  {
            return self.read(T);
        }
        else {
            return null;
        }
    }

    pub fn read(self: *Self, comptime T: type) ?T {
        const trait = std.meta.trait;
        if (self.tag) |t| {
            if (T == Self)  {
                switch (t) {
                    .object, .array => {
                        const child = self.forkFields();
                        self.skip();
                        return child;
                    },
                    else => return null,
                }
            }
            switch (t) {
                .object => {
                    return null;
                },
                .array => {
                    return null;
                },
                .string => if (comptime trait.isZigString(T)) {
                    self.i += 1;
                    const start = self.i;
                    while (self.stringNextRaw() != null) {}
                    const str = self.text[start..(self.i - 1)];
                    self.i += 1;
                    return str;
                },
                .number => if (comptime trait.isNumber(T)) {
                    const token = self.readManyTable(char_tables.numerical);
                    switch (@typeInfo(T)) {
                        .Int => return std.fmt.parseInt(T, token, 10) catch null,
                        .Float => return std.fmt.parseFloat(T, token) catch null,
                        else => unreachable,
                    }
                },
                .boolean, => if (T == bool) {
                    const token = self.readManyChars("truefalse");
                    return token[0] == 't';
                },
                .null_value => if (T == void) {
                    self.skipManyClass("null");
                    return {};
                },
            }
        }
        return null;
    }


    pub fn matchName(self: *Self, comptime name: []const u8) bool {
        const escaped_name = comptimeEscapeString(name);
        var str = self.stringRaw();
        var i: usize = 0;
        while (str.next()) |c| {
            if (escaped_name[i] != c) {
                str.rest();
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

    pub fn NamesEnum(comptime names: []const []const u8) type {
        const Field = std.builtin.TypeInfo.EnumField;
        var enum_fields: [names.len]Field = undefined;
        for (names) |name, i| {
            enum_fields[i] = .{
                .name = name,
                .value = i,
            };
        }
        return @Type(.{
            .Enum = .{
                .layout = .Auto,
                .tag_type = usize,
                .fields = &enum_fields,
                .decls = &.{},
                .is_exhaustive = true,
            },
        });
    }

    pub fn matchNames(self: *Self, comptime names: []const []const u8) ?NamesEnum(names) {
        const map = comptime blk: {
            const Pair = std.meta.Tuple(&.{[]const u8, usize});
            var pairs: [names.len]Pair = undefined;
            for (names) |name, i| {
                pairs[i] = .{ comptimeEscapeString(name), i };
            }
            break :blk std.ComptimeStringMap(usize, pairs);
        };
        if (self.read([]const u8)) |str| {
            if (map.get(str)) |index| {
                return @intToEnum(NamesEnum(names), index);
            }
            else {
                return null;
            }
        }
        else {
            return null;
        }

    }

    pub fn findFieldName(self: *Self, comptime name: []const u8) ?Tag {
        while (self.next()) |t| {
            if (t == .string) {
                if (self.matchName(name)) {
                    return self.next();
                }
                else {
                    self.skipNext();
                }
            }
            else {
                self.skip();
            }
        }
        return false;
    }

    pub fn ObjectField(comptime names: []const []const u8) type {
        return struct {
            name: NamesEnum(names),
            tag: ?Tag,
        };
    }

    pub fn findFieldNames(self: *Self, comptime names: []const []const u8) ?ObjectField(names) {
        while (self.next()) |t| {
            if (t == .string) {
                if (self.matchNames(names)) |name| {
                    return ObjectField(names) {
                        .name = name,
                        .tag = self.next(),
                    };
                }
                else {
                    self.skipNext();
                }
            }
            else {
                self.skip();
            }
        }
        return null;
    }



    pub const StringEscapeMode = enum {
        raw, unescaped,
    };

    // if the next value is a string, return an iterator over it.
    // `self`'s state remains unchanged
    // returns null if the next value is not a string
    pub fn stringMode(self: *Self, comptime mode: StringEscapeMode) StringIterator(mode) {
        self.i += 1;
        return StringIterator(mode) {
            .start = self.i,
            .walker = self,
        };
    }

    pub fn stringRaw(self: *Self) RawStringIterator
        { return self.stringMode(.raw); }
    pub fn string(self: *Self) UnescapedStringIterator
        { return self.stringMode(.unescaped); }

    pub const RawStringIterator = StringIterator(.raw);
    pub const UnescapedStringIterator = StringIterator(.unescaped);

    pub fn StringIterator(comptime mode: StringEscapeMode) type {
        return struct {
            start: usize,
            walker: *Walker,

            const StrIter = @This();

            pub fn reset(iter: *StrIter) void {
                iter.walker.i = iter.start;
            }

            pub fn next(iter: *StrIter) ?u8 {
                const c = iter.walker.stringNextMode(mode);
                if (c == null) {
                    iter.walker.i += 1;
                }
                return c;
            }

            pub fn rest(iter: *StrIter) void {
                iter.walker.stringRest();
            }
        };
    }


    fn stringNextMode(self: *Self, comptime mode: StringEscapeMode) ?u8 {
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

    fn stringNextRaw(self: *Self) ?u8
        { return self.stringNextMode(.raw); }

    fn stringNext(self: *Self) ?u8
        { return self.stringNextMode(.unescaped); }

    fn stringRest(self: *Self) void {
        while (self.stringNextMode(.raw) != null) {}
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
        if (self.next()) |t| {
            switch (t) {
                .object => {
                    self.fields();
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
                    self.fields();
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
                    var str = self.stringRaw();
                    try writer.writeByte('\"');
                    while (str.next()) |c| {
                        try writer.writeByte(c);
                    }
                    try writer.writeByte('\"');
                },
                .number => {
                    if (self.read(f64)) |n| {
                        try writer.print("{d}\n", .{n});
                    }
                },
                .boolean => {
                    if (self.read(bool)) |b| {
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