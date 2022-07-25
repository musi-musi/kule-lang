const std = @import("std");

const assert = std.debug.assert;
// so i set out to make language server and i accidentally a whole json parser
// none of this has been tested yet lmao

pub fn comptimeEscapeString(comptime string: []const u8) []const u8 {
    var escaped: []const u8 = "";
    for (string) |c| {
        const append = switch (c) {
            '"', '\\', '/'
                => &[_]u8{ '\\', c},
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

pub const Tag = enum {
    object,
    array,
    end,
    string,
    number,
    boolean,
    null_value,

    const char_map: [256]?Tag = blk: {
        var map = std.mem.zeroes([256]?Tag);
        map['{'] = .object;
        map['['] = .array;
        map['}'] = .end;
        map[']'] = .end;
        map['"'] = .string;
        map['t'] = .boolean;
        map['f'] = .boolean;
        map['n'] = .null_value;
        for ("-0123456789") |c| {
            map[c] = .number;
        }
        break :blk map;
    };


    fn canParseToType(tag: Tag, comptime T: type) bool {
        const trait = std.meta.trait;
        if (tag == .end) {
            return false;
        }
        switch (T) {
            Walker => return true,
        }
        switch (tag) {
            .object => {
                return (
                    trait.is(.Struct)(T)
                    and !trait.isTuple(T)
                );
            },
            .array => {
                return (
                    trait.isTuple(T)
                    or trait.isSlice(T)
                );
            },
            .end => return false,
            .string => {
                return (
                    trait.isZigString(T)
                    or T == Walker.StringIterator(.raw)
                    or T == Walker.StringIterator(.unescaped)
                );
            },
            .number => return trait.isNumber(T),
            .boolean => return T == bool,
            .null_value => return T == void,
        }
        
    }


};

const CharTable = [256]bool;

fn charTable(comptime class: []const u8) CharTable {
    var table = std.mem.zeroes(CharTable);
    for (class) |c| {
        table[c] = true;
    }
    return table;
}

const char = struct {
    const field_term = charTable(",]}");
    const numerical = charTable("-0123456789eE.");
};


pub fn walk(json: []const u8) Walker {
    return Walker.init(json);
}

/// walks a json source text in a static buffer, allowing for fast data extraction without allocations
pub const Walker = struct {
    
    text: []const u8 = "",
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

    pub fn forkOpen(self: Self, tag: Tag) ?Self {
        var child = self.fork();
        if (child.open(tag)) {
            return child.fork();
        }
        else {
            return null;
        }
    }

    pub fn join(self: *Self, child: Self) void {
        const self_addr = @ptrToInt(&self.text[self.i]);
        const child_addr = @ptrToInt(&child.text[child.i]);
        assert(child_addr > self_addr);
        self.i += (child_addr - self_addr);
    }


    pub fn to(self: *Self, tag: Tag) bool {
        return self.toNext() == tag;
    }

    pub fn toNext(self: *Self) ?Tag {
        for (self.text[self.i..]) |c, len| {
            if (Tag.char_map[c]) |tag| {
                self.i += len;
                return tag;
            }
        }
        self.i = self.text.len;
        return null;
    }

    pub fn open(self: *Self, tag: Tag) bool {
        if (self.to(tag)) {
            self.enter();
            return true;
        }
        else {
            return false;
        }
    }

    pub fn openNext(self: *Self) ?Tag {
        if (self.toNext()) |tag| {
            self.enter();
            return tag;
        }
        else {
            return null;
        }
    }

    pub fn enter(self: *Self) void {
        switch (self.text[self.i]) {
            '{', '[', '"' => self.i += 1,
            else => {},
        }
    }

    pub fn close(self: *Self, tag: Tag) void {
        switch (tag) {
            .object => self.closeBalanced('}'),
            .array => self.closeBalanced(']'),
            .string => {
                while (self.nextStringCharRaw()) |_| {} 
                if (self.i < self.text.len)     {
                    self.i += 1;
                }
            },
            .number => self.skipManyTable(char.numerical),
            .boolean, .null_value => self.skipManyClass("truefalsenull"),
            .end => self.i += 1,
        }
    }

    fn closeBalanced(self: *Self, comptime rbracket: u8) void {
        const text = self.text;
        const len = text.len;
        while (self.i < len) {
            const c = text[self.i];
            self.i += 1;
            switch (c) {
                rbracket => return,
                '{' => return self.close(.object),
                '[' => return self.close(.array),
                '"' => return self.close(.string),
                else => self.i += 1,
            }
        }
    }

    pub fn skip(self: *Self, tag: Tag) bool {
        if (self.open(tag)) {
            self.close(tag);
            return true;
        }
        else {
            return false;
        }
    }

    pub fn skipNext(self: *Self) bool {
        if (self.openNext()) |tag| {
            self.close(tag);
            return true;
        }
        else {
            return false;
        }
    }


    fn Read(comptime T: type) type {
        if (std.meta.trait.is(.Optional)(T)) {
            return std.meta.Child(T);
        }
        else {
            return T;
        }
    }

    pub fn read(self: *Self, comptime T: type) ?Read(T) {
        if (self.toNext()) |tag| {
            if (tag == .end) {
                return null;
            }
        }
        else {
            return null;
        }
        switch (T) {
            Walker => {
                return self.fork();
            },
            StringIterator(.raw) => return self.readStringIterator(.raw),
            StringIterator(.unescaped) => return self.readStringIterator(.unescaped),
            bool => return self.readBoolean(),
            void => return self.readNull(),
            else => {},
        }
        switch (@typeInfo(T)) {
            .Int, .Float => return self.readNumber(T),
            else => {},           
        }
        if (std.meta.trait.isZigString(T)) {
            return self.readStringRaw();
        }
        @compileError("cannot parse " ++ @typeName(T));
    }


    fn readObject(self: *Self, comptime T: type) ?T {
        const map = fieldIndexMap(T);

        const fields = std.meta.fields(T);
        var walkers = std.meta.zeroes([map.kvs.len]?Self);

        if (self.forkOpen(.object)) |obj| {
            while (obj.readStringRaw()) |field_name| {
                if (map.get(field_name)) |i| {
                    if (obj.toNext()) |_| {
                        walkers[i] = obj.fork();
                        obj.skipNext();
                    }
                }
                else {
                    obj.skipNext();
                }
            }
            obj.close(.object);
            self.join(obj);
        }
        else {
            return null;
        }

        var result: T = undefined;

        inline for (fields) |field, i| {
            if (walkers[i]) |walker| {
                if (walker.toNext()) |tag| {
                    if (tag.canParseToType(field.field_type)) {
                        if (walker.read(field.field_type)) |value| {
                            @field(result, field.name) = value;
                        }
                    }
                }
            }
        }
        return result;

    }

    fn fieldIndexMap(comptime T: type) type {
        const fields = std.meta.fields(T);
        const Pair = std.meta.Tuple(&.{[]const u8, usize});
        var pairs: [fields.len]Pair = undefined;
        for (fields) |field, i| {
            pairs[i] = .{ comptimeEscapeString(field.name), i};
        }
        return std.ComptimeStringMap(usize, pairs);
    }

    fn readStringIterator(self: *Self, comptime mode: StringEscapeMode) ?StringIterator(mode) {
        if (self.iterateString(mode)) |iter| {
            _ = self.skip(.string);
            return iter;
        }
        else {
            return null;
        }
    }


    pub fn nextField(self: *Self) ?Tag {
        if (self.toNext()) |tag| {
            if (tag != .end) {
                return tag;
            }
        }
        return null;
    }

    // pub fn findFieldName(self: *Self, comptime field_name: []const u8) bool {
    //     while (self.nextField()) |tag| {
    //         if (self.matchName(field_name)) {
    //             return true;
    //         }
    //         else {
    //             _ = self.skipNext();
    //         }
    //     }
    //     return false;
    // }

    

    pub fn matchName(self: *Self, comptime name: []const u8) bool {
        const escaped = comptime comptimeEscapeString(name);
        if (self.open(.string)) {
            defer self.close(.string);
            var i: usize = 0;
            while (self.nextStringCharRaw()) |c| : (i += 1) {
                if (i >= escaped.len or c != escaped[i]) {
                    self.close(.string);
                    return false;
                }
            }
            if (i == escaped.len) {
                self.close(.string);
                return true;
            }
            else {
                self.close(.string);
                return false;
            }

        }
    }

    pub const StringEscapeMode = enum {
        raw, unescaped,
    };


        
    // if the next value is a string, return an iterator over it.
    // `self`'s state remains unchanged
    // returns null if the next value is not a string
    pub fn iterateString(self: Self, comptime mode: StringEscapeMode) ?StringIterator(mode) {
        var child = self.fork();
        if (child.open(.string)) {
            return StringIterator(mode) {
                .walker = child.fork(),
            };
        }
        else {
            return null;
        }
    }

    pub fn iterateStringRaw(self: Self) ?RawStringIterator
        { return self.iterateString(.raw); }
    pub fn iterateStringUnescaped(self: Self) ?UnescapedStringIterator
        { return self.iterateString(.unescaped); }

    pub const RawStringIterator = StringIterator(.raw);
    pub const UnescapedStringIterator = StringIterator(.unescaped);

    pub fn StringIterator(comptime mode: StringEscapeMode) type {
        return struct {
            walker: Walker,

            const StrIter = @This();

            pub fn reset(iter: *StrIter) void {
                iter.walker.i = 0;
            }

            pub fn next(iter: *StrIter) ?u8 {
                return iter.walker.nextStringChar(mode);
            }
        };
    }


    pub fn nextStringChar(self: *Self, comptime mode: StringEscapeMode) ?u8 {
        switch (mode) {
            .raw => return self.nextStringCharRaw(),
            .unescaped => return self.nextStringCharUnescaped(),
        }
    }
    pub fn nextStringCharRaw(self: *Self) ?u8 {
        const text = self.text;
        const len = text.len;
        if (self.i < len) {
            const c = text[self.i];
            self.i += 1;
            if (c == '"' and text[self.i - 2] != '\\') {
                return null;
            }
            else {
                return c;
            }
        }
        else {
            return null;
        }
    }

    pub fn nextStringCharUnescaped(self: *Self) ?u8 {
        const text = self.text;
        const len = text.len;
        if (self.i < len) {
            const c = text[self.i];
            self.i += 1;
            switch (c) {
                '"' => {
                    return null;
                },
                '\\' => if (self.i < len) {
                    const code = text[self.i];
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
                },
                else => return c,
            }
        }
        return null;
    }

    pub fn readStringRaw(self: *Self) ?[]const u8 {
        if (self.open(.string)) {
            const text = self.text;
            const start = self.i;
            self.close(.string);
            if (text[self.i - 1] == '"') {
                return text[start..self.i-1];
            }
            else {
                // reached end of file
                return null;
            }
        }
        else {
            // not a string
            return null;
        }
    }

    pub fn readNumber(self: *Self, comptime T: type) ?T {
        if (self.open(.number)) {
            const text = self.text;
            const start = self.i;
            const len = lenManyTable(text[start..], char.numerical);
            self.i += len;
            const token = text[start..self.i];
            switch (@typeInfo(T)) {
                .Int => return std.fmt.parseInt(T, token) catch null,
                .Float => return std.fmt.parseFloat(T, token) catch null,
                else => @compileError(@typeName(T) ++ "is not a numerical type"),
            }
        }
        else {
            return null;
        }
    }

    pub fn readBoolean(self: *Self) ?bool {
        if (self.open(.boolean)) {
            const c = self.text[self.i];
            _ = self.close(.boolean);
            return c == 't';
        }
        else {
            return null;
        }
    }

    pub fn readNull(self: *Self) ?void {
        if (self.open(.null_value)) {
            self.skipManyClass("truefalsenull");
            return {};
        }
        else {
            return null;
        }
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
    

    fn dumpString(self: *Self, writer: anytype) @TypeOf(writer).Error!void {
        if (self.readStringIterator(.unescaped)) |iter| {
            var text = iter;
            while (text.next()) |c| {
                try writer.writeByte(c);
            }
        }
    }

    
    pub fn dump(self: *Self, writer: anytype, depth: usize) @TypeOf(writer).Error!void {
        if (self.toNext()) |tag| {
            switch (tag) {
                .object => {
                    try writer.writeAll("@\n");
                    self.enter();
                    defer self.close(.object);
                    var i: usize = 0;
                    while (self.nextField()) |_| {
                        if (i > 0) {
                            try writer.writeAll("\n");
                        }
                        i += 1;
                        try writer.writeByteNTimes(' ', (depth + 1) * 3);
                        try self.dumpString(writer);
                        try writer.writeAll(" -> ");
                        try self.dump(writer, depth + 1);
                    }
                },
                .array => {
                    try writer.writeAll("#\n");
                    self.enter();
                    defer self.close(.array);
                    var i: usize = 0;
                    while (self.nextField()) |_| {
                        if (i > 0) {
                            try writer.writeAll("\n");
                        }
                        i += 1;
                        try writer.writeByteNTimes(' ', (depth + 1) * 3);
                        try self.dump(writer, depth + 1);
                    }
                },
                .string => {
                    try self.dumpString(writer);
                },
                .number => {
                    if (self.readNumber(f64)) |num| {
                        try writer.print("{d}\n", .{num});
                    }
                },
                .boolean => {
                    if (self.readBoolean()) |b| {
                        try writer.print("{}\n", .{b});
                    }
                },
                .null_value => {
                    if (self.readNull()) |_| {
                        try writer.writeAll("null\n");
                    }
                },
                .end => {},//try writer.writeAll("~\n"),
            }
        }
    }

};