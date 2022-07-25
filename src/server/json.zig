const std = @import("std");

// so i set out to make language server and i accidentally a whole json parser
// none of this has been tested yet lmao

pub const ValueType = enum(u3) {
    object,
    array,
    string,
    number,
    boolean,
    null_value,

    fn initialTable(comptime t: ValueType) CharTable {
        return comptime switch (t) {
            .object => charTable("{"),
            .array => charTable("["),
            .string => charTable("\""),
            .number => charTable("-0123456789"),
            .boolean => charTable("tf"),
            .null_value => charTable("n"),
        };
    }
    

    const char_map: [256]?ValueType = blk: {
        var map = std.mem.zeroes([256]?ValueType);
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

fn charTable(comptime chars: []const u8) CharTable {
    var table = std.mem.zeroes(CharTable);
    for (chars) |c| {
        table[c] = true;
    }
    return table;
}

const char = struct {
    const field_term = charTable(",]}");
    const numerical = charTable("-0123456789eE.");
};


pub const JsonReader = struct {
    
    text: []u8 = "",
    pos: usize = 0,

    const Self = @This();

    pub fn reset(self: *Self, text: []u8) void {
        self.text = text;
        self.pos = 0;
    }

    fn manyLenChars(text: []const u8, comptime chars: []const u8) usize {
        return manyLenTable(text, comptime charTable(chars));
    }

    fn manyLenTable(text: []const u8, comptime table: CharTable) usize {
        for (text) |c, len| {
            if (!table[c]) {
                return len;
            }
        }
        return text.len;
    }

    fn untilLenChars(text: []const u8, comptime chars: []const u8) usize {
        return untilLenTable(text, comptime charTable(chars));
    }

    fn untilLenTable(text: []const u8, comptime table: CharTable) usize {
        for (text) |c, len| {
            if (table[c]) {
                return len;
            }
        }
        return text.len;
    }

    fn skipManyChars(self: *Self, comptime chars: []const u8) void {
        return self.skipManyTable(comptime charTable(chars));
    }
    fn skipManyTable(self: *Self, comptime table: CharTable) void {
        const len = manyLenTable(self.text[self.pos..], table);
        self.pos += len;
    }

    fn skipUntilChars(self: *Self, comptime chars: []const u8) void {
        return self.skipUntilTable(comptime charTable(chars));
    }
    fn skipUntilTable(self: *Self, comptime table: CharTable) void {
        const len = untilLenTable(self.text[self.pos..], table);
        self.pos += len;
    }

    pub fn nextValue(self: *Self) ?ValueType {
        const token = self.text[self.pos..];
        for (token) |c, len| {
            if (ValueType.char_map[c]) |value_type| {
                self.pos += len;
                return value_type;
            }
        }
        self.pos = self.text.len;
        return null;
    }


    pub fn skipString(self: *Self) void {
        _ = self.readString();
    }

    pub fn readString(self: *Self) ?[]const u8 {
        if (self.openString()) {
            return self.closeString();
        }
        else {
            return null;
        }
    }

    pub fn openString(self: *Self) bool {
        return self.open(.string);
    }

    pub fn closeString(self: *Self) ?[]const u8 {
        if (self.pos >= self.text.len) {
            return null;
        }
        const token = self.text[self.pos..];
        var i: usize = 0;
        while (i < token.len and token[i] != '"') : (i += 1) {
            const c = token[i];
            if (c == '\\') i += 1;
        }
        if (i < token.len or (token[i] == '"' and token [i-1] != '\\')) {
            const result = token[0..i];
            self.pos += i + 1;
            return result;
        }
        else {
            self.toTextEnd();
            return null;
        }
    }

    pub fn nextStringChar(self: *Self) ?u8 {
        const text = self.text;
        const len = text.len;
        if (self.pos < len) {
            const c = text[self.pos];
            self.pos += 1;
            switch (c) {
                '"' => return null,
                '\\' => if (self.pos < len) {
                    const code = text[self.pos];
                    self.pos += 1;
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


    pub fn skipValue(self: *Self) void {
        switch (self.nextValue()) {
            .object => self.skipObject(),
            .array => self.skipArray(),
            .string => self.skipString(),
            .number => self.skipNumber(),
            .boolean => self.skipManyChars("truefals"),
            .null_value => self.skipManyChars("nul"),
        }
    }

    pub fn skipObject(self: *Self) void {
        if (self.openObject()) {
            self.closeObject();
        }
    }

    pub fn openObject(self: *Self) bool {
        return self.open(.object);
    }


    pub fn hasFieldsRemaining(self: *Self) bool {
        switch (self.text[self.pos]) {
            '}', ']' => false,
            else => true,
        }
    }

    pub fn toNamedField(self: *Self, comptime field_name: []const u8) bool {
        while (self.hasFieldsRemaining()) : (self.nextField()) {
            if (self.matchName(field_name)) {
                return true;
            }
            else {
                self.skipValue();
            }
        }
        return false;
    }

    pub fn matchName(self: *Self, comptime name: []const u8) bool {
        if (self.openString()) {
            defer self.closeString();
            var i: usize = 0;
            while (self.nextStringChar()) |c| : (i += 1) {
                if (i >= name.len or c != name[i]) {
                    self.closeString();
                    return false;
                }
            }
            if (i == name.len) {
                self.closeString();
                return true;
            }
            else {
                self.closeString();
                return false;
            }

        }
    }


    pub fn closeObject(self: *Self) void {
        self.closeBalanced('}');
    }

    pub fn skipArray(self: *Self) void {
        if (self.openArray()) {
            self.closeArray();
        }
    }

    pub fn openArray(self: *Self) void {
        return self.open(.array);
    }

    pub fn closeArray(self: *Self) void {
        self.closeBalanced(']');
    }

    fn open(self: *Self, comptime value_type: ValueType) bool {
        if (self.nextValue() == value_type) {
            self.pos += 1;
            return true;
        }
        else {
            return false;
        }
    }

    fn closeBalanced(self: *Self, comptime rbracket: u8) bool {
        const text = self.text;
        const len = text.len;
        while (self.pos < len) {
            const c = text[self.pos];
            self.pos += 1;
            switch (c) {
                rbracket => {
                    return self.pos < len;
                },
                '{' => {
                    return self.closeObject();
                },
                '[' => {
                    return self.closeArray();
                },
                '"' => {
                    return self.closeString();
                },
                else => self.pos += 1,
            }
        }
        return false;
    }

    fn toTextEnd(self: *Self) void {
        self.pos = self.text.len;
    }

    fn nextField(self: *Self) void {
        self.pos += untilLenTable(self.text[self.pos..], char.field_term);
    }

    pub fn readNumber(self: *Self, comptime T: type) ?T {
        if (self.nextValue() == .number) {
            const text = self.text;
            const start = self.pos;
            const len = manyLenTable(text[start..], char.numerical);
            self.pos += len;
            const token = text[start..self.pos];
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

    pub fn skipNumber(self: *Self) void {
        if (self.nextValue() == .number) {
            const len = manyLenTable(self.text[self.pos..], char.numerical);
            self.pos += len;
        }
    }

    pub fn readBoolean(self: *Self) ?bool {
        if (self.nextValue() == .boolean) {
            const token =  self.text[self.pos..];
            switch (token[0]) {
                't' => {
                    if (std.mem.startsWith(u8, token, "true")) {
                        self.pos += "true".len;
                        return true;
                    }
                    else {
                        return null;
                    }
                },
                'f' => {
                    if (std.mem.startsWith(u8, token, "false")) {
                        self.pos += "false".len;
                        return false;
                    }
                    else {
                        return null;
                    }
                },
                else => unreachable,
            }
        }
        else {
            return null;
        }
    }

    pub fn readNull(self: *Self) ?void {
        if (self.nextValue() == .null_value) {
            if (std.mem.startsWith(u8, self.text[self.pos..], "null")) {
                self.pos += "null".len;
                return {};
            }
        }
        else {
            return null;
        }
    }

    
};