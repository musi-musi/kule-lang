const std = @import("std");

const Allocator = std.mem.Allocator;

pub const SourceLocation = struct {
    line: usize,
    column: usize,
};

pub const Source = struct {

    name: ?[]const u8,
    text: []const u8,
    lines: []const []const u8,

    pub fn init(allocator: Allocator, name: ?[]const u8, text: []const u8) !Source {
        var lines = std.ArrayListUnmanaged([]const u8){};
        var line_start: usize = 0;
        var i: usize = 0;
        while (i < text.len) {
            const c0 = text[i];
            if (c0 == '\n' or c0 == '\r') {
                const line = text[line_start..i];
                i += 1;
                if (i < text.len) {
                    const c1 = text[i];
                    if ((c1 == '\n' or c1 == '\r') and c0 != c1) {
                        i += 1;
                    }
                }
                line_start = i;
                try lines.append(allocator, line);
            }
            else {
                i += 1;
            }
        }
        try lines.append(allocator, text[line_start..]);

        return Source {
            .name = name,
            .text = text,
            .lines = lines.toOwnedSlice(allocator),
        };      
    }

    pub fn deinitFile(self: Source, allocator: Allocator) void {
        allocator.free(self.text);
        self.deinit(allocator);
    }

    pub fn deinit(self: Source, allocator: Allocator) void {
        allocator.free(self.lines);
    }

    pub fn fromFile(allocator: Allocator, name: ?[]const u8, file: std.fs.File) !Source {
        const stat = try file.stat();
        const text = try allocator.alloc(u8, stat.size);
        _ = try file.readAll(text);
        return try init(allocator, name, text);
    }

    pub fn fromFileLocal(allocator: Allocator, path: []const u8) !Source {
        const file = try std.fs.cwd().openFile(path, .{});
        defer file.close();
        return fromFile(allocator, path, file);
    }

    pub fn fromFileAbsolute(allocator: Allocator, path: []const u8) !Source {
        const file = try std.fs.openFileAbsolute(path, .{});
        defer file.close();
        return fromFile(allocator, path, file);
    }

    pub fn tokenLocation(self: Source, token: []const u8) ?SourceLocation {
        const token_addr = @ptrToInt(token.ptr);
        const text_addr = @ptrToInt(self.text.ptr);
        if (token_addr < text_addr or token_addr >= (text_addr + self.text.len)) {
            return null;
        }
        else {
            var window = self.lines;
            while (window.len > 1) {
                const pivot = @divFloor(window.len, 2);
                const line = window[pivot];
                if (token_addr < @ptrToInt(line.ptr)) {
                    window = window[0..pivot];
                }
                else {
                    window = window[pivot..];
                }
            }
            return SourceLocation {
                .line = (@ptrToInt(window.ptr) - @ptrToInt(self.lines.ptr)) / @sizeOf([]const u8),
                .column = token_addr - @ptrToInt(window[0].ptr),
            };
        }
    }

};


const testing = std.testing;

const test_text = "toki\npona\r\nli\rmusi\n\rmute";
const test_lines = [_][]const u8{
    "toki", "pona", "li", "musi", "mute",
};

test "Source.init()" {
    const source = try Source.init(testing.allocator, null, test_text);
    defer source.deinit(testing.allocator);
    try testing.expectEqual(test_lines.len, source.lines.len);
    for (source.lines) |line, i| {
        try testing.expectEqualStrings(test_lines[i], line);
    }
}

test "Source.tokenLocation()" {
    const source = try Source.init(
        testing.allocator, null, 
        "toki pona\n" ++            // 0..10
        "li pona mute.\n" ++        // 10..24
        "mi soweli walo suwi\n"     // 24..44
    );
    defer source.deinit(testing.allocator);
    const table = [_]std.meta.Tuple(&.{usize, usize, ?SourceLocation}){
        .{ 0, 4, SourceLocation{ .line = 0, .column = 0 } },
        .{ 13, 17, SourceLocation{ .line = 1, .column = 3 } },
        .{ 27, 33, SourceLocation{ .line = 2, .column = 3 } },
    };
    try testing.expectEqual(@as(?SourceLocation, null), source.tokenLocation("whoops!"));
    for (table) |entry| {
        const token = source.text[entry.@"0"..entry.@"1"];
        try testing.expectEqual(entry.@"2", source.tokenLocation(token));
    }

}