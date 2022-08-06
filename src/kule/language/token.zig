const std = @import("std");



pub const Token = struct {
    tag: Tag,
    text: []const u8,

    pub fn init(tag: Tag, text: []const u8) Token {
        return .{
            .tag = tag,
            .text = text,
        };
    }

    pub fn format(self: Token, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("({s} '{s}')", .{@tagName(self.tag), self.text});
    }

    pub const Tag = enum(u32) {
        invalid,
        start_of_file,
        end_of_file,

        name,
        number,

        import_path,

        kw_let,
        kw_where,
        kw_module,
        kw_pub,
        kw_import,


        dot = '.',
        plus = '+',
        minus = '-',
        aster = '*',
        fslash = '/',
        equal = '=',
        lparen = '(',
        rparen = ')',
        lcurly = '{',
        rcurly = '}',
        comma = ',',
        colon = ':',


        pub fn checkKeyword(text: []const u8) ?Tag {
            return keyword_map.get(text);
        }

        pub fn checkSingleChar(char: u8) ?Tag {
            return single_char_map[char];
        }

        const keyword_map: type = blk: {
            const Pair = std.meta.Tuple(&.{[]const u8, Tag});
            var pairs: []const Pair = &.{};
            for (std.enums.values(Tag)) |tag| {
                const tag_name = @tagName(tag);
                if (std.mem.startsWith(u8, tag_name, "kw_")) {
                    pairs = pairs ++ &[_]Pair{.{ tag_name[3..], tag }};
                }
            }
            break :blk std.ComptimeStringMap(Tag, pairs);
        };

        const single_char_map: [256]?Tag = blk: {
            var map = std.mem.zeroes([256]?Tag);
            for (std.enums.values(Tag)) |tag| {
                if (tag.isSingleChar()) {
                    map[@enumToInt(tag)] = tag;
                }
            }
            break :blk map;
        };

        fn isSingleChar(tag: Tag) bool {
            const i = @enumToInt(tag);
            if (i < 256) {
                const c = @intCast(u8, i);
                return std.ascii.isPrint(c);
            }
            else {
                return false;
            }
        }

        fn isKeyword(tag: Tag) bool {
            return std.mem.startsWith(u8, @tagName(tag), "kw_");
        }

        pub fn format(tag: Tag, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
            const no_quotes = fmt.len == 1 and fmt[0] == 's';
            if (tag.isKeyword()) {
                if (no_quotes) try writer.print("{s}", .{@tagName(tag)[3..]})
                else try writer.print("\"{s}\"", .{@tagName(tag)[3..]});
            }
            else if (tag.isSingleChar()) {
                if (no_quotes) try writer.print("{c}", .{@intCast(u8, @enumToInt(tag))})
                else try writer.print("'{c}'", .{@intCast(u8, @enumToInt(tag))});
            }
            else if (tag == .invalid) {
                try writer.writeAll("invalid token");
            }
            else {
                for (@tagName(tag)) |c| {
                    if (c == '_') {
                        try writer.writeByte(' ');
                    }
                    else {
                        try writer.writeByte(c);   
                    }
                }
            }
        }

        pub fn name(comptime tag: Tag) []const u8 {
            if (tag.isKeyword()) {
                return std.fmt.comptimePrint("\"{s}\"", .{@tagName(tag)[3..]});
            }
            else if (tag.isSingleChar()) {
                return std.fmt.comptimePrint("'{c}'", .{@intCast(u8, @enumToInt(tag))});
            }
            else {
                return switch (tag) {
                    .dot => "'.'",
                    .dotdot => "\"..\"",
                    else => blk: {
                        const tag_name = @tagName(tag);
                        var final_name: [tag_name.len]u8 = undefined;
                        for (tag_name) |c, i| {
                            if (c == '_') {
                                final_name[i] = ' ';
                            }
                            else {
                                final_name[i] = c;
                            }
                        }
                        break :blk &final_name;
                    },
                };
            }
        }

    };

};