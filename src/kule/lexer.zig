const std = @import("std");
const source = @import("source.zig");
const log = @import("log.zig");

const Source = source.Source;

pub const TokenTag = enum(u32) {
    invalid,

    identifier,
    number,

    import_path,

    kw_let,
    kw_where,
    kw_module,
    kw_pub,
    kw_import,

    dot,
    dotdot,


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


    const keyword_map: type = blk: {
        const Pair = std.meta.Tuple(&.{[]const u8, TokenTag});
        var pairs: []const Pair = &.{};
        for (std.enums.values(TokenTag)) |tag| {
            const tag_name = @tagName(tag);
            if (std.mem.startsWith(u8, tag_name, "kw_")) {
                pairs = pairs ++ &[_]Pair{.{ tag_name[3..], tag }};
            }
        }
        break :blk std.ComptimeStringMap(TokenTag, pairs);
    };

    const single_char_map: [256]?TokenTag = blk: {
        var map = std.mem.zeroes([256]?TokenTag);
        for (std.enums.values(TokenTag)) |tag| {
            if (tag.isSingleChar()) {
                map[@enumToInt(tag)] = tag;
            }
        }
        break :blk map;
    };

    fn isSingleChar(tag: TokenTag) bool {
        const i = @enumToInt(tag);
        if (i < 256) {
            const c = @intCast(u8, i);
            return std.ascii.isPrint(c);
        }
        else {
            return false;
        }
    }

    fn isKeyword(tag: TokenTag) bool {
        return std.mem.startsWith(u8, @tagName(tag), "kw_");
    }

    pub fn name(comptime tag: TokenTag) []const u8 {
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
                .import_path => "import path",
                .number => "number literal",
                else => @tagName(tag),
            };
        }
    }

};

pub const Token = struct {
    tag: TokenTag,
    text: []const u8,

    pub fn init(tag: TokenTag, text: []const u8) Token {
        return .{
            .tag = tag,
            .text = text,
        };
    }

};



pub const TokenStream = struct {

    source: Source,
    rest: []const u8,
    lookahead: ?Token,
    error_count: usize = 0,

    pub const Error = error {
        InvalidToken,
    };

    pub fn init(src: Source) Error!TokenStream {
        var self: TokenStream = .{
            .source = src,
            .rest = src.text,
            .lookahead = null,
        };
        _ = try self.tryNext();
        return self;
    }

    pub fn next(self: *TokenStream) ?Token {
        const result = self.lookahead;
        self.advance();
        return result;
    }

    pub fn dump(self: *TokenStream) !void {
        const w = std.io.getStdErr().writer();
        while (self.next()) |token| {
            try w.print("{s: <12}  {s}\n", .{@tagName(token.tag), token.text});
        }
    }

    pub fn tryNext(self: *TokenStream) !?Token {
        const token_opt = self.next();
        if (token_opt) |token| {
            if (token.tag == .invalid) {
                while (self.next() != null) {
                    // lex the rest of the source, logging errors
                    // there will be no way to recover this stream
                    self.advance();
                }
                return Error.InvalidToken;
            }
        }
        return token_opt;
    }

    fn advance(self: *TokenStream) void {
        self.rest = skipWhitespaceAndComments(self.rest);
        self.lookahead = self.parseToken();
        if (self.lookahead) |token| {
            self.rest = self.rest[token.text.len..];
        }
    }

    fn parseToken(self: *TokenStream) ?Token {
        const text = self.rest;
        if (text.len == 0) {
            return null;
        }
        if (self.lookahead) |last| {
            if (last.tag == .kw_import) {
                if (self.isOnNewLine()) {
                    self.sourceError(last.text, "import path missing. path must be on the same line", .{});
                    return Token.init(.import_path, "");
                }
                else {
                    if (self.parseImportPath()) |path| {
                        return Token.init(.import_path, path);
                    }
                }
            }
        }
        switch (text[0]) {
            'a'...'z', 'A'...'Z'
                => return parseIdentOrKeyword(text),
            '0'...'9'
                => return parseNumber(text),

            '.' => {
                if (std.mem.startsWith(u8, text, "..")) {
                    return Token.init(.dotdot, text[0..2]);
                }
                else {
                    return Token.init(.dot, text[0..1]);
                }
            },
            
            else => |c| {
                if (TokenTag.single_char_map[c]) |tag| {
                    return Token.init(tag, text[0..1]);
                }
                else {
                    const len: ?u3 = std.unicode.utf8ByteSequenceLength(c) catch null;    // just in case we support unicode later
                    const token_text = text[0..std.math.min(text.len, len orelse 1)];
                    const token = Token.init(.invalid, token_text);
                    if (len == null or !std.ascii.isPrint(c)) {
                        self.sourceError(token_text, "unexpected byte: {x}", .{c});
                    }
                    else {
                        self.sourceError(token_text, "unexpected character: {s}", .{token_text});
                    }
                    return token;
                }
            },
        }
    }

    pub fn sourceError(self: *TokenStream, token: []const u8, comptime format: []const u8, args: anytype) void {
        self.error_count += 1;
        log.sourceError(self.source, token, format, args);
    }

    fn isOnNewLine(self: TokenStream) bool {
        if (self.lookahead) |token| {
            const prev = token.text;
            const rest_addr = @ptrToInt(self.rest.ptr);
            const prev_addr = @ptrToInt(prev.ptr);
            const len = rest_addr - prev_addr;
            const gap = prev.ptr[prev.len..len];
            for (gap) |c| {
                if (c == '\n' or c == '\r') {
                    return true;
                }
            }
        }
        return false;
    }

    fn skipWhitespaceAndComments(text: []const u8) []const u8 {
        var t = text;
        while (t.len > 0) {
            const whitespace_len = whitespaceLen(t);
            t = t[whitespace_len..];
            const comment_len = commentLen(t);
            t = t[comment_len..];
            if (whitespace_len + comment_len == 0) {
                return t;
            }
        }
        return t;
    }

    fn whitespaceLen(text: []const u8) usize {
        return many(text, std.ascii.isSpace).len;
    }

    fn commentLen(text: []const u8) usize {
        if (text.len >= 2 and std.mem.startsWith(u8, text, "//")) {
            const len = many(text, struct{
                fn f(c: u8) bool {
                    return c != '\r' and c != '\n';
                }
            }.f).len;
            return len;
        }
        else {
            return 0;
        }
    }

    fn parseIdentOrKeyword(text: []const u8) Token {
        const token_text = many(text, struct {
            fn f(c: u8) bool {
                return std.ascii.isAlNum(c) or c == '_';
            }
        }.f);
        return .{
            .tag = TokenTag.keyword_map.get(token_text) orelse .identifier,
            .text = token_text,
        };
    }

    fn parseNumber(text: []const u8) Token {
        const token_text = many(text, std.ascii.isDigit);
        return .{
            .tag = .number,
            .text = token_text,
        };
    }

    fn parseImportPath(self: *TokenStream) ?[]const u8 {
        const text = self.rest;
        const len = for (text) |c, i| {
            if (!isImportPathChar(c)) {
                break i;
            }
            else {
                if (c == '/' and i + 1 < text.len and text[i + 1] == '/') {
                    break i;
                }
            }
        }
        else text.len;
        if (len == 0) {
            return null;
        }
        const path = text[0..len];
        if (len == 1) {
            switch (path[0]) {
                '.', '/' => self.sourceError(path, "import path must be a relative file", .{}),
                else => {},
            }
        }
        else if (path[0] == '/') {
            self.sourceError(path[0..1], "import path cannot be absolute", .{});
        }
        else if (path[len - 1] == '/') {
            self.sourceError(path[len-1..], "import path cannot be a directory", .{});
        }
        return path;
    }
    
    fn isImportPathChar(c: u8) bool {
        return switch (c) {
            '.', '_', '-', '/',  => true,
            else => std.ascii.isAlNum(c),
        };
    }
    

    fn many(text: []const u8, predicate: fn(u8) bool) []const u8 {
        const len = for (text) |c, i| {
            if (!predicate(c)) {
                break i;
            }
        }
        else text.len;
        return text[0..len];
    }

};

const testing = std.testing;

test "TokenStream.next()" {
    var src = try Source.init(testing.allocator, null, "let foo: u32 =\n\t413 + bar(612 * baz.zoo)");
    defer src.deinit(testing.allocator);
    const tokens = [_]Token {
        Token.init(.kw_let, "let"),
        Token.init(.identifier, "foo"),
        Token.init(.colon, ":"),
        Token.init(.identifier, "u32"),
        Token.init(.equal, "="),
        Token.init(.number, "413"),
        Token.init(.plus, "+"),
        Token.init(.identifier, "bar"),
        Token.init(.lparen, "("),
        Token.init(.number, "612"),
        Token.init(.aster, "*"),
        Token.init(.identifier, "baz"),
        Token.init(.dot, "."),
        Token.init(.identifier, "zoo"),
        Token.init(.rparen, ")"),
    };
    var stream = try TokenStream.init(src);
    for (tokens) |expected| {
        if (try stream.tryNext()) |actual| {
            try testing.expectEqual(@as(TokenTag, expected.tag), actual.tag);
            try testing.expectEqualStrings(expected.text, actual.text);
        }
        else {
            try testing.expect(false);
        }
    }
    try testing.expectEqual(@as(?Token, null), try stream.tryNext());
}