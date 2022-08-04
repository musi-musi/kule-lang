const std = @import("std");
const language = @import("../language.zig");
const compiler = @import("../compiler.zig");
const diagnostics = @import("../diagnostics.zig");

const Source = compiler.Source;
const Diagnostics = diagnostics.Diagnostics;


const Token = language.Token;
const Tag = Token.Tag;


pub const TokenStream = struct {

    source: *const Source,
    rest: []const u8,
    lookahead: Token,
    error_count: usize = 0,
    diagnostics: *Diagnostics,

    pub const Error = error {
        InvalidToken,
    };

    pub fn init(source: *const Source, diags: *Diagnostics) TokenStream {
        var self: TokenStream = .{
            .source = source,
            .rest = source.text,
            .lookahead = undefined,
            .diagnostics = diags,
        };
        if (source.text.len == 0) {
            self.lookahead = Token.init(.end_of_file, source.text);
        }
        else {
            self.lookahead = Token.init(.start_of_file, source.text[0..0]);
            _ = self.next();
        }
        return self;
    }

    pub fn next(self: *TokenStream) Token {
        const result = self.lookahead;
        if (result.tag != .end_of_file) {
            self.advance();
        }
        return result;
    }

    pub fn dump(self: *TokenStream) !void {
        const w = std.io.getStdErr().writer();
        while (self.next()) |token| {
            try w.print("{s: <12}  {s}\n", .{@tagName(token.tag), token.text});
        }
    }

    pub fn tryNext(self: *TokenStream) !Token {
        const token = self.next();
        if (token.tag == .invalid) {
            self.skipToEndOfFile();
            return Error.InvalidToken;
        }
        return token;
    }

    pub fn skipToEndOfFile(self: *TokenStream) void {
        while (self.next().tag != .end_of_file) {
            // lex the rest of the source, logging errors
            // there will be no way to recover this stream
            self.advance();
        }
    }

    fn advance(self: *TokenStream) void {
        self.rest = skipWhitespaceAndComments(self.rest);
        const token = self.parseToken();
        self.lookahead = token;
        if (token.tag == .end_of_file) {
            self.rest.len = 0;
        }
        else {
            self.rest = self.rest[token.text.len..];
        }
    }

    fn parseToken(self: *TokenStream) Token {
        const text = self.rest;
        const last = self.lookahead;
        if (text.len == 0) {
            const marker_index = blk: {
                const text_addr = @ptrToInt(self.source.text.ptr);
                const last_addr = @ptrToInt(last.text.ptr) + last.text.len;
                const index = last_addr - text_addr;
                if (index == self.source.text.len) {
                    break :blk index - 1;
                }
                else {
                    break :blk index;
                }
            };
            return Token.init(.end_of_file, self.source.text[marker_index..][0..1]);
        }
        if (last.tag == .kw_import) {
            if (self.isOnNewLine()) {
                self.logError(last.text, "import path missing. path must be on the same line", .{});
                return Token.init(.import_path, "");
            }
            else {
                if (self.parseImportPath()) |path| {
                    return Token.init(.import_path, path);
                }
            }
        }
        switch (text[0]) {
            'a'...'z', 'A'...'Z'
                => return parseNameOrKeyword(text),
            '0'...'9'
                => return self.parseNumber(),

            '.' => return self.parseDot(),
            
            else => |c| {
                if (Tag.checkSingleChar(c)) |tag| {
                    return Token.init(tag, text[0..1]);
                }
                else {
                    const len: ?u3 = std.unicode.utf8ByteSequenceLength(c) catch null;    // just in case we support unicode later
                    const token_text = text[0..std.math.min(text.len, len orelse 1)];
                    const token = Token.init(.invalid, token_text);
                    if (len == null or !std.ascii.isPrint(c)) {
                        self.logError(token_text, "unexpected byte: {x}", .{c});
                    }
                    else {
                        self.logError(token_text, "unexpected character: {s}", .{token_text});
                    }
                    return token;
                }
            },
        }
    }

    pub fn logError(self: *TokenStream, token: []const u8, comptime format: []const u8, args: anytype) void {
        self.diagnostics.logError(token, format, args);
    }

    fn isOnNewLine(self: TokenStream) bool {
        const token = self.lookahead;
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

    fn parseNameOrKeyword(text: []const u8) Token {
        const token_text = many(text, struct {
            fn f(c: u8) bool {
                return std.ascii.isAlNum(c) or c == '_';
            }
        }.f);
        return .{
            .tag = Tag.checkKeyword(token_text) orelse .name,
            .text = token_text,
        };
    }


    fn parseDot(self: *TokenStream) Token {
        const rest = self.rest;
        if (rest.len > 1){        
            const digits_len = many(rest[1..], std.ascii.isDigit).len;
            if (digits_len > 0) {
                const number = rest[0..digits_len + 1];
                self.logError(number, "decimal number literal starts with decimal point. add a single leading 0", .{});
                return Token.init(.number, number);
            }
        }
        return Token.init(.dot, rest[0..1]);
    }

    fn parseNumber(self: *TokenStream) Token {
        const rest = self.rest;
        const whole_len = many(rest, std.ascii.isDigit).len;
        var len = whole_len;
        if (len < rest.len and rest[len] == '.') {
            len += 1;
            if (len < rest.len and rest[len] != '.') {
                len += many(rest[len..], std.ascii.isDigit).len;
            }
        }
        const number = rest[0..len];
        if (whole_len > 1 and number[0] == '0') {
            self.logError(number, "decimal number literal has leading zeroes", .{});
        }
        if (len == whole_len + 1) {
            self.logError(number, "decimal number literal has no digits after decimal point", .{});
        }
        return Token.init(.number, number);
    }
    
    fn isNumberChar(c: u8) bool {
        return std.ascii.isDigit(c) or c == '.';
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
                '.', '/' => self.logError(path, "import path must be a relative file", .{}),
                else => {},
            }
        }
        else if (path[0] == '/') {
            self.logError(path[0..1], "import path cannot be absolute", .{});
        }
        else if (path[len - 1] == '/') {
            self.logError(path[len-1..], "import path cannot be a directory", .{});
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
    var source = try Source.init(testing.allocator, null, "let foo: u32 =\n\t413 + bar(612 * baz.zoo)");
    defer source.deinit(testing.allocator);
    const tokens = [_]Token {
        Token.init(.kw_let, "let"),
        Token.init(.name, "foo"),
        Token.init(.colon, ":"),
        Token.init(.name, "u32"),
        Token.init(.equal, "="),
        Token.init(.number, "413"),
        Token.init(.plus, "+"),
        Token.init(.name, "bar"),
        Token.init(.lparen, "("),
        Token.init(.number, "612"),
        Token.init(.aster, "*"),
        Token.init(.name, "baz"),
        Token.init(.dot, "."),
        Token.init(.name, "zoo"),
        Token.init(.rparen, ")"),
    };
    var stream = try TokenStream.init(source);
    for (tokens) |expected| {
        if (try stream.tryNext()) |actual| {
            try testing.expectEqual(@as(Tag, expected.tag), actual.tag);
            try testing.expectEqualStrings(expected.text, actual.text);
        }
        else {
            try testing.expect(false);
        }
    }
    try testing.expectEqual(@as(?Token, null), try stream.tryNext());
}