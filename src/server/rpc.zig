const std = @import("std");
const json = @import("json.zig");

const ascii = std.ascii;

const Jw = json.Value;

const Allocator = std.mem.Allocator;
const File = std.fs.File;

const Reader = File.Reader;

const Tuple = std.meta.Tuple;

pub const int = i32;
pub const uint = u32;

pub const In = struct {
    
    allocator: Allocator,
    reader: Reader,

    pub fn init(allocator: Allocator, reader: Reader) In {
        return .{
            .allocator = allocator,
            .reader = reader,
        };
    }


};

pub const Message = struct {
    id: ?json.Value = null,
    method: []const u8 = undefined,
    params: ?json.Value = null,
    full_text: []const u8 = undefined,

    pub const Error = error {
        EndOfStream,
        MissingJsonData,
        MissingMethodName,
    } || Allocator.Error || Reader.Error;

    pub fn init(allocator: Allocator, reader: Reader) Error!Message {
        const isDigit = ascii.isDigit;
        var c: u8 = 0;
        while (!isDigit(c)) : (c = try reader.readByte()) {}
        var len: usize = 0;
        while (isDigit(c)) : (c = try reader.readByte()) {
            len = len * 10 + c - '0';
        }
        while (c != '{') : (c = try reader.readByte()) {}
        const text = try allocator.alloc(u8, len);
        errdefer allocator.free(text);
        text[0] = c;
        _ = try reader.readAll(text[1..]);
        return json.parse(text, Message) orelse Error.MissingJsonData;
    }

    pub fn deinit(self: Message, allocator: Allocator) void {
        allocator.free(self.full_text);
    }


};
