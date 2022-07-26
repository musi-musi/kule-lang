const std = @import("std");
const json = @import("json.zig");

const ascii = std.ascii;

const Jw = json.Walker;

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

pub const Id = union(enum) {
    integer: int,
    string: []const u8,
};

pub const Message = struct {
    id: ?Id = null,
    method: []const u8 = undefined,
    params: ?json.Walker = null,
    full_text: []const u8,

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
        var walker = json.walk(text);
        if (walker.nextFields()) {
            var message = Message {
                .full_text = text,
            };
            const names = &[_][] const u8{
                "id", "method", "params",
            };
            while (walker.findFieldNames(names)) |field| {
                switch (field.name) {
                    .id => {
                        message.id = switch (field.tag orelse .null_value) {
                            .number => (
                                if (walker.read(int)) |i| .{
                                    .integer = i,
                                } else null
                            ),
                            .string => (
                                if (walker.read([]const u8)) |s| .{
                                    .string = s,
                                } else null
                            ),
                            else => null,
                        };
                    },
                    .method => {
                        message.method = walker.read([]const u8) orelse {
                            return Error.MissingMethodName;
                        };
                    },
                    .params => {
                        switch (field.tag orelse .null_value) {
                            .object, .array => {
                                message.params = walker.forkFields();
                            },
                            else => {},
                        }
                        return message;
                    },
                }
            }
            return message;
        }
        else {
            return Error.MissingJsonData;
        }
    }

    pub fn deinit(self: Message, allocator: Allocator) void {
        allocator.free(self.full_text);
    }


};
