const std = @import("std");

const Server = @import("server/server.zig").Server;
const json = @import("server/json.zig");
const rpc = @import("server/rpc.zig");

const File = std.fs.File;

const Allocator = std.mem.Allocator;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var server = try Server.init(allocator);
    defer server.deinit();
    try server.run();
}
