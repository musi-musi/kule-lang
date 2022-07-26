const std = @import("std");
const json = @import("server/json.zig");
const rpc = @import("server/rpc.zig");

const File = std.fs.File;

const Allocator = std.mem.Allocator;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    const stdin = std.io.getStdIn().reader();
    const message = try rpc.Message.init(allocator, stdin);
    std.log.info("recieved:", .{});
    if (message.id) |id| {
        std.log.info("id: {s}", .{id.text});
    }
    std.log.info("method: {s}", .{message.method});
    if (message.params) |params| {
        std.log.info("params: {s}", .{params.text});
    }
}
