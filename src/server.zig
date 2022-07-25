const std = @import("std");
const json = @import("server/json.zig");
// const rpc = @import("server/rpc.zig");

const File = std.fs.File;

const Allocator = std.mem.Allocator;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var list = std.ArrayListUnmanaged(u8){};
    const stdin = std.io.getStdIn().reader();
    const writer = list.writer(allocator);
    while (stdin.readByte()) |c| {
        try writer.writeByte(c);
    }
    else |_| {}
    const text = list.toOwnedSlice(allocator);
    defer allocator.free(text);
    var walker = json.walk(text);
    try walker.dump(std.io.getStdErr().writer(), 0);
    // var reader = rpc.fileReader(std.io.getStdIn());
    // var method_buf: [1024]u8 = undefined;
    // const header = try reader.readHeader(&method_buf);
    // std.log.info("parsed rpc header:{}", .{header});
    // try reader.skipObject();
    // const out = std.io.getStdOut().writer();
    // try out.writeAll("remaining input \n>>");
    // try reader.dumpRemaining(out);
    // try out.writeAll("<<\n");
}

// test "server.rpc" {
//     _ = rpc;
// }