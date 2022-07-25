const std = @import("std");
const json = @import("server/json.zig");
// const rpc = @import("server/rpc.zig");

const File = std.fs.File;

pub fn main() !void {
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