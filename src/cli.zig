const std = @import("std");
const kule = @import("kule.zig");

const Allocator = std.mem.Allocator;

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len <= 1) {
        kule.log.err("must provide at least one source filename", .{});
        // std.os.exit(1);
    }
    else {
        for (args[1..]) |arg| {
            parseFile(allocator, arg) catch {};
        }
    }
}

fn parseFile(allocator: Allocator, path: []const u8) !void {
    var src = try kule.Source.fromFileLocal(allocator, path);
    defer src.deinitFile(allocator);
    // var tokens = try kule.TokenStream.init(src);
    // try tokens.dump();
    var diagnostics = kule.diagnostics.Diagnostics.init(allocator);
    defer diagnostics.deinit();
    var parser = try kule.Parser.init(allocator, src, &diagnostics);
    defer parser.deinit();
    const err: ?kule.Parser.Error = blk: {
        if (parser.parse()) |ast| {
            defer ast.deinit();
            const stdout = std.io.getStdOut().writer();
            kule.log.info("parsed file {s}", .{path});
            try ast.dump(stdout);
            break :blk null;
        }
        else |err| {
            kule.log.err("{s} failed with {d} errors", .{path, parser.error_count});
            break :blk err;
        }
    };
    try diagnostics.logMessages();
    return err orelse {};
}