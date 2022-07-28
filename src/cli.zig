const std = @import("std");
const kule = @import("kule.zig");

const compiler = kule.compiler;

const Allocator = std.mem.Allocator;


pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    var args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    if (args.len <= 1) {
        kule.log.err("must provide action", .{});
        // std.os.exit(1);
    }
    else {
        const action = args[1];
        if (std.mem.eql(u8, action, "parse")) {
            if (args.len <= 2) {
                kule.log.err("must provide at least one source file", .{});
                // std.os.exit(1);
            }
            for (args[2..]) |arg| {
                parseFile(allocator, arg) catch {};
            }
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
    if (compiler.parseSource(allocator, &src, &diagnostics)) |result| {
        var src_module = result;
        defer src_module.deinit();
        // const stdout = std.io.getStdOut().writer();
        kule.log.info("parsed file {s}", .{path});
        // try src_module.dump(stdout);
    }
    else |err| {
        kule.log.err("{s} failed with {d} errors", .{path, diagnostics.error_count});
        try diagnostics.logMessages();
        return err;
    }
}
