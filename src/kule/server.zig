const std = @import("std");
const compiler = @import("compiler.zig");

const source = @import("source.zig");
const diagnostics = @import("diagnostics.zig");

const Source = source.Source;
const Diagnostics = diagnostics.Diagnostics;

pub fn run() !void {
    const stdin = std.io.getStdIn().reader();
    const file = try std.fs.cwd().createFile("server_out", .{});
    defer file.close();
    const stdout = file.writer();
    while (stdin.readByte()) |c| {
        // const c = try stdin.readByte();
        // if (c == 0) break :main_loop;
        // const c = stdin.readByte() catch |err| switch(err) {};
        try stdout.writeByte(c);
    }
    else |_| {}
}