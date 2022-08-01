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
        kule.logger.err("must provide action", .{});
        // std.os.exit(1);
    }
    else {
        const action = args[1];
        if (std.mem.eql(u8, action, "parse")) {
            if (args.len <= 2) {
                kule.logger.err("must provide at least one source file", .{});
                // std.os.exit(1);
            }
            for (args[2..]) |arg| {
                parseFile(allocator, arg) catch {};
            }
        }
        // if (std.mem.eql(u8, action, "list-constants")) {
        //     // const Type = kule.compiler.types.Type;
        //     for (kule.compiler.language_constants) |constant| {
        //         const value = constant.@"1";
        //         if (value.value_type == .type_value and value.data.type_value == .number) {
        //             const number = value.data.type_value.number;
        //             std.log.info("{s} {x:0>3} {x:0>3} {}", .{constant.@"0", @enumToInt(number.encoding), @enumToInt(number.dimensions), number.is_dynamic});
        //         }
        //         else {
        //             std.log.info("{s}", .{constant.@"0"});
        //         }
        //     }
        // }
    }
}


fn parseFile(allocator: Allocator, path: []const u8) !void {
    var src = try kule.Source.fromFileLocal(allocator, path);
    defer src.deinitFile(allocator);
    var unit = compiler.CompilationUnit.init(allocator, &src);
    defer unit.deinit();
    if (compiler.parseUnit(&unit)) {
        kule.logger.info("parsed file {s}", .{path});
    }
    else |err| {
        kule.logger.err("{s} failed with {d} errors ({s})", .{path, unit.diagnostics.error_count, @errorName(err)});
    }
    unit.diagnostics.logMessages();
}
