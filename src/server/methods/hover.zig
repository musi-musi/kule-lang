
const std = @import("std");
const rpc = @import("../rpc.zig");
const server = @import("../server.zig");
const json = @import("../json.zig");
const kule = @import("../../kule.zig");

const Server = server.Server;
const Request = rpc.Request;
const Value = json.Value;

const Hover = struct {
    contents: struct {
        kind: []const u8 = "markdown",
        value: []const u8,
    },
};

const Metadata = kule.language.Semantics.Metadata;

pub fn @"textDocument/hover"(s: *Server, request: Request) !void {
    if (request.params(struct {
        textDocument: struct {
            uri: []const u8,
        },
        position: struct {
            line: usize,
            character: usize,
        }
    })) |p| {
        const uri = p.textDocument.uri;
        const pos = p.position;
        if (s.validation.units.get(uri)) |unit| {
            if (unit.semantics != null) {
                const sem = &(unit.semantics.?);
                const lines = unit.source.lines;
                if (pos.line < lines.len and pos.character < lines[pos.line].len) {
                    const line = lines[pos.line];
                    const addr = @ptrToInt(&line[pos.character]);
                    const meta_opt: ?Metadata = (
                        for (line) |*c| {
                            if (sem.meta_map.get(@ptrToInt(c))) |meta| {
                                const start = @ptrToInt(meta.slice.ptr);
                                const end = start + meta.slice.len;
                                if (addr >= start and addr < end) {
                                    break meta;
                                }
                            }
                        }
                        else null
                    );
                    // _ = sem;
                    // // var buffer: [80]u8 = undefined;
                    // // const text: []const u8 = std.fmt.bufPrint(&buffer, "fuck you {d}", .{pos.line}) catch "please fucking work";
                    // try request.respond(Hover {
                    //     .contents = .{
                    //         .value = try std.fmt.allocPrint(s.allocator, "{d}:{d} {x}", .{pos.line, pos.character, @ptrToInt(addr)}),
                    //     },
                    // });
                    // return;
                    if (meta_opt) |meta| {
                        const text = blk: {
                            if (meta.value) |value| {
                                break :blk try std.fmt.allocPrint(s.allocator, "```kule\n{s}: {}\n = {}\n```", .{meta.slice, meta.taip, value});
                            }
                            else {
                                break :blk try std.fmt.allocPrint(s.allocator, "```kule\n{s}: {}\n```", .{meta.slice, meta.taip});
                            }
                        };
                        defer s.allocator.free(text);
                        try request.respond(Hover {
                            .contents = .{
                                .kind = "markdown",
                                .value = text,
                            },
                            // .range = .{
                            //     .start = .{
                            //         .line = pos.line,
                            //         .character = pos.character,
                            //     },
                            //     .end = .{
                            //         .line = pos.line,
                            //         .character = pos.character + meta.slice.len,
                            //     },
                            // },
                        });
                        return;
                    }
                    // else {
                    //     const line = lines[pos.line];
                    //     const char = pos.character;
                    //     const text = blk: {
                    //         if (char < line.len - 1) {
                    //             break :blk try std.fmt.allocPrint(s.allocator, "{s}`{c}`{s}", .{line[0..char], line[char], line[char+1..]});
                    //         }
                    //         else {
                    //             break :blk try std.fmt.allocPrint(s.allocator, "{s}`{c}`", .{line[0..char], line[char]});
                    //         }
                    //     };
                    //     defer s.allocator.free(text);
                    //     try request.respond(Hover {
                    //         .contents = .{
                    //             .kind = "markdown",
                    //             .value = text,
                    //         },
                    //         // .range = .{
                    //         //     .start = .{
                    //         //         .line = pos.line,
                    //         //         .character = pos.character,
                    //         //     },
                    //         //     .end = .{
                    //         //         .line = pos.line,
                    //         //         .character = pos.character + meta.slice.len,
                    //         //     },
                    //         // },
                    //     });
                    //     return;
                    // }
                }
            }
        }
        try request.respondNull();

    }
}