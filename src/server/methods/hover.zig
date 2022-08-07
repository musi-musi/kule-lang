
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

const Meta = kule.language.Semantics.Meta;

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
                    const meta_opt: ?*const Meta = (
                        for (line) |*c| {
                            if (sem.data.addr_map.get(@ptrToInt(c))) |node| {
                                if (sem.data.map.get(node)) |*meta| {
                                    if (meta.token) |token| {
                                        const start = @ptrToInt(token.ptr);
                                        const end = start + token.len;
                                        if (addr >= start and addr < end) {
                                            break meta;
                                        }
                                    }
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
                        var text = std.ArrayList(u8).init(s.allocator);
                        defer text.deinit();
                        const writer = text.writer();
                        try writer.writeAll("```kule\n");
                        const name_start = text.items.len;
                        if (meta.value_symbol) |vs| {
                            try writer.print("{q}", .{vs});
                        }
                        else {
                            try writer.print("{s}", .{meta.token.?});
                        }
                        const name_end = text.items.len;
                        if (meta.ktype) |ktype| {
                            if (meta.ktype_symbol) |ts| {
                                const sym_txt = try std.fmt.allocPrint(s.allocator, "{q}", .{ts});
                                defer s.allocator.free(sym_txt);
                                const typ_txt = try std.fmt.allocPrint(s.allocator, "{}", .{ktype});
                                defer s.allocator.free(typ_txt);
                                if (std.mem.eql(u8, sym_txt, typ_txt)) {
                                    try writer.print(": {s}\n", .{typ_txt});
                                }
                                else {
                                    try writer.print(": {s} ({s})\n", .{sym_txt, typ_txt});
                                }
                            }
                            else {
                                try writer.print(": {}\n", .{ktype});
                            }
                            if (meta.value()) |value| {
                                const value_start = text.items.len;
                                try writer.print("{~}", .{value});
                                const value_text = text.items[value_start..];
                                const name_text = text.items[name_start..name_end];
                                if (std.mem.eql(u8, value_text, name_text)) {
                                    text.items.len -= value_text.len;
                                }
                                else {
                                    try writer.writeByte('\n');
                                }
                            }
                        }
                        else {
                            try writer.writeByte('\n');
                        }
                        try writer.writeAll("```");
                        try request.respond(Hover {
                            .contents = .{
                                .kind = "markdown",
                                .value = text.items,
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