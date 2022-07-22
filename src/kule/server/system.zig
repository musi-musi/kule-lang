const std = @import("std");
const server = @import("server.zig");

const Server = server.Server;

pub const initialize = struct {
    clientInfo: struct {
        name: []const u8,
        version: []const u8,
    },
    rootPath: []const u8,
    rootUri: []const u8,
    // capabilities: struct {
    //     textDocument: struct {
    //         publishDiagnostics: struct {
    //             relatedInformation: bool,
    //             versionSupport: bool,
    //         },
    //     },
    // },

    pub fn respond(s: *Server, request: anytype) !void {
        // _ = s;
        // _ = request;
        // std.log.debug("{s}", .{request.params.?.clientInfo.name});
        try s.result(request.params);
    }
};