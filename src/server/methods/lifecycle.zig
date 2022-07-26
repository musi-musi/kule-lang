const std = @import("std");
const rpc = @import("../rpc.zig");
const server = @import("../server.zig");
const json = @import("../json.zig");

const Server = server.Server;
const Request = rpc.Request;
const Value = json.Value;

const capabilities = .{
    .textDocumentSync = 1,
};

pub fn initialize(s: *Server, request: Request) !void {
    _ = s;
    if (request.params(struct {
        rootPath: ?[]const u8,
        rootUri: ?[]const u8,
        workspaceFolders: ?Value 
    })) |p| {
        if (p.rootPath) |path| {
            try s.log(.log, "root path: {s}", .{ path });
        }
        if (p.rootUri) |uri| {
            try s.log(.log, "root uri: {s}", .{ uri });
        }
        if (p.workspaceFolders) |folders| {
            var f = folders;
            while (f.parseNext(struct {
                uri: []const u8,
                name: []const u8,
            })) |folder| {
                try s.log(.log, "workspace folder [{s}]: {s}", .{ folder.name, folder.uri});
            }
        }
    }
    try request.respond(.{
        .capabilities = capabilities,
        .serverInfo = .{
            .name = "kule language server",
        },
    });
}

pub fn initialized(s: *Server, _: Request) !void {
    _ = s;
}

pub fn shutdown(s: *Server, request: Request) !void {
    s.shutdown();
    try request.respond({});
}

pub fn exit(s: *Server, request: Request) !void {
    s.exit();
    try request.respond({});
}

