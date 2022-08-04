const std = @import("std");
const rpc = @import("../rpc.zig");
const server = @import("../server.zig");
const json = @import("../json.zig");

const Server = server.Server;
const Request = rpc.Request;
const Value = json.Value;

const server_capabilities = .{
    .textDocumentSync = 1,
    .hoverProvider = true,
};

pub fn initialize(s: *Server, request: Request) !void {
    _ = s;
    if (request.params(struct {
        rootUri: ?[]const u8,
        workspaceFolders: ?Value 
    })) |p| {
        if (p.workspaceFolders) |folders| {
            var f = folders;
            while (f.parseNext(struct {
                uri: []const u8,
                name: []const u8,
            })) |folder| {
                _ = try s.workspace.addProject(folder.uri);
            }
        }
        else if (p.rootUri) |uri| {
            _ = try s.workspace.addProject(uri);
        }
    }
    try request.respond(.{
        .capabilities = server_capabilities,
        .serverInfo = .{
            .name = "kule language server",
        },
    });
}

pub fn initialized(s: *Server, _: Request) !void {
    try s.initialized();
}

pub fn shutdown(s: *Server, request: Request) !void {
    s.shutdown();
    try request.respond({});
}

pub fn exit(s: *Server, request: Request) !void {
    s.exit();
    try request.respond({});
}

