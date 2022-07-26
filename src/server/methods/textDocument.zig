const std = @import("std");
const rpc = @import("../rpc.zig");
const server = @import("../server.zig");
const json = @import("../json.zig");

const Server = server.Server;
const Request = rpc.Request;
const Value = json.Value;

pub fn @"textDocument/didOpen"(s: *Server, request: Request) !void {
    if (request.params(struct {
        textDocument: struct {
            uri: []const u8,
        },
    })) |p| {
        try s.log(.log, "opened {s}", .{p.textDocument.uri});
    }
}

pub fn @"textDocument/didClose"(s: *Server, request: Request) !void {
    if (request.params(struct {
        textDocument: struct {
            uri: []const u8,
        },
    })) |p| {
        try s.log(.log, "closed {s}", .{p.textDocument.uri});
    }
}

pub fn @"textDocument/didChange"(s: *Server, request: Request) !void {
    _ = s;
    _ = request;
}

pub fn @"textDocument/didSave"(s: *Server, request: Request) !void {
    _ = s;
    _ = request;
}