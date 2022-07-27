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
            text: json.String,
        },
    })) |p| {
        const uri = p.textDocument.uri;
        const text = p.textDocument.text;
        const file = try s.workspace.addOrUpdateFile(uri, text);
        try s.log(.log, "open {s}", .{file.name});
    }
}

pub fn @"textDocument/didClose"(s: *Server, request: Request) !void {
    if (request.params(struct {
        textDocument: struct {
            uri: []const u8,
        },
    })) |p| {
        const uri = p.textDocument.uri;
        if (s.workspace.removeFile(uri)) {
            try s.log(.log, "remove file {s}", .{uri});
        }
    }
}

pub fn @"textDocument/didChange"(s: *Server, request: Request) !void {
    if (request.params(struct {
        textDocument: struct {
            uri: []const u8,
            text: json.String,
        },
    })) |p| {
        const uri = p.textDocument.uri;
        const text = p.textDocument.text;
        const file = try s.workspace.addOrUpdateFile(uri, text);
        try s.log(.log, "update {s}", .{file.name});
    }
}

pub fn @"textDocument/didSave"(s: *Server, request: Request) !void {
    _ = s;
    _ = request;
}
