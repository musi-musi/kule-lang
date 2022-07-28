const std = @import("std");
const rpc = @import("../rpc.zig");
const server = @import("../server.zig");
const json = @import("../json.zig");
const kule = @import("../../kule.zig");

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
        const diags = try s.validation.validateFile(file);
        try diags.publish(s);
        if (diags.module == null) {
            try s.log(.log, "{s} failed to parse ({d} errors)", .{file.name, diags.diag.error_count});
        }

        // try s.log(.log, "open {s} ({d} errors)", .{file.name, diags.diag.err_count});
        // try s.log(.log, "open {s}", .{file.name});
        // try sourceDump(s, &diags.source);
        // try s.log(.log, ">>>>>>\n{s}\n<<<<<<", .{text.raw});
        // try s.log(.log, "======\n{s}\n======", .{file.text});
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
        if (s.validation.removeFile(uri)) {
            try s.log(.log, "remove file validation {s}", .{uri});
        }
    }
}

pub fn @"textDocument/didChange"(s: *Server, request: Request) !void {
    if (request.params(struct {
        textDocument: struct {
            uri: []const u8,
        },
        contentChanges: Value,
    })) |p| {
        var changes = p.contentChanges;
        if (changes.next() != null) {
            if (changes.parse(struct {
                text: json.String,
            })) |change| {
                const uri = p.textDocument.uri;
                const text = change.text;
                const file = try s.workspace.addOrUpdateFile(uri, text);
                const diags = try s.validation.validateFile(file);
                try diags.publish(s);

                if (diags.module == null) {
                    try s.log(.log, "{s} failed to parse ({d} errors)", .{file.name, diags.diag.error_count});
                }

                // try s.log(.log, "update {s}", .{file.name});
                // try s.log(.log, "update {s} ({d} errors)", .{file.name, diags.diag.err_count});
                // try sourceDump(s, &diags.source);
            }
        }
        // try s.log(.log, ">>>>>>\n{s}\n<<<<<<", .{text.raw});
        // try s.log(.log, "======\n{s}\n======", .{file.text});

    }
}

fn sourceDump(s: *Server, source: *const kule.Source) !void {
    try s.log(.log, "lines: {d}", .{source.lines.len});
    // for (source.lines) |line| {
    //     try s.log(.log, "{x:0>16}@{s}@", .{@ptrToInt(line.ptr), line});
    // }
}

pub fn @"textDocument/didSave"(s: *Server, request: Request) !void {
    if (request.params(struct {
        textDocument: struct {
            uri: []const u8,
        },
    })) |p| {
        const uri = p.textDocument.uri;
        if (s.validation.diags.get(uri)) |diags| {
            try diags.publish(s);
        }
    }
}