const std = @import("std");
const kule = @import("../kule.zig");
const server = @import("server.zig");
const workspace = @import("workspace.zig");
const lsp = @import("lsp.zig");
const json = @import("json.zig");

const Allocator = std.mem.Allocator;

const Server = server.Server;

const Source = kule.Source;
const Diagnostics = kule.diagnostics.Diagnostics;
const SourceModule = kule.compiler.SourceModule;


const File = workspace.File;

const SourceDiags = std.StringHashMapUnmanaged(*SourceDiag);

pub const Validation = struct {
    allocator: Allocator,
    diags: SourceDiags,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self {
            .allocator = allocator,
            .diags = .{},
        };
    }

    pub fn validateFile(self: *Self, file: *File) !*SourceDiag {
        _ = self.removeFile(file.uri);
        const src_diag = try self.allocator.create(SourceDiag);
        src_diag.* = try SourceDiag.init(self.allocator, file);
        src_diag.module = kule.compiler.parseSource(
            self.allocator,
            &src_diag.source,
            &src_diag.diag,
        ) catch null;
        try self.diags.put(self.allocator, file.uri, src_diag);
        return src_diag;
    }

    pub fn removeFile(self: *Self, uri: []const u8) bool {
        if (self.diags.fetchRemove(uri)) |kv| {
            var src_diag = kv.value;
            src_diag.deinit(self.allocator);
            self.allocator.destroy(src_diag);
            return true;
        }
        else {
            return false;
        }
    }

    pub fn deinit(self: *Self) void {
        var diags = self.diags.valueIterator();
        while (diags.next()) |ptr| {
            const diag = ptr.*;
            diag.deinit(self.allocator);
            self.allocator.destroy(diag);
        }
        self.diags.deinit(self.allocator);
    }

};

pub const SourceDiag = struct {
    uri: []const u8,
    source: Source,
    diag: Diagnostics,
    module: ?SourceModule,

    pub fn init(allocator: Allocator, file: *File) !SourceDiag {
        return SourceDiag {
            .uri = file.uri,
            .source = try Source.init(allocator, file.name, file.text),
            .diag = Diagnostics.init(allocator),
            .module = null,
        };
    }

    pub fn deinit(self: *SourceDiag, allocator: Allocator) void {
        self.source.deinit(allocator);
        self.diag.deinit();
        if (self.module) |module| {
            module.deinit();
        }
    }


    pub fn publish(self: *SourceDiag, s: *Server) !void {
        const msgs = self.diag.messages.items;
        const diags = try s.allocator.alloc(lsp.Diagnostic, msgs.len);
        defer s.allocator.free(diags);
        for (msgs) |msg, i| {
            diags[i] = lsp.Diagnostic.fromSrcMsg(msg);
        }
        const diag_note = .{
            .uri = self.uri,
            .diagnostics = diags,
        };
        // const j = try json.serialize(s.allocator, diag_note);
        // defer s.allocator.free(j);
        // try s.log(.log, ">>{s}<<", .{j});
        try s.notifyClient("textDocument/publishDiagnostics", diag_note);
    }

};