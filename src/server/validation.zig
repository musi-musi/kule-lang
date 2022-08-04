const std = @import("std");
const kule = @import("../kule.zig");
const server = @import("server.zig");
const workspace = @import("workspace.zig");
const lsp = @import("lsp.zig");
const json = @import("json.zig");

const compiler = kule.compiler;

const Allocator = std.mem.Allocator;

const Server = server.Server;

const Source = compiler.Source;
const Diagnostics = kule.diagnostics.Diagnostics;
const CompilationUnit = compiler.CompilationUnit;


const File = workspace.File;

const Units = std.StringHashMapUnmanaged(*CompilationUnit);

pub const Validation = struct {
    allocator: Allocator,
    units: Units,

    const Self = @This();

    pub fn init(allocator: Allocator) Self {
        return Self {
            .allocator = allocator,
            .units = .{},
        };
    }

    pub fn validateFile(self: *Self, file: *File) !*CompilationUnit {
        _ = self.removeFile(file.uri);
        const unit = try self.allocator.create(CompilationUnit);
        errdefer self.allocator.destroy(unit);
        const source = try self.allocator.create(Source);
        errdefer self.allocator.destroy(source);
        source.* = try Source.init(self.allocator, file.name, file.text);
        errdefer source.deinit(self.allocator);
        unit.* = CompilationUnit.init(self.allocator, source);
        kule.compiler.parseUnit(unit) catch {};
        kule.compiler.analyzeUnitExt(unit, true) catch {};
        try self.units.put(self.allocator, file.uri, unit);
        return unit;
    }

    pub fn removeFile(self: *Self, uri: []const u8) bool {
        if (self.units.fetchRemove(uri)) |kv| {
            var unit = kv.value;
            unit.source.deinit(self.allocator);
            self.allocator.destroy(unit.source);
            unit.deinit();
            self.allocator.destroy(unit);
            return true;
        }
        else {
            return false;
        }
    }

    pub fn deinit(self: *Self) void {
        var units = self.units.valueIterator();
        while (units.next()) |ptr| {
            const unit = ptr.*;
            unit.source.deinit(self.allocator);
            self.allocator.destroy(unit.source);
            unit.deinit();
            self.allocator.destroy(unit);
        }
        self.units.deinit(self.allocator);
    }

    pub fn publishFileDiagnostics(self: Self, uri: []const u8, s: *Server) !void {
        if (self.units.get(uri)) |unit| {
            const msgs = unit.diagnostics.messages.items;
            const lsp_diags = try s.allocator.alloc(lsp.Diagnostic, msgs.len);
            defer s.allocator.free(lsp_diags);
            for (msgs) |msg, i| {
                lsp_diags[i] = try lsp.Diagnostic.fromSrcMsgWithRelated(s.allocator, uri, msg.*);
            }
            const diag_note = .{
                .uri = uri,
                .diagnostics = lsp_diags,
            };
            // const j = try json.serialize(s.allocator, diag_note);
            // defer s.allocator.free(j);
            // try s.log(.log, ">>{s}<<", .{j});
            try s.notifyClient("textDocument/publishDiagnostics", diag_note);
        }
    }

};