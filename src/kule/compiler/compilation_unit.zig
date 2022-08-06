const std = @import("std");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const compiler = @import("../compiler.zig");
const diagnostics = @import("../diagnostics.zig");
const language = @import("../language.zig");

const Semantics = language.Semantics;


const Syntax = language.Syntax;
const Module = Semantics.Module;

const Diagnostics = diagnostics.Diagnostics;

const Source = compiler.Source;


pub const CompilationUnit = struct {

    source: *const Source,
    allocator: Allocator,
    arena: ArenaAllocator,
    diagnostics: Diagnostics,

    syntax: ?Syntax = null,
    semantics: ?Semantics = null,

    const Self = @This();

    pub fn init(allocator: Allocator, source: *const Source) Self {
        var self = Self {
            .source = source,
            .allocator = allocator,
            .arena = ArenaAllocator.init(allocator),
            .diagnostics = Diagnostics.init(allocator, source),
        };
        return self;
    }

    pub fn initSemantics(self: *Self) *Semantics {
        self.semantics = Semantics.init(self.arena.allocator(), self.name());
        return &(self.semantics.?);
    }

    pub fn name(self: Self) []const u8 {
        const str = self.source.displayName();
        const ext: []const u8 = ".kule";
        if (std.mem.endsWith(u8, str, ext)) {
            return str[0..str.len - ext.len];
        }
        else {
            return str;
        }

    }

    pub fn initSyntax(self: *Self) *Syntax {
        self.syntax = Syntax{};
        return &(self.syntax.?);
    }

    pub fn deinit(self: Self) void {
        self.diagnostics.deinit();
        self.arena.deinit();
    }

};