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
        self.semantics = Semantics.init(self.arena.allocator(), self.source.displayName());
        return &(self.semantics.?);
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