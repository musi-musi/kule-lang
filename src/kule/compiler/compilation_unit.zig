const std = @import("std");

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const source = @import("../source.zig");
const diagnostics = @import("../diagnostics.zig");
const expression = @import("expression.zig");


const Module = expression.Module;
const Diagnostics = diagnostics.Diagnostics;

const Source = source.Source;

pub const CompilationUnit = struct {

    source: *const Source,
    allocator: Allocator,
    arena: ArenaAllocator,
    diagnostics: Diagnostics,

    root_module: Module,

    pub fn init(allocator: Allocator, src: *const Source) CompilationUnit {
        var self = CompilationUnit {
            .source = src,
            .allocator = allocator,
            .arena = ArenaAllocator.init(allocator),
            .diagnostics = Diagnostics.init(allocator, src),
            .root_module = Module {
                .decls = &.{},
                .name = src.name,
            },
        };
        return self;
    }

    pub fn deinit(self: CompilationUnit) void {
        self.arena.deinit();
        self.diagnostics.deinit();
    }

};