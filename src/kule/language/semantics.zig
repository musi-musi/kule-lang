const std = @import("std");

const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;

const language = @import("../language.zig");

const Taip = language.Taip;
const Value = language.Value;

const Syntax = language.Syntax;

const Expr = Syntax.Expr;

fn AddrMap(comptime V: type) type {
    return std.HashMapUnmanaged(usize, V, struct {

        fn hash(_: @This(), addr: usize) u64 {
            if (@sizeOf(usize) <= @sizeOf(u64)) {
                return @intCast(u64, addr);
            }
            else {
                return @truncate(u64, addr);
            }
        }

        fn eql(_: @This(), a: usize, b: usize) bool {
            return a == b;
        }

    }, 80);
}

pub const Semantics = struct {

    module: Module = .{
        .scope = .{},
        .statements = &.{},
    },

    pub const ExprTaips = AddrMap(Taip);
    pub const ExprValues = AddrMap(Value);

    pub const Module = struct {
        scope: *Scope,
        statements: []Statement,
    };

    pub const Statement = struct {

        visibility: Visibility,
        body: Body,

        pub const Visibility = enum {
            /// visible only to the source file the statement was declared in  
            local,
            /// visible wherever imported
            public,
        };

        pub const Body = union(enum) {
            binding: Binding,
        };
    };

    pub const Binding = struct {
        decl: Decl,
        where_clauses: []Decl,
    };

    pub const Decl = struct {
        name: []const u8,
        taip: Taip,
        body: Body,

        pub const Body = union(enum) {
            direct: Expr,
            function: Function,
        };

    };

    pub const Function = struct {
        
        params: []Param,
        return_taip: Taip,
        body: Expr,

        pub const Param = struct {
            name: []const u8,
            taip: Taip,
        };

    };

    pub const Scope = struct {
        parent: ?*Scope = null,

    };

    pub const Symbol = struct {

    };

};