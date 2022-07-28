const std = @import("std");
const types = @import("types.zig");
const parser = @import("parser.zig");

const Type = types.Type;
const Ast = parser.Ast;

const Expr = Ast.Expr;


pub const Symbol = struct {
    name: []const u8,
    value: Value,

    const Self = @This();

};


pub const Table = struct {
    symbols: Map,
    parent: ?*Table,

    pub const Map = std.StringHashMapUnmanaged(Symbol);

    const Self = @This();

};
