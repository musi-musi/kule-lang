const std = @import("std");
const lexer = @import("lexer.zig");
const types = @import("types.zig");
const analysis = @import("analysis.zig");
const source = @import("../source.zig");

const Source = source.Source;

const Symbol = analysis.Symbol;
const Token = lexer.Token;
const Scope = analysis.Scope;

const Type = types.Type;
const Value = types.Value;



pub const Operator = enum {
    add, subtract, multiply, divide, negate,

    pub fn fromBinary(operator: Token) Operator {
        return switch (operator.tag) {
            .plus => .add,
            .minus => .subtract,
            .aster => .multiply,
            .fslash => .divide, 
            else => std.debug.panic("unexpected binary operator token: {s} '{s}')", .{@tagName(operator.tag), operator.text}),
        };
    }

    pub fn fromUnary(operator: Token) Operator {
        return switch (operator.tag) {
            .minus => .negate,
            else => std.debug.panic("unexpected unary operator token: {s} '{s}')", .{@tagName(operator.tag), operator.text}),
        };
    }

};

pub const ExprNode = struct {
    expr: Expr,
    expr_type: Type = undefined,

    value: ?Value = null,

    pub fn init(expr: anytype) ExprNode {
        const node_expr = switch (@TypeOf(expr)) {
            Expr.Name => Expr{ .name = expr },
            Expr.Binary => Expr{ .binary = expr },
            Expr.Unary => Expr{ .unary = expr },
            Expr.NumLit => Expr{ .num_lit = expr },
            Expr.DotMember => Expr{ .dot_member = expr },
            Expr.EvalParams => Expr{ .eval_params = expr },
            Expr.ModuleDef => Expr{ .module_def = expr },
            Expr.Import => Expr{ .import = expr },
            else => |Val| @compileError(@typeName(Val) ++ "is not a member of Expr"),
        };
        return ExprNode {
            .expr = node_expr,
        };
    }

    /// if this expr has a determined value, and that value is a Type, return it
    /// otherwise, return null
    pub fn typeValue(expr: ExprNode) ?Type {
        if (expr.value) |value| {
            if (value.data == .type_value) {
                return value.data.type_value;
            }
        }
        return null;
    }
};

pub const Expr = union(enum) {

    name: Name,
    binary: Binary,
    unary: Unary,
    num_lit: NumLit,
    dot_member: DotMember,
    eval_params: EvalParams,
    module_def: ModuleDef,
    import: Import,

    pub const Name = struct {
        name: Token,
        symbol: ?Symbol = null,
    };

    pub const Binary = struct {
        operator: Token,
        lhs: *ExprNode,
        rhs: *ExprNode,
    };

    pub const Unary = struct {
        operator: Token,
        operand: *ExprNode,
    };

    pub const NumLit = struct {
        literal: Token,
        value: Value,
    };

    pub const DotMember = struct {
        container: *ExprNode,
        member_name: Token,
        dot: Token,
        symbol: ?Symbol = null,
    };

    pub const EvalParams = struct {
        subject: *ExprNode,
        params: []ExprNode,
        lparen: Token,
        rparen: Token,
    };

    pub const ModuleDef = struct {
        module: Module,
        kw_module: Token,
        lcurly: Token,
        rcurly: Token,
    };

    pub const Import = struct {
        kw_import: Token,
        path: Token,
        module: ?*Module = null,
    };

};

pub const TypeExpr = struct {
    colon: Token,
    expr: *ExprNode,
};

pub const Module = struct {

    decls: []Decl,
    name: ?[]const u8 = null,
    scope: ?*Scope = null,
    parent: ?*Module = null,

    pub fn format(m: Module, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
        if (m.parent) |parent| {
            try parent.format("", .{}, writer);
            try writer.writeByte('.');
        }
        if (m.name) |name| {
            try writer.writeAll(name.text);
        }
        else {
            try writer.writeAll(".???");
        }
    }


};

pub const Decl = struct {

    name: Token,
    equal: Token,
    is_pub: bool,
    expr: ExprNode,
    type_expr: ?TypeExpr,
    symbol: Symbol = undefined,
    function: ?*Function,

    where_clauses: []Decl,

    scope: ?*Scope = null,
    module: *Module = undefined,

    pub fn function(self: *Decl) ?*Function {
        if (self.expr.expr == .function_def) {
            return self.expr.expr.function_def.function;
        }
        else {
            return null;
        }
    }

    pub fn format(d: Decl, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        try d.module.format(fmt, options, writer);
        try writer.writeByte('.');
        try writer.writeAll(d.name.text);
    }

};

pub const Param = struct {
    name: Token,
    type_expr: TypeExpr,
    index: usize = undefined,
    function: *Function = undefined,
    symbol: Symbol = undefined,
    param_type: ?Type = null,

    pub fn format(p: Param, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        try writer.print("parameter '{s}' of {}", .{p.name.text, p.function});
    }

};


pub const Function = struct {
    params: []Param,
    expr: ExprNode,
    type_expr: ?TypeExpr,
    scope: *Scope = undefined,
    decl: ?*Decl = null,

    pub fn format(f: Function, comptime fmt: []const u8, options: std.fmt.FormatOptions, writer: anytype) !void {
        if (f.decl) |decl| {
            try decl.format(fmt, options, writer);
            try writer.writeAll("(..)");
        }
        else {
            try writer.writeAll("anonymous function");
        }
    }
};
