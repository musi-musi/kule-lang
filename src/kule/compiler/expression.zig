const std = @import("std");
const lexer = @import("lexer.zig");
const types = @import("types.zig");
const scoping = @import("scoping.zig");
const source = @import("../source.zig");

const Source = source.Source;

const Symbol = scoping.Symbol;
const Token = lexer.Token;
const Type = types.Type;
const Scope = scoping.Scope;

pub const SourceModule = struct {

    source: *const Source,
    arena: std.heap.ArenaAllocator,

    module: Module,

    pub fn deinit(self: SourceModule) void {
        self.arena.deinit();
    }

};

pub const Operator = enum {
    add, sub, mul, div, cast, neg, pos,
};

pub const Expr = struct {
    val: ExprVal,
    val_type: ?Type = null,

    pub fn init(val: anytype) Expr {
        const expr_val = switch (@TypeOf(val)) {
            ExprVal.Name => ExprVal{ .name = val },
            ExprVal.Binary => ExprVal{ .binary = val },
            ExprVal.Unary => ExprVal{ .unary = val },
            ExprVal.NumLit => ExprVal{ .num_lit = val },
            ExprVal.DotMember => ExprVal{ .dot_member = val },
            ExprVal.EvalParams => ExprVal{ .eval_params = val },
            ExprVal.ModuleDef => ExprVal{ .module_def = val },
            ExprVal.Import => ExprVal{ .import = val },
            else => |Val| @compileError(@typeName(Val) ++ "is not a member of ExprVal"),
        };
        return Expr {
            .val = expr_val,
            .val_type = expr_val.valueType(),
        };
    }
};

pub const ExprVal = union(enum) {

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

    pub fn valueType(self: ExprVal) ?Type {
        return switch (self) {
            .num_lit => types.number_type,
            else => null,
        };
    }

    pub const Binary = struct {
        operator: Token,
        lhs: *Expr,
        rhs: *Expr,
    };

    pub const Unary = struct {
        operator: Token,
        operand: *Expr,
    };

    pub const NumLit = struct {
        literal: Token,
        value: types.number_type.Val(),
    };

    pub const DotMember = struct {
        container: *Expr,
        member_name: Token,
        symbol: ?Symbol = null,
    };

    pub const EvalParams = struct {
        subject: *Expr,
        params: []Expr,
    };

    pub const ModuleDef = struct {
        module: Module,
    };

    pub const Import = struct {
        path: Token,
        module: ?*Module = null,
    };
};

pub const Module = struct {

    decls: []Decl,

    scope: ?*Scope = null,

};

pub const Decl = struct {

    name: Token,
    is_pub: bool,
    expr: Expr,
    params: []ParamDef,
    type_expr: ?Expr,
    symbol: ?Symbol = null,

    where_clauses: []Decl,

    scope: ?*Scope = null,

};

pub const ParamDef = struct {
    name: Token,
    type_expr: ?Expr,
    index: usize = undefined,
    scope: ?*Scope = null,
};