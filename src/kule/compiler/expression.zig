const std = @import("std");
const lexer = @import("lexer.zig");
const types = @import("types.zig");

const Token = lexer.Token;
const Type = types.Type;

const number_type = Type {
    .scalar = .number,
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
        expr: ?*Expr = null,
        origin: ?Origin = null,

        pub const Origin = union(enum) {
            decl: *Decl,
            param: *ParamDef,
            where: *Decl,
        };
    };

    pub fn valueType(self: ExprVal) ?Type {
        return switch (self) {
            .num_lit => number_type,
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
    };

    pub const DotMember = struct {
        container: *Expr,
        member_name: Token,
        decl: ?*Decl = null,
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

};

pub const Decl = struct {

    name: Token,
    is_pub: bool,
    expr: Expr,
    params: []ParamDef,
    type_expr: ?Expr,

    where_clauses: []Decl,

};

pub const ParamDef = struct {
    name: Token,
    type_expr: ?Expr,
};