const std = @import("std");
const types = @import("types.zig");
const expression = @import("expression.zig");
const lexer = @import("lexer.zig");
const diagnostics = @import("../diagnostics.zig");

const Allocator = std.mem.Allocator;

const Diagnostics = diagnostics.Diagnostics;

const Token = lexer.Token;

const Type = types.Type;
const Value = types.Value;
const Scalar = types.Scalar;
const Vector = types.Vector;

const valueType = types.valueType;

const SourceModule = expression.SourceModule;
const Expr = expression.Expr;
const Decl = expression.Decl;
const ParamDef = expression.ParamDef;
const Module = expression.Module;

const builtin_types = types.builtin_types;

const SymbolMap = std.StringHashMapUnmanaged(Symbol);

pub const Symbol = union(enum) {
    decl: *Decl,
    param: *ParamDef,
    builtin_type: Type,

    fn init(val: anytype) Symbol {
        return switch (@TypeOf(val)) {
            *Decl => Symbol { .decl = val },
            *ParamDef => Symbol { .param = val },
            Type => Symbol { .builtin_type = val },
            else => |Val| @compileError(@typeName(Val) ++ " is not a field of Symbol"),
        };
    }

    fn kindText(symbol: Symbol) []const u8 {
        return switch (symbol) {
            .decl => "declaration",
            .param => "parameter",
            .builtin_type => "builtin type",
        };
    }

    fn nameToken(symbol: Symbol) ?Token {
        return switch(symbol) {
            .decl => |decl| decl.name,
            .param => |param| param.name,
            else => null,
        };
    }
};

fn findBuiltinTypeSymbol(name: []const u8) ?Symbol {
    if (builtin_types.get(name)) |builtin_type| {
        return Symbol {
            .builtin_type = builtin_type,
        };
    }
    else {
        return null;
    }
}

pub const ScopeContainer = union(enum) {
    module: *Module,
    decl: *Decl,
};

pub const Scope = struct {
    parent: ?*Scope,
    symbols: SymbolMap,
    container: ScopeContainer,

    const Error = Allocator.Error;

    pub fn init(parent: ?*Scope, container: anytype) Scope {
        return .{
            .parent = parent,
            .symbols = .{},
            .container = switch (@TypeOf(container)) {
                *Module => ScopeContainer{ .module = container },
                *Decl => ScopeContainer{ .decl = container },
                else => |C| @compileError(@typeName(C) ++ " is not a member of ScopeContainer"),
            },
        };
    }

    pub fn findSymbol(self: Scope, name: []const u8) ?Symbol {
        if (findBuiltinTypeSymbol(name)) |symbol| {
            return symbol;
        }
        else if (self.symbols.get(name)) |symbol| {
            return symbol;
        }
        else if (self.parent) |parent| {
            return parent.findSymbol(name);
        }
        else {
            return null;
        }
    }

};

pub fn analyzeSourceModule(allocator: Allocator, source_module: *SourceModule, diags: ?*Diagnostics) !void {
    var analyzer = Analyzer.init(allocator, source_module, diags);
    try analyzer.createModuleScope(null, &source_module.module);
    analyzer.referenceModuleSymbols(&source_module.module);
}

pub const Analyzer = struct {
    allocator: Allocator,
    source_module: *SourceModule,
    diags: ?*Diagnostics,
    error_count: usize = 0,

    const Self = @This();

    const Error = Allocator.Error;


    pub fn init(allocator: Allocator, source_module: *SourceModule, diags: ?*Diagnostics) Self {
        return Self {
            .allocator = allocator,
            .source_module = source_module,
            .diags = diags,
        };
    }

    fn sourceError(self: *Self, token: []const u8, comptime format: []const u8, args: anytype) void {
        if (self.diags) |diags| {
            diags.sourceErrorErrorPanic(self.source_module.source, token, format, args);
            self.error_count += 1;
        }
    }

    fn sourceErrorNameShadow(self: *Self, token: []const u8, symbol: Symbol, existing: Symbol) void {
        self.sourceError(token, "{s} name shadows existing {s} {s}", .{symbol.kindText(), existing.kindText(), token});
    }


    fn checkScopeShadow(self: *Self, scope: *Scope) void {
        var symbols = scope.symbols.valueIterator();
        while (symbols.next()) |symbol| {
            self.checkSymbolShadow(symbol, scope);
        }
    }

    fn checkSymbolShadow(self: *Self, symbol: Symbol, scope: *Scope) void {
        const name = symbol.nameToken().?.text;
        const existing: ?Symbol = blk: {
            if (findBuiltinTypeSymbol(name)) |builtin_type_symbol| {
                break :blk builtin_type_symbol;
            }
            else if (scope.parent) |parent| {
                break :blk parent.findSymbol(name);
            }
            break :blk null;
        };
        if (existing != null) {
           self.sourceErrorNameShadow(name, symbol, existing.?);
        }
    }


    fn tryAddSymbol(self: *Self, scope: *Scope, name: []const u8, symbol_value: anytype) Error!void {
        const symbol = Symbol.init(symbol_value);
        if (scope.symbols.get(name)) |existing| {
            self.sourceErrorNameShadow(name, symbol, existing);
        }
        else {
            try scope.symbols.put(self.allocator, name, symbol);
        }
    }

    fn createModuleScope(self: *Self, parent: ?*Scope, module: *Module) Error!void {
        if (module.scope) |scope| {
            scope.parent = parent;
        }
        else {
            const scope = try self.allocator.create(Scope);
            errdefer self.allocator.destroy(scope);
            scope.* = Scope.init(parent, module);
            module.scope = scope;
            for (module.decls) |*decl| {
                try self.tryAddSymbol(scope, decl.name.text, decl);
                try self.createDeclScope( scope, decl);
            }
        }
    }

    fn createDeclScope(self: *Self, parent: ?*Scope, decl: *Decl) Error!void {
        if (decl.scope) |scope| {
            scope.parent = parent;
        }
        else {
            const scope = try self.allocator.create(Scope);
            errdefer self.allocator.destroy(scope);
            scope.* = Scope.init(parent, decl);
            decl.scope = scope;
            for (decl.params) |*param| {
                param.scope = scope;
                try self.tryAddSymbol(scope, param.name.text, param);
                if (param.type_expr) |*expr| {
                    try self.createExprScopes(scope, expr);
                }
            }
            for (decl.where_clauses) |*where_clause| {
                try self.tryAddSymbol(scope, where_clause.name.text, where_clause);
                try self.createDeclScope( scope, where_clause);
            }
            if (decl.type_expr) |*expr| {
                try self.createExprScopes(scope, expr);
            }
            try self.createExprScopes(scope, &decl.expr);
        }
    }

    fn createExprScopes(self: *Self, parent: ?*Scope, expr: *Expr) Error!void {
        switch (expr.val) {
            .binary => |binary| {
                try self.createExprScopes(parent, binary.lhs);
                try self.createExprScopes(parent, binary.rhs);
            },
            .unary => |unary| {
                try self.createExprScopes(parent, unary.operand);
            },
            .dot_member => |dot_member| {
                try self.createExprScopes(parent, dot_member.container);
            },
            .eval_params => |eval_params| {
                try self.createExprScopes(parent, eval_params.subject);
                for (eval_params.params) |*param| {
                    try self.createExprScopes(parent, param);
                }
            },
            .module_def => |*module_def| {
                const module = &module_def.module;
                try self.createModuleScope( parent, module);
            },
            else => {}
        }
    }

    pub fn referenceModuleSymbols(self: *Self, module: *Module) void {
        for (module.decls) |*decl| {
            _ = self.referenceDeclSymbols(decl);
        }
    }

    pub fn referenceDeclSymbols(self: *Self, decl: *Decl) void {
        const scope = decl.scope.?;
        self.checkSymbolShadow(Symbol.init(decl), scope.parent.?);
        for (decl.where_clauses) |*where_clause| {
            self.referenceDeclSymbols(where_clause);
        }
        for (decl.params) |*param| {
            self.checkSymbolShadow(Symbol.init(param), scope);
            if (param.type_expr) |*expr| {
                self.referenceExprSymbols(scope, expr);
            }
        }
        if (decl.type_expr) |*expr| {
            self.referenceExprSymbols(scope, expr);
        }
        self.referenceExprSymbols(scope, &decl.expr);
    }

    pub fn referenceExprSymbols(self: *Self, scope: *Scope, expr: *Expr) void {
        switch (expr.val) {
            .name => |*name| {
                if (scope.findSymbol(name.name.text)) |symbol| {
                    name.symbol = symbol;
                }
                else {
                    self.sourceError(name.name.text, "use of undeclared symbol \"{s}\"", .{name.name.text});
                }
            },
            .binary => |binary| {
                self.referenceExprSymbols(scope, binary.lhs);
                self.referenceExprSymbols(scope, binary.rhs);
            },
            .unary => |unary| {
                self.referenceExprSymbols(scope, unary.operand);
            },
            .dot_member => |*dot_member| {
                self.referenceExprSymbols(scope, dot_member.container);
                if (scope.findSymbol(dot_member.member_name.text)) |symbol| {
                    dot_member.symbol = symbol;
                }
                else {
                    self.sourceError(dot_member.member_name.text, "use of undeclared symbol \"{s}\"", .{dot_member.member_name.text});
                }
            },
            .eval_params => |eval_params| {
                self.referenceExprSymbols(scope, eval_params.subject);
                for (eval_params.params) |*param| {
                    self.referenceExprSymbols(scope, param);
                }
            },
            .module_def => |*module_def| {
                const module = &module_def.module;
                self.referenceModuleSymbols(module);
            },
            else => {}
        }
    }

    const EvalParams = struct {
        values: []?Value,
        parent: ?*EvalParams,
        scope: *Scope,

        fn paramValue(self: EvalParams, param: *ParamDef) ?Value {
            if (param.scope == self.scope) {
                return self.values[param.index];
            }
            else {
                if (self.parent) |parent| {
                    return parent.paramValue(param);
                }
                else {
                    return null;
                }
            }
        }
    };

    fn coerceValue(self: *Self, token: []const u8, val: Value, in: Type, out: Type) ?Value {
        if (in == out) {
            return val;
        }
        if (in == .module or in == .type_value or out == .module or out == .type_value) { 
            self.sourceError(token, "cannot coerce {s} to {s}", .{in.name(), out.name()});
            return null;
        }
        const in_s = in.scalarComponent().?;
        const out_s = out.scalarComponent().?;
        if ((in_s == .float or in_s == .number) and out_s != .float) {
            self.sourceError(token, "cannot coerce {s} to {s} without a cast", .{in.name(), out.name()});
        }
        const in_size = @enumToInt(in.vectorSize().?);
        const out_size = @enumToInt(out.vectorSize().?);
        if (in_size == 1 and out_size == 1) {
            const result_scalar = self.castScalar(.coerce, token, val, 0, in, out) orelse return null;
            return Value {
                .num = .{ result_scalar, 0, 0, 0},
            };
        }
        else {
            return self.castVector(.coerce, token, val, in, out);
        }
    }

    fn castVector(self: *Self, mode: CastMode, token: []const u8, val: Value, in: Type, out: Type) ?Value {
        const in_size = @enumToInt(in.vectorSize().?);
        const out_size = @enumToInt(out.vectorSize().?);
        if (in_size != out_size and in_size != 1) {
            self.sourceError(token, "cannot {s} {s} to vector {s} of different element count",
                .{@tagName(mode), in.name(), out.name()},
            );
        }
        if (in_size == 1) {
            const v = self.castScalar(mode, token, val, 0, in, out) orelse return null;
            var num: [4]u64 = undefined;
            var i: usize = 0;
            while (i < out_size) : (i += 1) {
                num[i] = v;
            }
            return Value{ .num = num };
        }
        else {
            var num: [4]u64 = undefined;
            var i: usize = 0;
            while (i < out_size) : (i += 1) {
                num[i] = self.castScalar(mode, token, val, i, in, out) orelse return null;
            }
            return Value{ .num = num };
        }
    }

    fn castScalar(self: *Self, mode: CastMode, token: []const u8, value: Value, index: usize, in: Type, out: Type) ?u64 {
        const in_index = in.scalarComponent().?.indexInAll();
        const out_index = out.scalarComponent().?.indexInAll();
        return scalarCastTable[in_index][out_index](self, mode, token, value, index);
    }

    const ScalarCastFn = fn(*Self, CastMode, []const u8, Value, usize) ?u64;
    
    const CastMode = enum {
        cast,
        coerce,
    };

    const ScalarCastTable = [Scalar.all.len][Scalar.all.len]ScalarCastFn;

    const scalarCastTable: ScalarCastTable = blk: {
        var table: ScalarCastTable = undefined;
        for (Scalar.all) |in, i| {
            for (Scalar.all) |out, o| {
                table[i][o] = scalarCastFn(in, out);
            }
        }
        break :blk table;
    };

    fn scalarCastFn(comptime in: Type, comptime out: Type) ScalarCastFn {
        return struct {

            self: *Self,
            mode: CastMode,
            token: []const u8,
            val: Value,
            in_val: I,

            const Cast = @This();

            const E = error {
                NegativeToUnsign,
                TooNarrow,
            };


            const I = in.scalarComonent().?.Val();
            const O = out.scalarComonent().?.Val();

            const in_name = in.name();
            const out_name = out.name();

            const hsize = @sizeOf(I);
            const wsize = @sizeOf(O);


            fn castError(c: Cast, err: E) void {
                const self = c.self;
                switch (err) {
                    .NegativeToUnsign => {
                        if (in.vectorSize().? == 1) {
                            self.sourceError(
                                c.token, 
                                "cannot {s} negative " ++ in_name ++ " {d} to unsigned type " ++ out_name,
                                .{@tagName(c.mode), c.val.formatted(in)}
                            );
                        }
                        else {
                            self.sourceError(
                                c.token, 
                                "cannot {s} negative " ++ in_name ++ "component {d} to unsigned type " ++ out_name,
                                .{@tagName(c.mode), c.in_val}
                            );
                        }
                    },
                    .ToSmall => {
                        if (in.vectorSize().? == 1) {
                            self.sourceError(
                                c.token, 
                                "cannot {s} " ++ in_name ++ " (d): target type " ++ out_name ++ " is too narrow to represent it",
                                .{@tagName(c.mode), c.val.formatted(in)}
                            );
                        }
                        else {
                            self.sourceError(
                                c.token, 
                                "cannot {s} " ++ in_name ++ " component (d): target type " ++ out_name ++ " is too narrow to represent it",
                                .{@tagName(c.mode), c.in_val}
                            );
                        }
                    }
                }
            }

            fn cast(c: *Cast) E!O {
                const m = std.math;
                return switch (in) {
                    .num, .float => switch (out) {
                        .num, .float => blk: {
                            break :blk @floatCast(O, c.in_val);
                        },
                        .sign => blk: {
                            break :blk @floatToInt(O, m.floor(c.in_val));
                        },
                        .unsign => blk: {
                            if (c.in_val < 0) {
                                return Error.NegativeToUnsign;
                            }
                            break :blk @floatToInt(O, m.floor(c.in_val));
                        },
                    },
                    .sign => switch (out) {
                        .num, .float => blk: {
                            break :blk @intToFloat(O, c.in_val);
                        },
                        .sign => blk: {
                            if (hsize > wsize) {
                                if (c.in_val > m.maxInt(O)) {
                                    return Error.TooNarrow;
                                }
                                if (c.in_val < m.minInt(O)) {
                                    return Error.TooNarrow;
                                }
                            }
                            break :blk @intCast(O, c.in_val);
                        },
                        .unsign => blk: {
                            if (c.in_val < 0) {
                                return Error.NegativeToUnsign;
                            }
                            if (hsize > wsize) {
                                if (c.in_val > m.maxInt(O)) {
                                    return Error.TooNarrow;
                                }
                            }
                            break :blk @intCast(O, c.in_val);
                        },
                    },
                    .unsign => switch (out) {
                        .num, .float => blk: {
                            break :blk @intToFloat(O, c.in_val);
                        },
                        .sign => blk: {
                            if (hsize > wsize) {
                                if (c.in_val > m.maxInt(O)) {
                                    return Error.TooNarrow;
                                }
                            }
                            break :blk @intCast(O, c.in_val);
                        },
                        .unsign => blk: {
                            if (hsize > wsize) {
                                if (c.in_val > m.maxInt(O)) {
                                    return Error.TooNarrow;
                                }
                            }
                            break :blk @intCast(O, c.in_val);
                        },
                    }
                };
            }


            fn closure(self: *Self, mode: CastMode, token: []const u8, val: Value, index: usize) ?u64 {
                var c = Cast {
                    .self = self,
                    .mode = mode,
                    .token = token,
                    .val = val,
                    .in_val = val.getNumAs(I, index),
                };
                const out_val = c.cast() catch |err| {
                    c.castError(err);
                    return null;
                };
                return Value.toBits(u64, out_val);
            }

        }.closure;
    }

    // fn evalConstExpr(self: *Self, params: EvalParams, expr: *Expr) ?Value {
    //     const value = switch (expr.val) {
    //         .name => |name| {
    //             switch (name.symbol.?) {
    //                 .decl => |decl| return self.evalConstExpr(params, &decl.expr),
    //                 .param => |param| {
    //                     return params.paramValue(param);
    //                 },
    //                 .builtin_type => |builtin_type| {
    //                     return Value{ .@"type" = builtin_type };
    //                 },
    //             }
    //         },
    //         .binary => |binary| {
    //             const lhs = binary.lhs;
    //             const rhs = binary.rhs;
    //             const lhs_v = self.evalConstExpr(params, lhs);
    //             const rhs_v = self.evalConstExpr(params, rhs);
    //             if (lhs.val_type != null and rhs.val_type != null) {
    //                 if ()
    //             }
    //         },
    //     };
    //     if (value != null) {
    //         expr.val_type = valueType(value);
    //     }
    //     return value;
    // }

};
