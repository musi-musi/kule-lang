const std = @import("std");
const language = @import("../language.zig");
const compiler = @import("../compiler.zig");
const diagnostics = @import("../diagnostics.zig");


const CompilationUnit = compiler.CompilationUnit;


const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;


const KType = language.KType;
const Value = language.Value;
const Constant = language.Constant;


const Token = language.Token;
const Stx = language.Syntax;
const Sem = language.Semantics;

const SemData = Sem.Data;
const Meta = Sem.Meta;

const Symbol = Sem.Symbol;
const Scope = Sem.Scope;

const NodeKType = Sem.NodeKType;
const NodeValue = Sem.NodeValue;
const NodeDep = Sem.NodeDep;


const Expr = Stx.Expr;

const AllocError = Allocator.Error;

pub fn analyzeUnit(unit: *CompilationUnit) Sema.Error!void {
    const sem = unit.initSemantics();
    try sem.module.init(null, sem, unit.syntax.?.root_module.statements);
    const analyzer = (Sema {
        .unit = unit,
        .sem = sem,
        .stx = &(unit.syntax.?),
        .scope = undefined,
        .data = &sem.data,
    }).scoped(&sem.module.scope);
    try analyzer.collectModuleSymbols(&sem.module);
    try analyzer.analyzeModule(&sem.module);
}

const Sema = struct {
    unit: *CompilationUnit,
    sem: *Sem,
    stx: *Stx,
    
    // current context
    scope: *Scope,
    data: *SemData,
    expected_type: Meta = .{},



    const Error = error {
        Unresolved,
        TypeMismatch,
    } || AllocError || CastError || CoerceError || OpError;


    fn scoped(sema: Sema, scope_container: anytype) Sema {
        const T = @TypeOf(scope_container);
        const scope = switch (T) {
            *Scope => scope_container,
            *Sem.Decl => &scope_container.body_scope,
            *Sem.Binding => &scope_container.decl.body_scope,
            *Sem.WhereClause => &scope_container.decl.body_scope,
            *Sem.Module => &scope_container.scope,
            else => @compileError(@typeName(T) ++ " does not contain a scope"),
        };
        var inner = sema;
        inner.scope = scope;
        return inner;
    }

    fn withData(sema: Sema, data: *SemData) Sema {
        var inner = sema;
        inner.data = data;
        return inner;
    }

    fn expectingType(sema: Sema, expected_type: Meta) Sema {
        var inner = sema;
        inner.expected_type = expected_type;
        return inner;

    }

    fn initMeta(sema: Sema, node_ptr: anytype) AllocError!void {
        try sema.data.initMeta(sema.allocator(), node_ptr);
    }

    fn logError(sema: Sema, token: anytype, comptime format: []const u8, args: anytype) void {
        sema.unit.diagnostics.logError(Sem.nameOf(token), format, args);
    }

    fn allocator(sema: Sema) Allocator {
        return sema.unit.arena.allocator();
    }

    fn meta(sema: Sema, node_ptr: anytype) *Meta {
        return sema.data.ptr(node_ptr);
    }



    // ----------
    // collection
    // ----------


    fn collectModuleSymbols(sema: Sema, module: *Sem.Module) AllocError!void {
        for (module.statements) |*statement| {
            switch (statement.body) {
                .binding => {
                    _ = try sema.tryAddSymbol(&statement.body.binding);
                },
            }
        }
        for (module.statements) |*statement| {
            switch (statement.body) {
                .binding => {
                    try sema.collectBindingSymbols(&statement.body.binding);
                },
            }
        }
    }

    fn collectBindingSymbols(sema: Sema, binding: *Sem.Binding) AllocError!void {
        const body_scope = &binding.decl.body_scope;
        for (binding.where_clauses) |*where_clause| {
            _ = try sema.scoped(body_scope).tryAddSymbol(where_clause);
        }
        for (binding.where_clauses) |*where_clause| {
            try sema.collectDeclSymbols(&where_clause.decl);
        }
        try sema.collectDeclSymbols(&binding.decl);
    }

    fn collectDeclSymbols(sema: Sema, decl: *Sem.Decl) AllocError!void {
        const body_scope = &decl.body_scope;
        try sema.initMeta(decl);
        if (decl.type_expr) |type_expr| {
            try sema.collectExprSymbols(type_expr);
        }
        switch (decl.body) {
            .function => {
                const function = &decl.body.function;
                for (function.params) |*param| {
                    _ = try body_scope.tryAddSymbol(param);
                    try sema.collectExprSymbols(param.type_expr);
                }
                try sema.scoped(body_scope).collectExprSymbols(function.body);
            },
            .direct => |expr| {
                try sema.scoped(body_scope).collectExprSymbols(expr);
            }
        }
    }

    fn collectExprSymbols(sema: Sema, expr: anytype) AllocError!void {
        const Node = @TypeOf(expr);
        try sema.initMeta(expr);
        switch (Node) {
            *Expr => try sema.collectExprSymbols(&expr.expr),
            *Expr.Add, *Expr.Mul => {
                try sema.collectExprSymbols(&expr.operand);
                if (expr.terms) |terms| {
                    for (terms) |*term| {
                        try sema.initMeta(term);
                        try sema.collectExprSymbols(&term.operand);
                    }
                }
            },
            *Expr.Neg => try sema.collectExprSymbols(&expr.operand),
            *Expr.Eval => {
                try sema.collectExprSymbols(&expr.function);
                if (expr.params) |params| {
                    for (params.params) |*param| {
                        try sema.collectExprSymbols(param);
                    }
                }
            },
            *Expr.Access => try sema.collectExprSymbols(&expr.container),
            *Expr.Atom => switch (expr.*) {
                .module => |module_def| {
                    const module = try sema.allocator().create(Sem.Module);
                    try module.init(sema.scope, sema.sem, module_def.statements);
                    try sema.scoped(module).collectModuleSymbols(module);
                    const mod = sema.meta(expr);
                    mod.setTypeFromValue(Meta.initType(.module));
                    mod.value_data = Value.Data { .module = module };
                    switch (sema.scope.container) {
                        .binding => |binding| mod.value_symbol = Symbol.init(binding),
                        .where_clause => |where_clause| mod.value_symbol = Symbol.init(where_clause),
                        else => {},
                    }
                },
                .parens => |parens| try sema.collectExprSymbols(parens.expr),
                else => {},
            },
            else => @compileError(@typeName(Node) ++ " is not an expression node"),
        }

    }

    // --------
    // analysis
    // --------

    fn analyzeModule(sema: Sema, module: *Sem.Module) Error!void {
        for (module.statements) |*statement| {
            switch (statement.body) {
                .binding => {
                    _ = try sema.scoped(&module.scope).analyzeBinding(&statement.body.binding);
                }
            }
        }
    }

    fn analyzeBinding(sema: Sema, binding: *Sem.Binding) Error!Meta {
        const self = sema.meta(binding);
        if (self.state != .complete) {
            self.state = .complete;
            for (binding.where_clauses) |*where_clause| {
                _ = try sema.analyzeWhereClause(where_clause);
            }
            self.copyTypeAndValue(try sema.analyzeDecl(&binding.decl));
            self.value_symbol = Symbol.init(binding);
        }
        return self.*;
    }

    fn analyzeWhereClause(sema: Sema, where_clause: *Sem.WhereClause) Error!Meta {
        const self = sema.meta(where_clause);
        if (self.state != .complete) {
            self.state = .complete;
            self.copyTypeAndValue(try sema.analyzeDecl(&where_clause.decl));
            self.value_symbol = Symbol.init(where_clause);
        }
        return self.*;
    }

    fn analyzeDecl(sema: Sema, decl: *Sem.Decl) Error!Meta {
        const sema_body = sema.scoped(decl);
        const self = sema.meta(decl);
        if (self.state != .complete) {
            self.state = .complete;
            const type_expr: Meta = (
                if (decl.type_expr) |type_expr| 
                    try sema.analyzeTypeExpr(type_expr)
                else Meta{}
            );
            switch (decl.body) {
                .function =>  {
                    const function = &decl.body.function;
                    for (function.params) |*fn_param| {
                        const param = sema.meta(fn_param);
                        const param_type_expr = try sema.analyzeTypeExpr(fn_param.type_expr);
                        param.setTypeFromValue(param_type_expr);
                    }
                    _ = try sema_body.analyzeDeclBody(type_expr, function.body);
                    const fn_type = Meta.initType(.function);
                    self.setTypeFromValue(fn_type);
                    self.value_data = Value.Data { .function = function };
                },
                .direct => |direct| {
                    const body = try sema_body.analyzeDeclBody(type_expr, direct);
                    self.copyTypeAndValue(body);
                },
            }
        }
        return self.*;
    }

    fn analyzeDeclBody(sema: Sema, type_expr: Meta, body_expr: *Expr) Error!Meta {
        const equal = @fieldParentPtr(Stx.Decl, "expr", body_expr).equal;
        var expr = try sema.expectingType(type_expr).analyzeExpr(body_expr);
        try sema.cast(.coerce, equal, &expr, type_expr);
        return expr;
    }

    fn analyzeTypeExpr(sema: Sema, type_expr: *Expr) Error!Meta {
        const colon = @fieldParentPtr(Stx.TypeExpr, "expr", type_expr).colon;        
        const expr = try sema.analyzeExpr(type_expr);
        if (expr.ktype) |expr_type| {
            if (expr_type != .ktype) {
                sema.logError(colon, "expected type, found {}", .{expr_type});
                return Error.Unresolved;
            }
        }
        return expr;
    }


    fn analyzeExpr(sema: Sema, expr: anytype) Error!Meta {
        const Node = @TypeOf(expr);
        const self = sema.meta(expr);
        if (self.state != .complete) {
            switch (Node) {
                *Expr => { 
                    self.copyTypeAndValue(try sema.analyzeExpr(&expr.expr));
                },
                *Expr.Add, *Expr.Mul => {
                    var lhs = try sema.analyzeExpr(&expr.operand);
                    if (expr.terms) |terms| {
                        for (terms) |*term_expr| {
                            const term = sema.meta(term_expr);
                            const rhs = try sema.analyzeExpr(&term_expr.operand);
                            try sema.binaryOperation(term_expr.op, lhs, rhs, term);
                            lhs = term.*;
                        }
                    }
                    self.copyTypeAndValue(lhs);
                },
                *Expr.Neg => {
                    self.copyTypeAndValue(try sema.analyzeExpr(&expr.operand));
                    if (expr.op) |op| {
                        if (op.tag == .minus) {
                            if (self.value()) |value| {
                                const ktype = value.ktype;
                                if (ktype.isNumeric() and ktype.scalar == .unsigned) {
                                    // if the operand is unsigned but has a known value at compile time,
                                    // it can coerce to signed before negation
                                    try sema.cast(.coerce, op, self, Meta.initType(.signed));
                                }
                            }
                            const operand = self.*;
                            try sema.unaryOperation(op, operand, self);
                            // if we are negating a number literal directly, set its type and value
                            // this is literally just so that it shows up as signed and negative on diagnostics lol
                            if (expr.operand.function.container == .number) {
                                sema.meta(&expr.operand).copyTypeAndValue(self.*);
                                sema.meta(&expr.operand.function).copyTypeAndValue(self.*);
                                sema.meta(&expr.operand.function.container).copyTypeAndValue(self.*);
                            }
                        }
                    }
                },
                *Expr.Eval => {
                    self.copyTypeAndValue(try sema.analyzeExpr(&expr.function));
                    // if (expr.params) |params| {
                    //     for (params.params) |*param| {
                    //         try self.analyzeExpr(param);
                    //     }
                    // }
                },
                *Expr.Access => blk: {
                    const con = try sema.analyzeExpr(&expr.container);
                    if (expr.member) |member_expr| {
                        if (con.ktype) |con_type| {
                            if (con_type != .module) {
                                sema.logError(
                                    member_expr.dot,
                                    "{} does not support member access",
                                    .{con_type},
                                );
                                return Error.Unresolved;
                            }
                            if (con.value()) |value| {
                                const module = value.data.module;
                                if (module.scope.findSymbol(member_expr.name)) |member_symbol| {
                                    const member = try sema.analyzeSymbol(member_symbol);
                                    self.copyTypeAndValue(member);
                                    break :blk;
                                }
                                else {
                                    sema.logError(
                                        member_expr.name,
                                        "module {} missing member '{s}'",
                                        .{module, member_expr.name.text},
                                    );
                                    return Error.Unresolved;
                                }
                            }
                        }
                    }
                    self.copyTypeAndValue(con);
                },
                *Expr.Atom => switch (expr.*) {
                    .number => |number| {
                        const value: Value = blk: {
                            if (std.fmt.parseUnsigned(KType.TypeOf(.unsigned), number.text, 10)) |unsigned| {
                                break :blk Value.initConstScalar(.unsigned, unsigned);
                            }
                            else |_| if (std.fmt.parseFloat(KType.TypeOf(.float), number.text)) |float| {
                                break :blk Value.initConstScalar(.float, float);
                            }
                            else |_| unreachable;
                        };
                        const type_constant = language.lookupConstant(@tagName(value.ktype.scalar)).?;
                        self.value_data = value.data;
                        self.ktype = value.ktype;
                        self.ktype_symbol = Symbol.init(type_constant);
                    },
                    .name => |name| {
                        if (sema.scope.findSymbol(name)) |symbol| {
                            self.copyTypeAndValue(try sema.analyzeSymbol(symbol));
                        }
                        else {
                            sema.logError(name, "use of undeclared symbol '{s}'", .{name.text});
                            return Error.Unresolved;
                        }
                    },
                    .module => {
                        if (self.value_data) |data| {
                            const module = data.module;
                            try sema.analyzeModule(module);
                        }
                    },
                    .parens => |parens| {
                        self.copyTypeAndValue(try sema.analyzeExpr(parens.expr));
                    },
                    .import => {
                        const type_constant = language.lookupConstant(@tagName(KType.Tag.module)).?;
                        self.ktype = type_constant.value.data.ktype;
                        self.ktype_symbol = Symbol.init(type_constant);
                    },
                },
                else => @compileError(@typeName(Node) ++ " is not an expression node"),
            }
        }
        self.state = .complete;
        return self.*;
    }


    fn analyzeSymbol(sema: Sema, symbol: Symbol) Error!Meta {
        switch (symbol) {
            .binding => |binding| {
                return try sema.analyzeBinding(binding);
            },
            .where_clause => |where_clause| {
                return try sema.analyzeWhereClause(where_clause);
            },
            .function_param => |function_param| {
                return sema.data.get(function_param);
            },
            .constant => |constant| return Meta {
                .ktype = constant.ktype,
                .value_data = constant.value.data,
                .value_symbol = symbol,
            },
        }
    }


    fn tryAddSymbol(sema: Sema, symbol_item: anytype) AllocError!?Symbol {
        const symbol = Symbol.init(symbol_item);
        try sema.initMeta(symbol_item);
        if (try sema.scope.tryAddSymbol(symbol)) |existing| {
            _ = sema.unit.diagnostics.startError(
                symbol.name(),
                "{k} shadows existing {kn}",
                .{symbol, existing},
            ).addRelated(&.{existing.name()}, "declared here", .{});
            return existing;
        }
        else {
            const symbol_meta = sema.meta(symbol_item);
            symbol_meta.value_symbol = symbol;
            return null;
        }
    }

    const CoerceError = language.CoerceError;
    const CastError = language.CastError;

    const CastKTypeError = language.CastKTypeError;
    const CoerceKTypeError = language.CoerceKTypeError;
    const CastValueError = language.CastValueError;
    const CoerceValueError = language.CoerceValueError;


    const OpError = BinaryOpError || UnaryOpError;

    const ResultKTypeError = language.ResultKTypeError;

    const BinaryOpError = language.BinaryOpError;
    const BinaryOpKTypeError = language.BinaryOpKTypeError;
    const BinaryOpValueError = language.BinaryOpValueError;
    const UnaryOpError = language.UnaryOpError;
    const UnaryOpKTypeError = language.UnaryOpKTypeError;
    const UnaryOpValueError = language.UnaryOpValueError;


    const CastMode = enum {
        cast, coerce,

        fn ErrorSet(comptime mode: CastMode) type {
            return switch (mode) {
                .cast => CastError,
                .coerce => CoerceError,
            };
        }
    };

    fn logCastError(self: Sema, comptime mode: CastMode, token: anytype, err: anytype, a_ktype: KType, a_value: ?Value, b_ktype: KType) @TypeOf(err)!void {
        const ErrSet = switch (mode) {
            .cast => CastError,
            .coerce => CoerceError,
        };
        switch(@errSetCast(ErrSet, err)) {
            ErrSet.Incompatable => self.logError(token, 
                "cannot " ++ @tagName(mode) ++ " {} to {}",
                .{a_ktype, b_ktype}),
            ErrSet.DimensionMismatch => self.logError(token, 
                "cannot " ++ @tagName(mode) ++ " {} to {} with different dimensions",
                .{a_ktype, b_ktype}),
            ErrSet.IntegerOverflow => self.logError(token, 
                "failed to " ++ @tagName(mode) ++ " {} {} to {} without overflow",
                .{a_ktype, a_value.?, b_ktype}),
            ErrSet.NegativeToUnsigned => self.logError(token, 
                "failed to " ++ @tagName(mode) ++ " negative {} {} to {}",
                .{a_ktype, a_value.?, b_ktype}),
            ErrSet.NanToInteger => self.logError(token, 
                "failed to " ++ @tagName(mode) ++ " {} {} to {}",
                .{a_ktype, a_value.?, b_ktype}),
            ErrSet.InfinityToInteger => self.logError(token, 
                "failed to " ++ @tagName(mode) ++ " {} {} to {}",
                .{a_ktype, a_value.?, b_ktype}),
            else => switch (mode) {
                .cast => return,
                .coerce => switch (@errSetCast(ErrSet, err)) {
                    ErrSet.ScalarModeMismatch => self.logError(token, 
                        "cannot coerce {} to {} without a cast",
                        .{a_ktype, b_ktype}),
                    ErrSet.ScalarInformationLoss => self.logError(token, 
                        "cannot coerce {} to {} without a cast",
                        .{a_ktype, b_ktype}),
                    else => return,
                }
            }
        }
        return err;
    }

    fn logBinaryOpError(self: Sema, err: anytype, op: Token, a_ktype: KType, b_ktype: KType, a_value: ?Value, b_value: ?Value) Error!void {
        switch (@errSetCast(BinaryOpError, err)) {
            BinaryOpError.DivideByZero => self.logError(op,
                "divide by zero {} {s} {}",
                .{a_value.?, op.text, b_value.?}),
            BinaryOpError.UnsupportedOperation => self.logError(op,
                "undefined operation {} {s} {}",
                .{a_ktype, op.text, b_ktype}),
            else => switch (op.tag) {
                .plus, .minus, .aster, .fslash => {
                    if (language.binaryResultKType(op.tag, a_ktype, b_ktype)) |result_ktype| {
                        if (result_ktype.eql(a_ktype)) return self.logCastError(.coerce, op, err, a_ktype, a_value, b_ktype);
                        if (result_ktype.eql(b_ktype)) return self.logCastError(.coerce, op, err, b_ktype, b_value, a_ktype);
                    }
                    else |_| {
                        try self.checkCastType(.coerce, op, a_ktype, b_ktype);
                        try self.checkCastType(.coerce, op, b_ktype, a_ktype);
                    }
                },
                else => return,
            },
        }
        return err;
    }

    fn logUnaryOpError(self: Sema, err: anytype, op: Token, a_ktype: KType, a_value: ?Value) Error!void {
        switch (@errSetCast(UnaryOpError, err)) {
            UnaryOpError.NegateUnsigned => self.logError(op,
                "cannot negate {}",
                .{a_ktype}),
            UnaryOpError.UnsupportedOperation => self.logError(op,
                "undefined operation {s} {}",
                .{op.text, a_ktype}),
            else => switch (op.tag) {
                .plus, .minus => {
                    if (language.unaryResultKType(op.tag, a_ktype)) |result_ktype| {
                        try self.logCastError(.coerce, op, err, a_ktype, a_value.?, result_ktype);
                    }
                    else |_| {
                        return;
                    }
                },
                else => return,
            },
        }
        return err;
    }

    /// if target has a known value, it MUST be a KType. this is not checked
    fn cast(sema: Sema, comptime mode: CastMode, token: anytype, self: *Meta, target: Meta) Error!void {
        if (self.ktype) |self_type| {
            if (target.value_data) |target_data| {
                const target_type = target_data.ktype;
                try sema.checkCastType(mode, token, self_type, target_type);
                self.ktype_symbol = target.value_symbol;
                if (self.value())  |self_value| {
                    const result = try sema.castValueAssumeSafe(mode, token, self_value, target_type);
                    self.ktype = target_type;
                    self.value_data = result.data;
                }
            }
        }
    }


    fn checkCastType(sema: Sema, comptime mode: CastMode, token: anytype, a: KType, b: KType) Error!void {
        errdefer |err| sema.logCastError(mode, token, err, a, null, b) catch {};
        switch (mode) {
            .cast => try language.checkCastKType(a, b),
            .coerce => try language.checkCoerceKType(a, b),
        }
    }

    fn castValue(sema: Sema, comptime mode: CastMode, token: anytype, a: Value, b: KType) Error!Value {
        if (a.ktype.eql(b)) return a;
        try sema.checkCastType(mode, token, a.ktype, b);
        return try sema.castValueAssumeSafe(mode, token, a, b);
    }

    fn castValueAssumeSafe(sema: Sema, comptime mode: CastMode, token: anytype, a: Value, b: KType) Error!Value {
        errdefer |err| sema.logCastError(mode, token, err, a.ktype, a, b) catch {};
        return try language.castValueAssumeSafe(a, b);
    }

    fn binaryOperation(sema: Sema, op: Token, lhs: Meta, rhs: Meta, res: *Meta) Error!void {
        if (lhs.ktype) |l_type| {
            if (rhs.ktype) |r_type| {
                const res_type = try sema.binaryResultType(op, l_type, r_type);
                res.ktype = res_type;
                if (res_type.eql(l_type)) res.ktype_symbol = lhs.ktype_symbol;
                if (res_type.eql(r_type)) res.ktype_symbol = rhs.ktype_symbol;
                if (lhs.value()) |l_val| {
                    if (rhs.value()) |r_val| {
                        const l_coerced = try sema.castValue(.coerce, op, l_val, res_type);
                        const r_coerced = try sema.castValue(.coerce, op, r_val, res_type);
                        errdefer |err| sema.logBinaryOpError(err, op, l_type, r_type, l_val, r_val) catch {};
                        const res_val = try language.doBinaryOpAssumeSafe(op.tag, res_type, l_coerced, r_coerced);
                        res.value_data = res_val.data;
                    }
                }
            }
        }
    }

    fn unaryOperation(sema: Sema, op: Token, operand: Meta, res: *Meta) Error!void {
        if (operand.ktype) |operand_type| {
            const res_type = try sema.unaryResultType(op, operand_type);
            res.ktype = res_type;
            if (operand.value()) |operand_val| {
                const operand_coerced = try sema.castValue(.coerce, op, operand_val, res_type);
                errdefer |err| sema.logUnaryOpError(err, op, operand_type, operand_val) catch {};
                const res_val = try language.doUnaryOpAssumeSafe(op.tag, res_type, operand_coerced);
                res.value_data = res_val.data;
            }
        }
    }

    fn binaryResultType(sema: Sema, op: Token, a: KType, b: KType) BinaryOpKTypeError!KType {
        errdefer |err| sema.logBinaryOpError(err, op, a, b, null, null) catch {};
        return try language.binaryResultKType(op.tag, a, b);
    }

    fn unaryResultType(self: Sema, op: Token, a: KType) UnaryOpKTypeError!KType {
        errdefer |err| self.logUnaryOpError(err, op, a, null) catch {};
        return try language.unaryResultKType(op.tag, a);
    }

    

};