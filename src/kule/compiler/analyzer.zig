const std = @import("std");
const language = @import("../language.zig");
const compiler = @import("../compiler.zig");
const diagnostics = @import("../diagnostics.zig");


const CompilationUnit = compiler.CompilationUnit;


const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;


const Taip = language.Taip;
const Value = language.Value;
const Constant = language.Constant;


const Token = language.Token;
const Stx = language.Syntax;
const Sem = language.Semantics;

const Symbol = Sem.Symbol;
const Scope = Sem.Scope;

const DepInfo = Sem.DepInfo;


const Expr = Stx.Expr;

const AllocError = Allocator.Error;

pub fn analyzeUnit(unit: *CompilationUnit) Analyzer.Error!void {
    try analyzeUnitExt(unit, false);
}

pub fn analyzeUnitExt(unit: *CompilationUnit, populate_metadata: bool) Analyzer.Error!void {
    const analyzer = Analyzer.init(unit, populate_metadata);
    try analyzer.analyzeRootModule();
}

const Analyzer = struct {
    unit: *CompilationUnit,
    sem: *Sem,
    stx: *Stx,
    populate_metadata: bool = false,


    const Error = error {
        Unresolved,
        TypeMismatch,
    } || AllocError || CastError || CoerceError || OpError;

    const Self = @This();

    fn init(unit: *CompilationUnit, populate_metadata: bool) Self {
        return Self {
            .unit = unit,
            .sem = unit.initSemantics(),
            .stx = &(unit.syntax.?),
            .populate_metadata = populate_metadata,
        };
    }

    fn logError(self: Self, token: anytype, comptime format: []const u8, args: anytype) void {
        self.unit.diagnostics.logError(Sem.nameOf(token), format, args);
    }

    fn allocator(self: Self) Allocator {
        return self.unit.arena.allocator();
    }

    fn analyzeRootModule(self: Self) Error!void {
        try self.collectModuleSymbols(null, &self.sem.module, self.stx.root_module.statements);
        try self.analyzeModule(&self.sem.module);
    }

    fn setTaip(self: Self, node: anytype, taip: Taip) AllocError!void {
        if (self.populate_metadata) {
            if (try self.meta(node)) |m| {
                m.taip = taip;
            }
        }
        try self.sem.setTaip(node, taip);
    }

    fn setValue(self: Self, node: anytype, value: Value) AllocError!void {
        if (self.populate_metadata) {
            if (try self.meta(node)) |m| {
                m.value = value;
            }
        }
        try self.sem.setValue(node, value);
    }

    fn setDepInfo(self: Self, node: anytype, dep_info: DepInfo) AllocError!void {
        try self.sem.setDepInfo(node, dep_info);
    }

    fn taipOf(self: Self, node: anytype) ?Taip
        { return self.sem.taipOf(node); }
    fn valueOf(self: Self, node: anytype) ?Value
        { return self.sem.valueOf(node); }
    fn depInfoOf(self: Self, node: anytype) ?DepInfo
        { return self.sem.depInfoOf(node); }

    fn collectModuleSymbols(self: Self, parent: ?*Scope, module: *Sem.Module, statement_nodes: ?Stx.Statements) AllocError!void {
        try module.init(parent, self.sem, statement_nodes);
        const scope = &module.scope;
        for (module.statements) |*statement| {
            switch (statement.body) {
                .binding => {
                    _ = try self.tryAddSymbol(scope, &statement.body.binding);
                },
            }
        }
        for (module.statements) |*statement| {
            switch (statement.body) {
                .binding => {
                    try self.collectBindingSymbols(&statement.body.binding);
                },
            }
        }
    }

    fn collectBindingSymbols(self: Self, binding: *Sem.Binding) AllocError!void {
        const scope = &binding.decl.body_scope;
        for (binding.where_clauses) |*where_clause| {
            _ = try self.tryAddSymbol(scope, where_clause);
        }
        for (binding.where_clauses) |*where_clause| {
            try self.collectDeclSymbols(&where_clause.decl);
        }
        try self.collectDeclSymbols(&binding.decl);
    }

    fn collectDeclSymbols(self: Self, decl: *Sem.Decl) AllocError!void {
        const scope = &decl.body_scope;
        switch (decl.body) {
            .function => |function| {
                for (function.params) |*param| {
                    _ = try self.tryAddSymbol(scope, param);
                    try self.collectExprSymbols(scope.parent.?, param.taip_expr);
                }
                try self.collectExprSymbols(scope, function.body);
            },
            .direct => |expr| {
                try self.collectExprSymbols(scope, expr);
            }
        }
    }

    fn collectExprSymbols(self: Self, parent_scope: *Scope, expr: anytype) AllocError!void {
        const Node = @TypeOf(expr);
        switch (Node) {
            *Expr => try self.collectExprSymbols(parent_scope, &expr.expr),
            *Expr.Add, *Expr.Mul => {
                try self.collectExprSymbols(parent_scope, &expr.operand);
                if (expr.terms) |terms| {
                    for (terms) |*term| {
                        try self.collectExprSymbols(parent_scope, &term.operand);

                    }
                }
            },
            *Expr.Neg => try self.collectExprSymbols(parent_scope, &expr.operand),
            *Expr.Eval => {
                try self.collectExprSymbols(parent_scope, &expr.function);
                if (expr.params) |params| {
                    for (params.params) |*param| {
                        try self.collectExprSymbols(parent_scope, param);
                    }
                }
            },
            *Expr.Access => try self.collectExprSymbols(parent_scope, &expr.container),
            *Expr.Atom => switch (expr.*) {
                .module => |module_def| {
                    const module = try self.allocator().create(Sem.Module);
                    try self.setValue(expr, Value.init(.module, module));
                    try self.collectModuleSymbols(parent_scope, module, module_def.statements);
                },
                .parens => |parens| try self.collectExprSymbols(parent_scope, parens.expr),
                else => {},
            },
            else => @compileError(@typeName(Node) ++ " is not an expression node"),
        }

    }

    fn analyzeModule(self: Self, module: *Sem.Module) Error!void {
        for (module.statements) |*statement| {
            switch (statement.body) {
                .binding => {
                    _ = try self.analyzeBinding(&statement.body.binding);
                }
            }
        }
    }

    fn analyzeBinding(self: Self, binding: *Sem.Binding) Error!Taip {
        const body_scope = binding.decl.body_scope;
        for (binding.where_clauses) |*where_clause| {
            _ = try self.analyzeWhereClause(where_clause);
        }
        const taip = try self.analyzeDecl(body_scope.parent.?, &binding.decl);
        try self.setTaip(binding, taip);
        if (self.valueOf(&binding.decl)) |value| {
            try self.setValue(binding, value);
        }
        return taip;
    }

    fn analyzeWhereClause(self: Self, where_clause: *Sem.WhereClause) Error!Taip {
        const taip = try self.analyzeDecl(where_clause.decl.body_scope.parent.?.parent.?, &where_clause.decl);
        try self.setTaip(where_clause, taip);
        if (self.valueOf(&where_clause.decl)) |value| {
            try self.setValue(where_clause, value);
        }
        return taip;
    }

    fn analyzeDecl(self: Self, taip_scope: *Scope, decl: *Sem.Decl) Error!Taip {
        const decl_taip: ?Taip = (
            if (decl.taip_expr) |taip_expr| 
                try self.analyzeTaipExpr(taip_scope, taip_expr)
            else null
        );
        switch (decl.body) {
            .function =>  {
                const function = &decl.body.function;
                for (function.params) |*param| {
                    const taip = try self.analyzeTaipExpr(taip_scope, param.taip_expr);
                    try self.setTaip(param, taip);
                    param.taip = taip;
                }
                const expr_taip = try self.analyzeDeclBody(decl_taip, &decl.body_scope, function.body);
                const return_taip = decl_taip orelse expr_taip;
                function.return_taip = return_taip;
                try self.setTaip(function, return_taip);
                const function_taip = (try Taip.taipOfFunction(self.allocator(), function)).?;
                try self.setTaip(decl, function_taip);
                try self.setValue(decl, Value.init(function_taip, function));
                return function_taip;
            },
            .direct => |direct| {
                const expr_taip = try self.analyzeDeclBody(decl_taip, &decl.body_scope, direct);
                const taip = decl_taip orelse expr_taip;
                try self.setTaip(decl, taip);
                if (self.valueOf(direct)) |value| {
                    try self.setValue(decl, value);
                }
                return taip;
            },
        }
    }

    fn analyzeDeclBody(self: Self, expected_taip: ?Taip, scope: *Scope, expr: *Expr) Error!Taip {
        const decl_node = @fieldParentPtr(Stx.Decl, "expr", expr);
        const equal = decl_node.equal;
        const expr_taip = try self.analyzeExpr(scope, expr);
        if (expected_taip) |expected| {
            try self.checkCoerceTaip(equal, expr_taip, expected);
            if (self.valueOf(expr)) |value| {
                const coerced = try self.coerceValue(equal, value, expected);
                try self.setValue(expr, coerced);
            }
        }
        return expr_taip;
    }

    fn analyzeTaipExpr(self: Self, scope: *Scope, expr: *Expr) Error!Taip {
        const taip_expr = @fieldParentPtr(Stx.TypeExpr, "expr", expr);
        const colon = taip_expr.colon;
        const expr_taip = try self.analyzeExpr(scope, expr);
        if (expr_taip != .taip) {
            self.unit.diagnostics.logError(colon.text, "expected type, found {}", .{expr_taip});
            return Error.Unresolved;
        }
        else {
            if (self.sem.valueOf(expr)) |value| {
                return value.data.taip;
            }
            else {
                self.unit.diagnostics.logError(colon.text, "could not resolve type expression", .{});
                return Error.Unresolved;
            }
        }
    }


    fn analyzeExpr(self: Self, scope: *Scope, expr: anytype) Error!Taip {
        const Node = @TypeOf(expr);
        if (self.taipOf(expr)) |taip| {
            return taip;
        }
        const expr_taip: Taip = et: {
            switch (Node) {
                *Expr => { 
                    const taip = try self.analyzeExpr(scope, &expr.expr);
                    if (self.valueOf(&expr.expr)) |value| {
                        try self.setValue(expr, value);
                    }
                    break :et taip;
                },
                *Expr.Add, *Expr.Mul => {
                    var taip = try self.analyzeExpr(scope, &expr.operand);
                    var value: ?Value = self.valueOf(&expr.operand);
                    if (expr.terms) |terms| {
                        for (terms) |*term| {
                            const rhs_taip = try self.analyzeExpr(scope, &term.operand);
                            // _ = rhs_taip;
                            taip = try self.binaryResultTaip(term.op, taip, rhs_taip);
                            try self.setTaip(term, taip);
                            if (value) |lhs_value| {
                                if (self.valueOf(&term.operand)) |rhs_value| {
                                    const result = try self.doBinaryOp(term.op, lhs_value, rhs_value);
                                    try self.setValue(term, result);
                                    value = result;
                                }
                            }
                        }
                    }
                    if (value) |v| {
                        try self.setValue(expr, v);
                    }
                    break :et taip;
                },
                *Expr.Neg => {
                    const operand_taip = try self.analyzeExpr(scope, &expr.operand);
                    if (expr.op) |op| {
                        if (self.valueOf(&expr.operand)) |operand_value| {
                            if (op.tag == .minus) {
                                if (!operand_taip.isNumeric()) {
                                    // this will always fail because the negate (.minus) operator 
                                    // is only for numeric types
                                    // return the correct error and log the right diagnostic
                                    _ = try self.unaryResultTaip(op, operand_taip);
                                }
                                if (operand_taip.numericScalar() == .unsigned) {
                                    const signed_value = try self.coerceValue(op, operand_value, Taip.init(.signed));
                                    const result_value = try self.doUnaryOp(op, signed_value);
                                    const result_taip = result_value.taip;
                                    // if we are negating a number literal directly, set its type and value
                                    // this is literally just so that it shows up as signed and negative on diagnostics lol
                                    if (expr.operand.function.container == .number) {
                                        try self.setTaip(&expr.operand, result_taip);
                                        try self.setValue(&expr.operand, result_value);
                                        try self.setTaip(&expr.operand.function, result_taip);
                                        try self.setValue(&expr.operand.function, result_value);
                                        try self.setTaip(&expr.operand.function.container, result_taip);
                                        try self.setValue(&expr.operand.function.container, result_value);
                                    }
                                    try self.setValue(expr, result_value);
                                    break :et result_taip;
                                }
                            }
                            try self.setValue(expr, try self.doUnaryOp(op, operand_value));
                        }
                        const result_taip = try self.unaryResultTaip(op, operand_taip);
                        break :et result_taip;
                    }
                    else {
                        if (self.valueOf(&expr.operand)) |value| {
                            try self.setValue(expr, value);
                        }
                        break :et operand_taip;
                    }
                },
                *Expr.Eval => {
                    const taip = try self.analyzeExpr(scope, &expr.function);
                    if (self.valueOf(&expr.function)) |value| {
                        try self.setValue(expr, value);
                    }
                    break :et taip;
                    // if (expr.params) |params| {
                    //     for (params.params) |*param| {
                    //         try self.analyzeExpr(scope, param);
                    //     }
                    // }
                },
                *Expr.Access => {
                    const container_taip = try self.analyzeExpr(scope, &expr.container);
                    if (expr.member) |member| {
                        if (container_taip == .module) {
                            if (self.valueOf(&expr.container)) |container| {
                                const module = container.data.module;
                                if (module.scope.findSymbol(member.name)) |member_symbol| {
                                    try self.setDepInfo(expr, .{
                                        .route_id = 0,
                                        .symbol = member_symbol,
                                    });
                                    const member_taip = try self.analyzeSymbol(member_symbol);
                                    if (self.valueOfSymbol(member_symbol)) |value| {
                                        try self.setValue(expr, value);
                                    }
                                    break :et member_taip;
                                }
                                else {
                                    self.unit.diagnostics.logError(
                                        member.name.text,
                                        "module has no member '{s}'",
                                        .{member.name.text},
                                    );
                                    return Error.Unresolved;
                                }
                            }
                            else {
                                self.unit.diagnostics.logError(
                                    member.dot.text,
                                    "could not resolve container value",
                                    .{}
                                );
                                return Error.Unresolved;
                            }
                        }
                        else {
                            self.unit.diagnostics.logError(
                                member.dot.text,
                                "{} does not support member access",
                                .{container_taip},
                            );
                            return Error.Unresolved;
                        }
                    }
                    else {
                        if (self.valueOf(&expr.container)) |value| {
                            try self.setValue(expr, value);
                        }
                        break :et container_taip;
                    }
                },
                *Expr.Atom => switch (expr.*) {
                    .number => |number| {
                        const value: Value = blk: {
                            if (std.fmt.parseUnsigned(Taip.TypeOf(.unsigned), number.text, 10)) |unsigned| {
                                break :blk Value.initConstScalar(.unsigned, unsigned);
                            }
                            else |_| if (std.fmt.parseFloat(Taip.TypeOf(.float), number.text)) |float| {
                                break :blk Value.initConstScalar(.float, float);
                            }
                            else |_| unreachable;
                        };
                        try self.setValue(expr, value);
                        break :et value.taip;
                    },
                    .name => |name| {
                        if (scope.findSymbol(name)) |symbol| {
                            try self.setDepInfo(expr, .{
                                .route_id = 0,
                                .symbol = symbol,
                            });
                            const taip = try self.analyzeSymbol(symbol);
                            if (self.valueOfSymbol(symbol)) |value| {
                                try self.setValue(expr, value);
                            }
                            break :et taip;
                        }
                        else {
                            self.unit.diagnostics.logError(name.text, "use of undeclared symbol '{s}'", .{name.text});
                            return Error.Unresolved;
                        }
                    },
                    .module => {
                        const module = self.valueOf(expr).?.data.module;
                        try self.analyzeModule(module);
                        break :et Taip.init(.module);
                    },
                    .parens => |parens| {
                        const taip = try self.analyzeExpr(scope, parens.expr);
                        if (self.valueOf(parens.expr)) |value| {
                            try self.setValue(expr, value);
                        }
                        break :et taip;
                    },
                    .import => {
                        break :et Taip.init(.module);
                    },
                },
                else => @compileError(@typeName(Node) ++ " is not an expression node"),
            }
        };
        try self.setTaip(expr, expr_taip);
        return expr_taip;

    }

    fn analyzeSymbol(self: Self, symbol: Symbol) Error!Taip {
        switch (symbol) {
            .binding => |binding| {
                return try self.analyzeBinding(binding);
            },
            .where_clause => |where_clause| {
                return try self.analyzeWhereClause(where_clause);
            },
            .function_param => |function_param| {
                return function_param.taip.?;
            },
            .constant => |constant| return constant.taip,
        }
    }

    fn valueOfSymbol(self: Self, symbol: Symbol) ?Value {
        switch (symbol) {
            .binding => |binding| return self.valueOf(binding),
            .where_clause => |where_clause| return self.valueOf(where_clause),
            .function_param => |function_param| return self.valueOf(function_param),
            .constant => |constant| return constant.value,
        }
    }

    fn tryAddSymbol(self: Self, scope: *Scope, symbol_item: anytype) AllocError!?Symbol {
        const symbol = Symbol.init(symbol_item);
        if (try scope.tryAddSymbol(symbol)) |existing| {
            _ = self.unit.diagnostics.startError(
                symbol.name(),
                "{k} shadows existing {kn}",
                .{symbol, existing},
            ).addRelated(&.{existing.name()}, "declared here", .{});
            return existing;
        }
        else {
            return null;
        }
    }
    const CoerceError = language.CoerceError;
    const CastError = language.CastError;

    const CastTaipError = language.CastTaipError;
    const CoerceTaipError = language.CoerceTaipError;
    const CastValueError = language.CastValueError;
    const CoerceValueError = language.CoerceValueError;


    const OpError = BinaryOpError || UnaryOpError;

    const ResultTaipError = language.ResultTaipError;

    const BinaryOpError = language.BinaryOpError;
    const BinaryOpTaipError = language.BinaryOpTaipError;
    const BinaryOpValueError = language.BinaryOpValueError;
    const UnaryOpError = language.UnaryOpError;
    const UnaryOpTaipError = language.UnaryOpTaipError;
    const UnaryOpValueError = language.UnaryOpValueError;


    const CastMode = enum {
        cast, coerce,
    };

    fn logCastOrCoerceError(self: Self, token: anytype, comptime mode: CastMode, err: anytype, a_taip: Taip, a_value: ?Value, b_taip: Taip) @TypeOf(err)!void {
        const ErrSet = switch (mode) {
            .cast => CastError,
            .coerce => CoerceError,
        };
        switch(@errSetCast(ErrSet, err)) {
            ErrSet.Incompatable => self.logError(token, 
                "cannot " ++ @tagName(mode) ++ " {} to {}",
                .{a_taip, b_taip}),
            ErrSet.DimensionMismatch => self.logError(token, 
                "cannot " ++ @tagName(mode) ++ " {} to {} with different dimensions",
                .{a_taip, b_taip}),
            ErrSet.IntegerOverflow => self.logError(token, 
                "failed to " ++ @tagName(mode) ++ " {} {} to {} without overflow",
                .{a_taip, a_value.?, b_taip}),
            ErrSet.NegativeToUnsigned => self.logError(token, 
                "failed to " ++ @tagName(mode) ++ " negative {} {} to {}",
                .{a_taip, a_value.?, b_taip}),
            ErrSet.NanToInteger => self.logError(token, 
                "failed to " ++ @tagName(mode) ++ " {} {} to {}",
                .{a_taip, a_value.?, b_taip}),
            ErrSet.InfinityToInteger => self.logError(token, 
                "failed to " ++ @tagName(mode) ++ " {} {} to {}",
                .{a_taip, a_value.?, b_taip}),
            else => switch (mode) {
                .cast => return,
                .coerce => switch (@errSetCast(ErrSet, err)) {
                    ErrSet.ScalarModeMismatch => self.logError(token, 
                        "cannot coerce {} to {} without a cast",
                        .{a_taip, b_taip}),
                    ErrSet.ScalarInformationLoss => self.logError(token, 
                        "cannot coerce {} to {} without a cast",
                        .{a_taip, b_taip}),
                    else => return,
                }
            }
        }
        return err;
    }

    fn logBinaryOpError(self: Self, err: anytype, op: Token, a_taip: Taip, b_taip: Taip, a_value: ?Value, b_value: ?Value) @TypeOf(err)!void {
        switch (@errSetCast(BinaryOpError, err)) {
            BinaryOpError.DivideByZero => self.logError(op,
                "divide by zero {} {s} {}",
                .{a_value.?, op.text, b_value.?}),
            BinaryOpError.UnsupportedOperation => self.logError(op,
                "undefined operation {} {s} {}",
                .{a_taip, op.text, b_taip}),
            else => switch (op.tag) {
                .plus, .minus, .aster, .fslash => {
                    if (language.binaryResultTaip(op.tag, a_taip, b_taip)) |result_taip| {
                        if (result_taip.eql(a_taip)) return self.logCastOrCoerceError(op, .coerce, err, a_taip, a_value, b_taip);
                        if (result_taip.eql(b_taip)) return self.logCastOrCoerceError(op, .coerce, err, b_taip, b_value, a_taip);
                    }
                    else |_| {
                        try self.checkCoerceTaip(op, a_taip, b_taip);
                        try self.checkCoerceTaip(op, b_taip, a_taip);
                    }
                },
                else => return,
            },
        }
        return err;
    }

    fn logUnaryOpError(self: Self, err: anytype, op: Token, a_taip: Taip, a_value: ?Value) @TypeOf(err)!void {
        switch (@errSetCast(UnaryOpError, err)) {
            UnaryOpError.NegateUnsigned => self.logError(op,
                "cannot negate {}",
                .{a_taip}),
            UnaryOpError.UnsupportedOperation => self.logError(op,
                "undefined operation {s} {}",
                .{op.text, a_taip}),
            else => switch (op.tag) {
                .plus, .minus => {
                    if (language.unaryResultTaip(op.tag, a_taip)) |result_taip| {
                        try self.logCastOrCoerceError(op, .coerce, err, a_taip, a_value.?, result_taip);
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


    fn checkCastTaip(self: Self, token: anytype, a: Taip, b: Taip) CastTaipError!void {
        errdefer |err| self.logCastOrCoerceError(token, .coerce, err, a, null, b) catch {};
        try language.checkCastTaip(a, b);
    }

    fn checkCoerceTaip(self: Self, token: anytype, a: Taip, b: Taip) CoerceTaipError!void {
        errdefer |err| self.logCastOrCoerceError(token, .coerce, err, a, null, b) catch {};
        try language.checkCoerceTaip(a, b);
    }

    fn castValue(self: Self, token: anytype, a: Value, b: Taip) CastError!Value {
        if (a.taip.eql(b)) {
            return a;
        }
        try self.checkCastTaip(token, a.taip, b);
        return try self.coerceOrCastValueAssumeSafe(token, .cast, a, b);
    }

    fn coerceValue(self: Self, token: anytype, a: Value, b: Taip) CoerceError!Value {
        if (a.taip.eql(b)) {
            return a;
        }
        try self.checkCoerceTaip(token, a.taip, b);
        return try self.coerceOrCastValueAssumeSafe(token, .coerce, a, b);        
    }

    fn coerceOrCastValueAssumeSafe(self: Self, token: anytype, comptime mode: CastMode, a: Value, b: Taip) CastValueError!Value {
        errdefer |err| self.logCastOrCoerceError(token, mode, err, a.taip, a, b) catch {};
        return try language.castValueAssumeSafe(a, b);
    }

    fn binaryResultTaip(self: Self, op: Token, a: Taip, b: Taip) BinaryOpTaipError!Taip {
        errdefer |err| self.logBinaryOpError(err, op, a, b, null, null) catch {};
        return try language.binaryResultTaip(op.tag, a, b);
    }

    fn unaryResultTaip(self: Self, op: Token, a: Taip) UnaryOpTaipError!Taip {
        errdefer |err| self.logUnaryOpError(err, op, a, null) catch {};
        return try language.unaryResultTaip(op.tag, a);
    }


    fn doBinaryOp(self: Self, op: Token, a: Value, b: Value) BinaryOpError!Value {
        const res = try self.binaryResultTaip(op, a.taip, b.taip);
        const av = try self.coerceValue(op, a, res);
        const bv = try self.coerceValue(op, b, res);
        errdefer |err| self.logBinaryOpError(err, op, a.taip, b.taip, a, b) catch {};
        return try language.doBinaryOpAssumeSafe(op.tag, res, av, bv);
    }

    fn doUnaryOp(self: Self, op: Token, a: Value) UnaryOpError!Value {
        const res = try self.unaryResultTaip(op, a.taip);
        const av = try self.coerceValue(op, a, res);
        errdefer |err| self.logUnaryOpError(err, op, a.taip, a) catch {};
        return try language.doUnaryOpAssumeSafe(op.tag, res, av);
    }


    fn meta(self: Self, node: anytype) AllocError!?*Sem.Metadata {
        if (metaSlice(node)) |slice| {
            if (self.sem.meta_map.getOrPut(self.allocator(), @ptrToInt(slice.ptr))) |kv| {
                const m = kv.value_ptr;
                if (!kv.found_existing) {
                    m.* = Sem.Metadata {
                        .slice = slice,
                        .taip = undefined,
                        .value = null,
                    };
                }
                return m;
            }
            else |_| {
                return AllocError.OutOfMemory;
            }
        }
        else {
            return null;
        }
    }

    fn metaSlice(node: anytype) ?[]const u8 {
        const Node = @TypeOf(node);
        return switch (Node) {
            *Sem.Binding => node.decl.name,
            *Sem.WhereClause => node.decl.name,
            *Sem.Function.Param => node.name,
            *Expr.Add.Term => node.op.text,
            *Expr.Mul.Term => node.op.text,
            *Expr.Neg => (
                if (node.op) |op| op.text
                else null
            ),
            *Expr.Access => (
                if (node.member) |member| member.name.text
                else null
            ),
            *Expr.Atom => switch (node.*) {
                .number => node.number.text,
                .name => node.name.text,
                .module => node.module.kw_module.text,
                .import => node.import.kw_import.text,
                else => null,
            },
            else => null,
        };
    }

};