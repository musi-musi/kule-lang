const std = @import("std");
const language = @import("../language.zig");

const Allocator = std.mem.Allocator;
const Arena = std.heap.ArenaAllocator;


const Taip = language.Taip;
const Value = language.Value;
const Constant = language.Constant;


const Token = language.Token;
const Syntax = language.Syntax;


const Expr = Syntax.Expr;

const AllocError = Allocator.Error;

const Node = struct {
    addr: usize,
    type_name_addr: usize,
};

fn NodeMap(comptime V: type) type {
    return std.HashMapUnmanaged(Node, V, struct {

        pub fn hash(_: @This(), node: Node) u64 {
            const h:usize = node.addr ^ node.type_name_addr;
            if (@sizeOf(usize) <= @sizeOf(u64)) {
                return @intCast(u64, h);
            }
            else {
                return @truncate(u64, h);
            }
        }

        pub fn eql(_: @This(), a: Node, b: Node) bool {
            return a.addr == b.addr and a.type_name_addr == b.type_name_addr;
        }

    }, 80);
}

pub const Semantics = struct {

    unit_name: []const u8,
    allocator: Allocator,

    module: Module = undefined,


    taip_map: NodeMap(Taip) = .{},
    value_map: NodeMap(Value) = .{},
    dep_map: NodeMap(DepInfo) = .{},

    meta_map: MetadataMap = .{},

    pub const DepInfo = struct {
        route_id: usize,
        symbol: Symbol,
    };

    const Self = @This();

    pub fn init(allocator: Allocator, unit_name: []const u8) Self {
        return Self {
            .unit_name = unit_name,
            .allocator = allocator,
        };
    }

    fn initNode(node_ptr: anytype) Node {
        const meta = std.meta;
        const trait = meta.trait;
        const T = @TypeOf(node_ptr);
        comptime {
            if (!trait.isSingleItemPtr(T)) {
                @compileError(@typeName(T) ++ " is not a single item pointer");
            }
            else if(trait.is(.Pointer)(meta.Child(T))) {
                @compileError(@typeName(T) ++ " is a pointer to another pointer");
            }
        }
        const type_name = @typeName(comptime meta.Child(T));
        return Node {
            .addr = @ptrToInt(node_ptr),
            .type_name_addr = @ptrToInt(type_name),
        };
    }

    fn set(allocator: Allocator, comptime V: type, map: *NodeMap(V), node_ptr: anytype, value: anytype) AllocError!void {
        const node = initNode(node_ptr);
        try map.put(allocator, node, value);
    }

    fn get(comptime V: type, map: *NodeMap(V), node_ptr: anytype) ?V {
        const node = initNode(node_ptr);
        return map.get(node);
    }

    pub fn setTaip(self: *Self, node_ptr: anytype, taip: Taip) AllocError!void
        { try set(self.allocator, Taip, &self.taip_map, node_ptr, taip); }
    pub fn taipOf(self: *Self, node_ptr: anytype) ?Taip
        { return get(Taip, &self.taip_map, node_ptr); }

    pub fn setValue(self: *Self, node_ptr: anytype, value: Value) AllocError!void
        { try set(self.allocator, Value, &self.value_map, node_ptr, value); }
    pub fn valueOf(self: *Self, node_ptr: anytype) ?Value
        { return get(Value, &self.value_map, node_ptr); }

    pub fn setDepInfo(self: *Self, node_ptr: anytype, dep_info: DepInfo) AllocError!void
        { try set(self.allocator, DepInfo, &self.dep_map, node_ptr, dep_info); }
    pub fn depInfoOf(self: *Self, node_ptr: anytype) ?DepInfo
        { return get(DepInfo, &self.dep_map, node_ptr); }


    pub const Module = struct {
        scope: Scope,
        statements: []Statement,


        pub fn init(self: *Module, parent_scope: ?*Scope, semantics: *Semantics, module_statements_opt: ?Syntax.Statements) Allocator.Error!void {
            self.scope = Scope.init(semantics, parent_scope, self);
            self.statements = &.{};
            const allocator = semantics.allocator;
            if (module_statements_opt) |module_statements| {
                if (module_statements.len > 0) {
                    const statements = try allocator.alloc(Statement, module_statements.len);
                    for (module_statements) |*s, i| {
                        try statements[i].init(&self.scope, s);
                    }
                    self.statements = statements;
                }
            }
        }

        pub fn format(self: Module, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
            if (self.scope.parent) |parent| {
                switch (parent.container) {
                    .binding => |binding| try writer.print("{}", .{binding}),
                    .where_clause => |where_clause| try writer.print("{}", .{where_clause}),
                    else => {},
                }
            }
            else {
                try writer.print("@[{s}]", .{self.scope.semantics.unit_name});
            }

        }

    };

    pub const Statement = struct {

        visibility: Visibility,
        body: Body,


        pub fn init(self: *Statement, parent_scope: *Scope, statement: *Syntax.Statement) Allocator.Error!void {
            if (statement.kw_pub == null) {
                self.visibility = .local;
            }
            else {
                self.visibility = .public;
            }
            switch (statement.body) {
                .binding => {
                    self.body = Body {
                        .binding = undefined,
                    };
                    try self.body.binding.init(parent_scope, &statement.body.binding);
                },
            }
        }

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
        where_clauses: []WhereClause,

        pub fn init(self: *Binding, parent_scope: *Scope, binding: *Syntax.Statement.Binding) Allocator.Error!void {
            try self.decl.init(parent_scope, self, &binding.decl);
            const allocator = parent_scope.semantics.allocator;
            self.where_clauses = &.{};
            if (binding.where_clauses) |binding_clauses| {
                if (binding_clauses.len > 0) {
                    const clauses = try allocator.alloc(WhereClause, binding_clauses.len);
                    for (binding_clauses) |*wc, i| {
                        try clauses[i].init(&self.decl.body_scope, wc);
                    }
                    self.where_clauses = clauses;
                }
            }
        }
        

        pub fn format(self: Binding, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
            try self.decl.formatParent(writer);
            try writer.writeAll(".");
            try self.decl.format(writer);
        }

    };

    pub const WhereClause = struct {
        decl: Decl,

        pub fn init(self: *WhereClause, parent_scope: *Scope, where_clause: *Syntax.WhereClause) Allocator.Error!void {
            try self.decl.init(parent_scope, self, &where_clause.decl);
        }

        pub fn format(self: WhereClause, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
            try self.decl.formatParent(writer);
            try writer.writeAll(".where::");
            try self.decl.format(writer);
        }

    };

    pub const Decl = struct {
        name: []const u8,
        taip_expr: ?*Expr,
        body: Body,

        body_scope: Scope,


        pub fn init(self: *Decl, parent_scope: *Scope, container: anytype, decl: *Syntax.Decl) Allocator.Error!void {
            self.name = decl.name.text;
            self.body_scope = Scope.init(parent_scope.semantics, parent_scope, container);
            const allocator = parent_scope.semantics.allocator;
            if (decl.type_expr) |*type_expr| {
                self.taip_expr = &type_expr.expr;
            }
            else {
                self.taip_expr = null;
            }
            if (decl.param_list) |param_list| {
                const decl_params = param_list.params;
                var function = Function{
                    .params = &.{},
                    .body = &decl.expr,
                };
                if (decl_params.len > 0) {
                    const params = try allocator.alloc(Function.Param, decl_params.len);
                    for (decl_params) |*p, i| {
                        params[i].name = p.name.text;
                        params[i].taip_expr = &p.type_expr.expr;
                    }
                    function.params = params;
                }
                self.body = Body {
                    .function = function,
                };
            }
            else {
                self.body = Body {
                    .direct = &decl.expr,
                };
            }

        }


        pub const Body = union(enum) {
            direct: *Expr,
            function: Function,
        };

        fn formatParent(self: Decl, writer: anytype) @TypeOf(writer).Error!void {
            if (self.body_scope.parent) |parent| {
                switch (parent.container) {
                    .binding => |binding| try writer.print("{}", .{binding}),
                    .module => |module| try writer.print("{}", .{module}),
                    else => {},
                }
            }
        }

        fn format(self: Decl, writer: anytype) @TypeOf(writer).Error!void {
            try writer.writeAll(self.name);
        }

    };

    pub const Function = struct {
        
        params: []Param,
        body: *Expr,
        return_taip: ?Taip = null,

        pub const Param = struct {
            name: []const u8,
            taip_expr: *Expr,
            taip: ?Taip = null,
        };

    };

    pub const SymbolTable = std.StringHashMapUnmanaged(Symbol);

    pub const Scope = struct {
        semantics: *Semantics,
        parent: ?*Scope = null,
        container: Container,
        symbols: SymbolTable = .{},

        pub const AddSymbolError = error {
            ShadowsExistingSymbol,
        };

        pub const Container = union(enum) {
            module: *Module,
            binding: *Binding,
            where_clause: *WhereClause,

            pub fn format(self: Container, comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
                switch (self) {
                    .module => try writer.writeAll("module"),
                    .binding => |binding| try writer.print("binding '{s}'", .{binding.decl.name}),
                    .where_clause => |where_clause| try writer.print("where clause '{s}'", .{where_clause.decl.name}),
                }
            }

        };

        pub fn init(semantics: *Semantics, parent: ?*Scope, container_node: anytype) Scope {
            const T = @TypeOf(container_node);
            const container = switch (T) {
                *Module => Container {
                    .module = container_node,
                },
                *Binding => Container {
                    .binding = container_node,
                },
                *WhereClause => Container {
                    .where_clause = container_node,
                },
                else => @compileError(@typeName(T) ++ " is not a scope container"),
            };
            return Scope {
                .semantics = semantics,
                .parent = parent,
                .container = container,
            };
        }

        pub fn findLocalSymbol(self: Scope, name: anytype) ?Symbol {
            return self.symbols.get(nameOf(name));
        }

        pub fn findGlobalSymbol(self: Scope, name: anytype) ?Symbol {
            if (language.lookupConstant(nameOf(name))) |constant| {
                return Symbol {
                    .constant = constant,
                };
            }
            else if (self.parent) |parent| {
                return parent.findSymbol(name);
            }
            else {
                return null;
            }
        }

        pub fn findSymbol(self: Scope, name: anytype) ?Symbol {
            return self.findLocalSymbol(name) orelse self.findGlobalSymbol(name);
        }

        /// try to add a symbol to this scope
        /// if the symbol name is already in scope (including constants and parent scopes)
        /// returns the existing symbol
        /// returns null on successful symbol addition
        pub fn tryAddSymbol(self: *Scope, symbol_item: anytype) Allocator.Error!?Symbol {
            const allocator = self.semantics.allocator;
            const symbol = Symbol.init(symbol_item);
            const name = symbol.name();
            if (self.findSymbol(name)) |existing| {
                return existing;
            }
            else {
                try self.symbols.put(allocator, name, symbol);
                return null;
            }
        }


    };

    pub const Symbol = union(enum) {
        binding: *Binding,
        where_clause: *WhereClause,
        function_param: *Function.Param,
        constant: *const Constant,

        pub fn init(item: anytype) Symbol {
            const T = @TypeOf(item);
            return switch (T) {
                Symbol => item,
                *Binding => Symbol { .binding = item },
                *WhereClause => Symbol { .where_clause = item },
                *Function.Param => Symbol { .function_param = item },
                *const Constant => Symbol { .constant = item },
                else => @compileError(@typeName(T) ++ " is not a symbol"),
            };
        }

        pub fn name(self: Symbol) []const u8 {
            return switch (self) {
                .binding => |binding| binding.decl.name,
                .where_clause => |where_clause| where_clause.decl.name,
                .function_param => |function_param| function_param.name,
                .constant => |constant| constant.name,
            };
        }

        pub fn kindName(self: Symbol) []const u8 {
            return switch (self) {
                .binding => "binding",
                .where_clause => "where clause",
                .function_param => "function parameter",
                .constant => "language constant",
            };
        }

        pub fn format(self: Symbol, comptime fmt: []const u8, _: std.fmt.FormatOptions, writer: anytype) @TypeOf(writer).Error!void {
            var print_kind: bool = false;
            var print_name: bool = false;
            for (fmt) |c| {
                if (c == 'k') print_kind = true;
                if (c == 'n') print_name = true;
            }
            if (!print_kind and !print_name) {
                print_kind = true;
                print_name = true;
            }
            if (print_kind and print_name) {
                try writer.print("{s} '{s}'", .{self.kindName(), self.name()});
            }
            else if (print_kind) {
                try writer.print("{s}", .{self.kindName()});
            }
            else if (print_name) {
                try writer.print("'{s}'", .{self.name()});

            }
        }

    };

    pub fn nameOf(item: anytype) []const u8 {
        const T = @TypeOf(item);
        if (comptime std.meta.trait.isSingleItemPtr(T)) {
            return nameOf(item.*);
        }
        else if (comptime std.meta.trait.isZigString(T)) {
            return item;
        }
        else if (T == Token) {
            return item.text;
        }
        else if (T == Symbol) {
            return item.name();
        }
        else if (@hasField(T, "name")) {
            return nameOf(item.name);
        }
        else if (@hasField(T, "decl")) {
            return nameOf(item.decl);
        }
        else {
            @compileError(@typeName(T) ++ " does not have a name");
        }
    }

    pub const Metadata = struct {
        slice: []const u8,
        taip: Taip,
        value: ?Value,
    };

    pub const MetadataMap = std.HashMapUnmanaged(usize, Metadata, struct {
        pub fn hash(_: @This(), addr: usize) u64 {
            if (@sizeOf(usize) <= @sizeOf(u64)) {
                return @intCast(u64, addr);
            }
            else {
                return @truncate(u64, addr);
            }
        }

        pub fn eql(_: @This(), a: usize, b: usize) bool {
            return a == b;
        }
    }, 80);
};
