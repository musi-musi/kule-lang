const std = @import("std");
const source = @import("source.zig");
const lexer = @import("lexer.zig");
const log = @import("log.zig");
const diagnostics = @import("diagnostics.zig");

const Source = source.Source;
const SourceLocation = source.SourceLocation;
const Token = lexer.Token;
const TokenTag = lexer.TokenTag;
const TokenStream = lexer.TokenStream;

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Diagnostics = diagnostics.Diagnostics;

pub const Parser = struct {

    allocator: Allocator,
    arena: ?*std.heap.ArenaAllocator,
    source: Source,
    tokens: TokenStream,
    error_count: usize = 0,
    diagnostics: ?*Diagnostics,

    pub const Error = error {
        ParseFailed,
    } || TokenStream.Error || Allocator.Error;

    pub fn init(allocator: Allocator, src: Source, diags: ?*Diagnostics) TokenStream.Error!Parser {
        return Parser {
            .allocator = allocator,
            .arena = null,
            .source = src,
            .tokens = try TokenStream.init(src, diags),
            .diagnostics = diags,
        };
    }

    pub fn deinit(self: Parser) void {
        _ = self;
        // self.arena.deinit();
    }

    pub fn parse(self: *Parser) Error!Ast {
        var tree = try Ast.parse(self);
        errdefer tree.deinit();
        self.error_count += self.tokens.error_count;
        if (self.error_count > 0) {
            return Error.ParseFailed;
        }
        return tree;
    }

    fn acceptEof(self: *Parser) Error!void {
        if (self.tokens.next()) |token| {
            self.sourceErrorUnexpectedToken(token, "end of file");
            return Error.ParseFailed;
        }
    }

    fn acceptNode(self: *Parser, comptime Node: type) Error!Node {
        if (try self.acceptNodeOpt(Node)) |node| {
            return node;
        }
        else {
            self.sourceErrorUnexpectedToken(self.tokens.lookahead,  Node.node_name);
            return Error.ParseFailed;
        }
    }

    fn acceptNodeOpt(self: *Parser, comptime Node: type) Error!?Node {
        if (self.nextIsNotOne(Node.start_tags)) {
            return null;
        }
        else {
            return try Node.parse(self);
        }
    }


    fn astAllocator(self: *Parser) Allocator {
        return self.arena.?.allocator();
    }

    pub fn isAtEof(self: *Parser) bool {
        return self.tokens.lookahead.tag == .end_of_file;
    }

    pub fn nextIsNot(self: Parser, comptime tag: TokenTag) bool {
        return self.nextIsNotOne(&.{tag});
    }

    pub fn nextIsNotOne(self: Parser, comptime tags: [] const TokenTag) bool {
        inline for (tags) |tag| {
            if (self.tokens.lookahead.tag == tag) {
                return false;
            }
        }
        return true;
    }


    pub fn expect(self: *Parser, comptime tag: TokenTag) Error!Token {
        return self.expectOne(&.{tag});
    }

    pub fn expectOpt(self: Parser, comptime tag: TokenTag) ?Token {
        return self.expectOneOpt(&.{tag});
    }

    pub fn expectOne(self: *Parser, comptime tags: []const TokenTag) Error!Token {
        if (self.expectOneOpt(tags)) |token| {
            return token;
        }
        else {
            comptime var expected = tags[0].name();
            comptime {
                if (tags.len > 1) {
                    if (tags.len > 2) {
                        for (tags[1..(tags.len - 1)]) |tag| {
                            expected = std.fmt.comptimePrint("{s}, {s}", .{expected, tag.name()});
                        }
                        expected = std.fmt.comptimePrint("{s}, or {s}", .{expected, tags[tags.len - 1].name()});
                    }
                    else {
                        expected = std.fmt.comptimePrint("{s} or {s}", .{expected, tags[1].name()});
                    }
                }
            }
            self.sourceErrorUnexpectedToken(self.tokens.lookahead, expected);
            return error.ParseFailed;
        }
    }

    pub fn expectOneOpt(self: Parser, comptime tags: []const TokenTag) ?Token {
        const next = self.tokens.lookahead;
        inline for (tags) |tag| {
            if (next.tag == tag) {
                return next;
            }
        }
        return null;
    }

    pub fn accept(self: *Parser, comptime tag: TokenTag) Error!Token {
        return self.acceptOne(&[1]TokenTag{tag});
    }

    pub fn acceptOpt(self: *Parser, comptime tag: TokenTag) TokenStream.Error!?Token {
        return self.acceptOneOpt(&[1]TokenTag{tag});
    }

    pub fn acceptOne(self: *Parser, comptime tags: []const TokenTag) Error!Token {
        const token = self.expectOne(tags);
        _ = try self.tokens.tryNext();
        return token;
    }

    pub fn acceptOneOpt(self: *Parser, comptime tags: []const TokenTag) TokenStream.Error!?Token {
        const token = self.expectOneOpt(tags);
        if (token != null) {
            _ = try self.tokens.tryNext();
        }
        return token;
    }

    fn sourceErrorUnexpectedToken(self: *Parser, token: Token, comptime expected_desc: []const u8) void {
        if (token.tag == .end_of_file){
            self.sourceError(token.text, "expected {s}, found end of file", .{expected_desc});
        }
        else {
            self.sourceError(token.text, "expected {s}, found \"{s}\"", .{expected_desc, token.text});
        }
    }

    pub fn sourceError(self: *Parser, token: []const u8, comptime format: []const u8, args: anytype) void {
        self.error_count += 1;
        if (self.diagnostics) |diags| {
            diags.sourceErrorErrorPanic(&self.tokens.source, token, format, args);
        }
    }

};

pub const Ast = struct {

    source: Source,
    arena: ArenaAllocator,
    root: ast.TopLevel = undefined,

    pub fn deinit(self: Ast) void {
        self.arena.deinit();
    }

    fn parse(p: *Parser) Parser.Error!Ast {
        var self = Ast {
            .source = p.source,
            .arena = ArenaAllocator.init(p.allocator),
        };
        p.arena = &self.arena;
        errdefer self.deinit();
        self.root = try ast.TopLevel.parse(p);
        return self;

    }

    pub fn dump(self: Ast, w: anytype) !void {
        const source_name = self.source.name orelse "???";
        try w.print("{s} ast:\n", .{source_name});
        try self.root.dump(w, 1);
    }
    
    pub usingnamespace ast;

};

const ast = struct {

    const E = Parser.Error;

    fn writeIndent(w: anytype, level: usize, text: []const u8) @TypeOf(w).Error!void {
        try w.writeByteNTimes(' ', level);
        try w.writeAll(text);
        try w.writeByte('\n');
    }

    fn printIndent(w: anytype, level: usize, comptime format: []const u8, args: anytype) @TypeOf(w).Error!void {
        try w.writeByteNTimes(' ', level);
        try w.print(format, args);
        try w.writeByte('\n');
    }

    fn dumpToken(w: anytype, level: usize, name: []const u8, token: Token) @TypeOf(w).Error!void {
        try printIndent(w, level, "{s} {s}", .{ name, token.text });
    }

    fn dumpList(w: anytype, level: usize, name: []const u8, comptime Item: type, items: []const Item) @TypeOf(w).Error!void {
        try writeIndent(w, level, name);
        for (items) |item, i| {
            try printIndent(w, level + 1, "[{d}]", .{i});
            if (Item == Token) {
                try printIndent(w, level + 2, "{s}", .{item.text});
            }
            else {
                try item.dump(w, level + 2);
            }
        }
    }

    fn heap(p: *Parser, node: anytype) Allocator.Error!*const @TypeOf(node) {
        const T = @TypeOf(node);
        const heap_node = try p.astAllocator().create(T);
        heap_node.* = node;
        return heap_node;
    }

    fn typeAnnotation(p: *Parser) E!Expr {
        _ = try p.accept(.colon);
        return p.acceptNode(Expr);
    }

    fn ParseFn(comptime T: type) type {
        return fn(*Parser) E!T;
    }

    fn delimitedList(
        p: *Parser,
        comptime Node: type,
        comptime delim: TokenTag,
        comptime end_predicate: fn (Token) bool,
        comptime allow_trailing: bool
    ) E![]const Node {
        const allocator = p.astAllocator();
        var items = std.ArrayListUnmanaged(Node){};
        while (!end_predicate(p.tokens.lookahead)) {
            if (items.items.len > 0) {
                _ = try p.accept(delim);
            }
            const item = try p.acceptNode(Node);
            try items.append(allocator, item);
        }
        if (allow_trailing) {
            _ = try p.acceptOpt(delim);
        }
        return items.toOwnedSlice(allocator);
    }

    fn unboundDelimitedList(
        p: *Parser,
        comptime Node: type,
        comptime delim: TokenTag,
    ) E![]const Node {
        const allocator = p.astAllocator();
        var items = std.ArrayListUnmanaged(Node){};
        while (true) {
            if (try p.acceptNodeOpt(Node)) |item| {
                try items.append(allocator, item);
                if ((try p.acceptOpt(delim)) == null) {
                    break;
                }
            }
            else {
                break;
            }
        }
        return items.toOwnedSlice(allocator);
    }

    fn list(
        p: *Parser, comptime Node: type) E![]const Node {
        const allocator = p.astAllocator();
        var items = std.ArrayListUnmanaged(Node){};
        while (try p.acceptNodeOpt(Node)) |item| {
            try items.append(allocator, item);
        }
        return items.toOwnedSlice(allocator);
    }

    fn tagIsPredicate(comptime tag: TokenTag) fn(Token) bool {
        return tagIsOnePredicate(&.{tag});
    }
    
    fn tagIsOnePredicate(comptime tags: []const TokenTag) fn(Token) bool {
        return struct {
            fn f(next: Token) bool {
                inline for (tags) |tag| {
                    if (next.tag == tag) {
                        return true;
                    }
                }
                return false;
            }
        }.f;
    }

    fn tagIsNotPredicate(comptime tag: TokenTag) fn(Token) bool {
        return tagIsNotOnePredicate(&.{tag});
    }

    fn tagIsNotOnePredicate(comptime tags: []const TokenTag) fn(Token) bool {
        return struct {
            fn f(next: Token) bool {
                inline for (tags) |tag| {
                    if (next.tag == tag) {
                        return false;
                    }
                }
                return true;
            }
        }.f;
    }

    fn eofPredicate(next: Token) bool {
        return next.tag == .end_of_file;
    }

    pub const TopLevel = struct {
        decls: []const LetDecl,

        fn parse(p: *Parser) E!TopLevel {
            const decls = try list(p, LetDecl);
            _ = try p.expectOne(LetDecl.start_tags ++ &[_]TokenTag{ .end_of_file });
            return TopLevel {
                .decls = decls,
            };
        }

        fn dump(self: TopLevel, w: anytype, level: usize) @TypeOf(w).Error!void {
            try dumpList(w, level, "top_level", LetDecl, self.decls);
        }

    };

    pub const ParamDecl = struct {
        name: Token,
        type_annotation: Expr,

        const node_name = "parameter declaration";
        const start_tags: [] const TokenTag = &.{ .identifier, };

        fn parse(p: *Parser) E!ParamDecl {
            const name = try p.accept(.identifier);
            blk: {
                if ((try p.acceptOpt(.colon)) != null) {
                    if (p.nextIsNotOne(&.{.rparen, .comma})) {
                        return ParamDecl {
                            .name = name,
                            .type_annotation = try p.acceptNode(Expr),
                        };
                    }
                    else {
                        break :blk;
                    }
                }
                else {
                    break :blk;
                }
            }
            p.sourceError(name.text, "parameter missing type annotation", .{});
            return ParamDecl {
                .name = name,
                .type_annotation = undefined,
            };
        }

        fn dump(self: ParamDecl, w: anytype, level: usize) @TypeOf(w).Error!void {
            try writeIndent(w, level, "param");
            try dumpToken(w, level + 1, "(name)", self.name);
            try writeIndent(w, level + 1, "(type)");
            try self.type_annotation.dump(w, level + 2);
        }
    };


    pub const LetDecl = struct {
        is_pub: bool,
        decl: Decl,
        where_clauses: []const WhereClause,

        const node_name = "let statement";
        const start_tags: [] const TokenTag = &.{ .kw_pub, .kw_let, };
        
        fn parse(p: *Parser) E!LetDecl {
            const is_pub: bool = (try p.acceptOne(&.{ .kw_pub, .kw_let })).tag == .kw_pub;
            if (is_pub) {
                _ = try p.accept(.kw_let);
            }
            const decl = try p.acceptNode(Decl);
            const where_clauses = try list(p, WhereClause);
            return LetDecl {
                .is_pub = is_pub,
                .decl = decl,
                .where_clauses = where_clauses,
            };
        }

        fn dump(self: LetDecl, w: anytype, level: usize) @TypeOf(w).Error!void {
            try writeIndent(w, level, "let_decl");
            try printIndent(w, level + 1, "(is_pub) {}", .{self.is_pub});
            try self.decl.dump(w, level + 1);
            if (self.where_clauses.len > 0) {
                try dumpList(w, level + 1, "where_clauses", WhereClause, self.where_clauses);
            }
        }
    };

    pub const Decl = struct {
        name: Token,
        type_annotation: ?Expr,
        value: Expr,
        parameters: ?[]const ParamDecl,

        const node_name = "declaration";
        const start_tags: [] const TokenTag = &.{ .identifier, };

        fn parse(p: *Parser) E!Decl {
            const name = try p.accept(.identifier);
            const parameters: ?[]const ParamDecl = blk: {
                if ((try p.acceptOpt(.lparen)) != null) {
                    const params = try delimitedList(p, ParamDecl, .comma, comptime tagIsPredicate(.rparen), true);
                    const rparen = try p.accept(.rparen);
                    if (params.len == 0) {
                        p.sourceError(rparen.text, "parameterized declaration requires at least one parameter", .{});
                    }
                    break :blk params;
                }
                else {
                    break :blk null;
                }
            };
            const type_annotation = (
                if (p.expectOpt(.colon) != null)
                    try typeAnnotation(p)
                else
                    null
            );
            _ = try p.accept(.equal);
            const value = try p.acceptNode(Expr);
            
            return Decl {
                .name = name,
                .type_annotation = type_annotation,
                .value = value,
                .parameters = parameters,
            };
        }

        fn dump(self: Decl, w: anytype, level: usize) @TypeOf(w).Error!void {
            try writeIndent(w, level, "decl");
            try dumpToken(w, level + 1, "(name)", self.name);
            if (self.type_annotation) |type_annotation| {
                try writeIndent(w, level + 1, "(type)");
                try type_annotation.dump(w, level + 2);
            }
            try writeIndent(w, level + 1,"(value)");
            try self.value.dump(w, level + 2);
            if (self.parameters) |parameters| {
                try dumpList(w, level + 1, "(parameters)", ParamDecl, parameters);
            }
        }

    };

    pub const WhereClause = struct {
        decl: Decl,

        const node_name = "where clause";
        const start_tags: [] const TokenTag = &.{ .kw_where, };

        fn parse(p: *Parser) E!WhereClause {
            _ = try p.accept(.kw_where);
            return WhereClause {
                .decl = try p.acceptNode(Decl),
            };
        }

        fn dump(self: WhereClause, w: anytype, level: usize) @TypeOf(w).Error!void {
            try writeIndent(w, level, "where_clause");
            try self.decl.dump(w, level + 1);
        }
    };

    pub const Expr = union(enum) {
        
        binary: Binary,
        unary: Unary,
        cast: Cast,
        member_access: MemberAccess,
        param_eval: ParamEval,
        identifier: Token,
        num_literal: Token,
        module_def: ModuleDef,
        import: Import,

        const node_name = "expression";
        const start_tags: []const TokenTag = keyword_start_tags ++ atom_start_tags ++ prefix_start_tags;

        const keyword_start_tags: [] const TokenTag = &.{.kw_module, .kw_import};
        const atom_start_tags: [] const TokenTag = &.{ .identifier, .number, .lparen };
        const prefix_start_tags: []const TokenTag = &.{ .plus, .minus, };

        const ExprParser = fn(*Parser, comptime usize) E!Expr;

        const parse_list = [_]ExprParser {
            binaryExpr(&.{ .plus, .minus}),
            binaryExpr(&.{ .aster, .fslash}),
            prefixExpr(prefix_start_tags),
            castExpr,
            paramEvalExpr,
            memberAccessExpr,
            atomExpr,
        };


        fn parse(p: *Parser) E!Expr {
            return parseLevel(p, 0);
        }

        fn parseLevel(p: *Parser, level: usize) E!Expr {
            return parse_list[level](p, level);
        }

        fn parseNonOperator(p: *Parser) E!Expr {
            const level: usize = comptime blk: {
                for (parse_list) |expr_parser, i| {
                    if (expr_parser == paramEvalExpr) {
                        break :blk i;
                    }
                }
            };
            return parseLevel(p, level);
        }

        fn binaryExpr(comptime operators: []const TokenTag) ExprParser {
            return struct {
                fn f(p: *Parser, level: usize) E!Expr {
                    const lhs = try parseLevel(p, level + 1);
                    if (try p.acceptOneOpt(operators)) |op| {
                        const rhs = try parseLevel(p, level);
                        return Expr{
                            .binary = .{
                                .lhs = try heap(p, lhs),
                                .rhs = try heap(p, rhs),
                                .op = op,
                            },
                        };
                    }
                    else {
                        return lhs;
                    }
                }
            }.f;
        }

        fn prefixExpr(comptime operators: []const TokenTag) ExprParser {
            return struct {
                fn f(p: *Parser, level: usize) E!Expr {
                    const op_opt = try p.acceptOneOpt(operators);
                    const subject = try parseLevel(p, level + 1);
                    if (op_opt) |op| {
                        return Expr{
                            .unary = .{
                                .subject = try heap(p, subject),
                                .op = op,
                            },
                        };
                    }
                    else {
                        return subject;
                    }
                }
            }.f;
        }

        fn castExpr(p: *Parser, level: usize) E!Expr {
            const subject = try parseLevel(p, level + 1);
            if ((try p.acceptOpt(.kw_as)) != null) {
                const target = try parseNonOperator(p);
                return Expr {
                    .cast = .{
                        .subject = try heap(p, subject),
                        .target = try heap(p, target),
                    },
                };
            }
            else {
                return subject;
            }
        }

        fn memberAccessExpr(p: *Parser, level: usize) E!Expr {
            const container = try parseLevel(p, level + 1);
            if ((try p.acceptOpt(.dot)) != null)  {
                const member_name = try p.accept(.identifier);
                return Expr {
                    .member_access = .{
                        .container = try heap(p, container),
                        .member_name = member_name,
                    },
                };
            }
            else {
                return container;
            }
        }

        fn paramEvalExpr(p: *Parser, level: usize) E!Expr {
            const subject = try parseLevel(p, level + 1);
            if ((try p.acceptOpt(.lparen)) != null)  {
                const params = try delimitedList(p, Expr, .comma, comptime tagIsPredicate(.rparen), true);
                const rparen = try p.accept(.rparen);
                if (params.len == 0) {
                    p.sourceError(rparen.text, "parameterized evaluation requires at least one parameter", .{});
                }
                return Expr {
                    .param_eval = .{
                        .subject = try heap(p, subject),
                        .params = params,
                    },
                };
            }
            else {
                return subject;
            }
        }

        fn atomExpr(parser: *Parser, _: usize) E!Expr {
            const Atom = struct {
                expr: Expr,

                const node_name = Expr.node_name;
                const start_tags: [] const TokenTag = keyword_start_tags ++ atom_start_tags;
                

                const Atom = @This();

                fn parse(p: *Parser) E!Atom {
                    const expr: Expr = blk: {
                        if (p.expectOneOpt(keyword_start_tags)) |token| {
                            switch (token.tag) {
                                .kw_module => {
                                    break :blk Expr {
                                        .module_def = try p.acceptNode(ModuleDef),
                                    };
                                },
                                .kw_import => {
                                    break :blk Expr {
                                        .import = try p.acceptNode(Import),
                                    };
                                },
                                else => unreachable,
                            }
                        }
                        else {
                            const token = try p.acceptOne(atom_start_tags);
                            break :blk switch (token.tag) {
                                .identifier => Expr { .identifier = token },
                                .number => Expr { .num_literal = token },
                                .lparen => {
                                    const expr = try p.acceptNode(Expr);
                                    _ = try p.accept(.rparen);
                                    break :blk expr;
                                },
                                else => unreachable,
                            };
                        }
                    };
                    return Atom {
                        .expr = expr,
                    };
                }

            };
            return (try parser.acceptNode(Atom)).expr;

           
        }

        fn dump(self: Expr, w: anytype, level: usize) @TypeOf(w).Error!void {
            switch (self) {
                .binary => |binary| try binary.dump(w, level),
                .unary => |unary| try unary.dump(w, level),
                .cast => |cast| try cast.dump(w, level),
                .member_access => |member_access| try member_access.dump(w, level),
                .param_eval => |param_eval| try param_eval.dump(w, level),
                .identifier => |identifier| try dumpToken(w, level, "identifier", identifier),
                .num_literal => |num_literal| try dumpToken(w, level, "num_literal", num_literal),
                .module_def => |module_def| try module_def.dump(w, level),
                .import => |import| try import.dump(w, level),
            }
        }

    };

    pub const Binary = struct {
        lhs: *const Expr,
        rhs: *const Expr,
        op: Token,

        fn dump(self: Binary, w: anytype, level: usize) @TypeOf(w).Error!void {
            try dumpToken(w, level, "binary", self.op);
            try self.lhs.dump(w, level + 1);
            try self.rhs.dump(w, level + 1);
        }

    };

    pub const Unary = struct {
        subject: *const Expr,
        op: Token,

        fn dump(self: Unary, w: anytype, level: usize) @TypeOf(w).Error!void {
            try dumpToken(w, level, "unary", self.op);
            try self.subject.dump(w, level + 1);
        }

    };

    pub const Cast = struct {
        subject: *const Expr,
        target: *const Expr,

        fn dump(self: Cast, w: anytype, level: usize) @TypeOf(w).Error!void {
            try writeIndent(w, level, "cast");
            try self.subject.dump(w, level + 1);
            try self.target.dump(w, level + 1);
        }

    };

    pub const MemberAccess = struct {
        container: *const Expr,
        member_name: Token,

        fn dump(self: MemberAccess, w: anytype, level: usize) @TypeOf(w).Error!void {
            try dumpToken(w, level, "member", self.member_name);
            try self.container.dump(w, level + 1);
        }
    };

    pub const ParamEval = struct {
        subject: *const Expr,
        params: []const Expr,
        
        fn dump(self: ParamEval, w: anytype, level: usize) @TypeOf(w).Error!void {
            try writeIndent(w, level, "param_eval");
            try writeIndent(w, level + 1, "(subject)");
            try self.subject.dump(w, level + 2);
            try dumpList(w, level + 1, "(params)", Expr, self.params);
        }
    };

    pub const ModuleDef = struct {
        decls: []const LetDecl,

        const node_name = "module definition";
        const start_tags: []const TokenTag = &.{ .kw_module, };

        fn parse(p: *Parser) E!ModuleDef {
            _ = try p.accept(.kw_module);
            _ = try p.accept(.lcurly);
            const decls = try list(p, LetDecl);
            _ = try p.accept(.rcurly);
            return ModuleDef {
                .decls = decls,
            };
        }

        fn dump(self: ModuleDef, w: anytype, level: usize) @TypeOf(w).Error!void {
            try dumpList(w, level, "module_def", LetDecl, self.decls);
        }

    };

    pub const Import = struct {
        path: Token,

        const node_name = "import";
        const start_tags: []const TokenTag = &.{ .kw_import, };

        fn parse(p: *Parser) E!Import {
            _ = try p.accept(.kw_import);
            return Import {
                .path = try p.accept(.import_path),
            };
        }

        fn dump(self: Import, w: anytype, level: usize) @TypeOf(w).Error!void {
            try writeIndent(w, level, "import");
            try printIndent(w, level + 1, "(path) {s}", .{self.path.text});
        }

    };

};