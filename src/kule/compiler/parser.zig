const std = @import("std");
const source = @import("../source.zig");
const lexer = @import("lexer.zig");
const log = @import("../log.zig");
const diagnostics = @import("../diagnostics.zig");
const types = @import("types.zig");
const expression = @import("expression.zig");


const Type = types.Type;
const Source = source.Source;
const SourceLocation = source.SourceLocation;
const Token = lexer.Token;
const TokenTag = lexer.TokenTag;
const TokenStream = lexer.TokenStream;

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Diagnostics = diagnostics.Diagnostics;


const SourceModule = expression.SourceModule;
const Expr = expression.Expr;
const ExprVal = expression.ExprVal;
const Module = expression.Module;
const Decl = expression.Decl;
const ParamDef = expression.ParamDef;



pub const ParserError = Parser.Error;

pub fn parseSource(allocator: Allocator, src: *const Source, diags: ?*Diagnostics) ParserError!SourceModule {
    var parser = try Parser.init(allocator, src, diags);
    errdefer parser.deinit();
    return try parser.parse();
}


const Parser = struct {

    allocator: Allocator,
    arena: std.heap.ArenaAllocator,
    source: *const Source,
    tokens: TokenStream,
    error_count: usize = 0,
    diagnostics: ?*Diagnostics,

    const Error = error {
        ParseFailed,
    } || TokenStream.Error || Allocator.Error;

    pub fn init(allocator: Allocator, src: *const Source, diags: ?*Diagnostics) TokenStream.Error!Parser {
        return Parser {
            .allocator = allocator,
            .arena = ArenaAllocator.init(allocator),
            .source = src,
            .tokens = try TokenStream.init(src, diags),
            .diagnostics = diags,
        };
    }

    pub fn deinit(self: Parser) void {
        self.arena.deinit();
    }

    fn parse(self: *Parser) !SourceModule {
        var module = Module{
            .decls = try self.manyNode(LetDeclNode),
        };
        if ((try self.acceptOpt(.end_of_file)) == null) {
            self.sourceErrorUnexpectedToken(self.tokens.lookahead, LetDeclNode.name);
        }
        self.error_count += self.tokens.error_count;
        if (self.error_count > 0) {
            return Error.ParseFailed;
        }
        return SourceModule {
            .source = self.source,
            .arena = self.arena,
            .module = module,
        };
    }

    fn Node(comptime parse_fn: anytype, comptime name_: []const u8, comptime start_tags_: []const TokenTag) type {
        return struct {

            const Result = blk: {
                const ParseFn = @TypeOf(parse_fn);
                const Return = @typeInfo(ParseFn).Fn.return_type.?;
                break :blk @typeInfo(Return).ErrorUnion.payload;
            };

            const name = name_;
            const start_tags = start_tags_;

            const parseResult = parse_fn;

        };
    }

    fn acceptNode(p: *Parser, comptime N: type) Error!N.Result {
        try p.expectNode(N);
        return try N.parseResult(p);
    }

    fn acceptNodeOpt(p: *Parser, comptime N: type) Error!?N.Result {
        if (p.expectNodeOpt(N)) {
            return try N.parseResult(p);
        }
        else {
            return null;
        }
    }


    fn expectNode(p: *Parser, comptime N: type) Error!void {
        if (!p.expectNodeOpt(N)) {
            p.sourceErrorUnexpectedToken(p.tokens.lookahead, N.name);
            return Error.ParseFailed;
        }
    }

    fn expectNodeOpt(p: *Parser, comptime N: type) bool {
        return p.expectOneOpt(N.start_tags) != null;
    }

    fn manyNode(p: *Parser, comptime N: type) Error![]N.Result {
        var items = std.ArrayListUnmanaged(N.Result){};
        const allocator = p.arena.allocator();
        while (try p.acceptNodeOpt(N)) |item| {
            try items.append(allocator, item);
        }
        return items.toOwnedSlice(allocator);
    }

    fn commaSepNode(p: *Parser, comptime N: type) Error![]N.Result {
        var items = std.ArrayListUnmanaged(N.Result){};
        const allocator = p.arena.allocator();
        while (try p.acceptNodeOpt(N)) |item| {
            try items.append(allocator, item);
            if ((try p.acceptOpt(.comma)) == null) {
                break;
            }
        }
        return items.toOwnedSlice(allocator);
    }

    const LetDeclNode = Node(pLetDecl, "let statement", &.{.kw_pub, .kw_let});

    fn pLetDecl(p: *Parser) Error!Decl {
        const is_pub = (try p.acceptOpt(.kw_pub)) != null;
        _ = try p.accept(.kw_let);
        var decl = try p.pDecl();
        decl.is_pub = is_pub;
        decl.where_clauses = try p.manyNode(WhereClauseNode);
        return decl;
    }

    fn pDecl(p: *Parser) Error!Decl {
        const name = try p.accept(.identifier);
        const params = try p.pParamDefs();
        const type_expr = try p.pTypeAnnotation();
        _ = try p.accept(.equal);
        const expr = try p.acceptNode(ExprNode);
        return Decl {
            .name = name,
            .is_pub = false,
            .expr = expr,
            .params = params,
            .type_expr = type_expr,
            .where_clauses = &.{},
        };
    }

    const WhereClauseNode = Node(pWhereClause, "where clause", &.{.kw_where});

    fn pWhereClause(p: *Parser) Error!Decl {
        _ = try p.acceptOpt(.kw_where);
        return try p.pDecl();
    }

    fn pParamDefs(p: *Parser) Error![]ParamDef {
        if (try p.acceptOpt(.lparen)) |_| {
            const params = try p.commaSepNode(ParamDefNode);
            const rparen = try p.accept(.rparen);
            if (params.len == 0) {
                p.sourceError(rparen.text, "parameter list cannot be empty", .{});
            }
            for (params) |*param, i| {
                param.index = i;
            }
            return params;
        }
        else {
            return &[_]ParamDef{};
        }
    }

    const ParamDefNode = Node(pParamDef, "param definition", &.{.identifier});

    fn pParamDef(p: *Parser) Error!ParamDef {
        const name = try p.accept(.identifier);
        return ParamDef {
            .name = name,
            .type_expr = try p.pTypeAnnotation(),
        };
    }

    fn pTypeAnnotation(p: *Parser) Error!?Expr {
        if (try p.acceptOpt(.colon)) |_| {
            return try p.acceptNode(ExprNode);
        }
        else {
            return null;
        }
    }

    const ExprNode = Node(pExpr, "expression", expr_start);
    const ExprSumNode = Node(pExprSum, "expression", expr_start);
    const ExprProductNode = Node(pExprProduct, "expression", expr_start);
    const ExprPrefixNode = Node(pExprPrefix, "expression", expr_start);
    const ExprCastNode = Node(pExprCast, "expression", atom_start);
    const ExprEvalParamsNode = Node(pExprEvalParams, "expression", atom_start);
    const ExprDotMemberNode = Node(pExprDotMember, "expression", atom_start);
    const ExprAtomNode = Node(pExprAtom, "expression", atom_start);

    const expr_start: []const TokenTag = &[_]TokenTag{.plus, .minus} ++ atom_start;

    fn pExpr(p: *Parser) Error!Expr {
        return try p.pExprSum();
    }


    fn pExprSum(p: *Parser) Error!Expr {
        const lhs = try p.acceptNode(ExprProductNode);
        if (try p.acceptOneOpt(&.{.plus, .minus})) |operator| {
            const rhs = try p.acceptNode(ExprSumNode);
            return Expr.init(ExprVal.Binary {
                .operator = operator,
                .lhs = try p.allocExpr(lhs),
                .rhs = try p.allocExpr(rhs),
            });
        }
        else {
            return lhs;
        }
    }

    fn pExprProduct(p: *Parser) Error!Expr {
        const lhs = try p.acceptNode(ExprPrefixNode);
        if (try p.acceptOneOpt(&.{.aster, .fslash})) |operator| {
            const rhs = try p.acceptNode(ExprProductNode);
            return Expr.init(ExprVal.Binary {
                .operator = operator,
                .lhs = try p.allocExpr(lhs),
                .rhs = try p.allocExpr(rhs),
            });
        }
        else {
            return lhs;
        }
    }

    fn pExprPrefix(p: *Parser) Error!Expr {
        const operator_opt = try p.acceptOneOpt(&.{.plus, .minus});
        const operand = try p.acceptNode(ExprCastNode);
        if (operator_opt) |operator| {
            return Expr.init(ExprVal.Unary {
                .operator = operator,
                .operand = try p.allocExpr(operand),
            });
        }
        else {
            return operand;
        }
    }

    fn pExprCast(p: *Parser) Error!Expr {
        const lhs = try p.acceptNode(ExprEvalParamsNode);
        if (try p.acceptOneOpt(&.{.kw_as})) |operator| {
            const rhs = try p.acceptNode(ExprEvalParamsNode);
            return Expr.init(ExprVal.Binary {
                .operator = operator,
                .lhs = try p.allocExpr(lhs),
                .rhs = try p.allocExpr(rhs),
            });
        }
        else {
            return lhs;
        }
    }

    fn pExprEvalParams(p: *Parser) Error!Expr {
        const subject = try p.acceptNode(ExprDotMemberNode);
        if (try p.acceptOpt(.lparen)) |_| {
            const params = try p.commaSepNode(ExprNode);
            _ = try p.accept(.rparen);
            return Expr.init(ExprVal.EvalParams {
                .subject = try p.allocExpr(subject),
                .params = params,
            });
        }
        else {
            return subject;
        }
    }

    fn pExprDotMember(p: *Parser) Error!Expr {
        const container = try p.acceptNode(ExprAtomNode);
        if (try p.acceptOpt(.dot)) |_| {
            const member_name = try p.accept(.identifier);
            return Expr.init(ExprVal.DotMember{
                .container = try p.allocExpr(container),
                .member_name = member_name,
            });
        }
        else {
            return container;
        }
    }

    const atom_start: []const TokenTag = &.{
        .lparen,
        .identifier, .number, 
        .kw_module, .kw_import,
    };

    fn pExprAtom(p: *Parser) Error!Expr {
        const token = try p.acceptOne(atom_start);
        switch (token.tag) {
            .lparen => {
                const expr = try p.acceptNode(ExprNode);
                _ = try p.accept(.rparen);
                return expr;
            },
            .identifier => {
                return Expr.init(ExprVal.Name{
                    .name = token,
                });
            },
            .number => {
                const value = std.fmt.parseFloat(comptime types.number_type.Val(), token.text) catch 0;
                return Expr.init(ExprVal.NumLit {
                    .literal = token,
                    .value = value,
                });
            },
            .kw_module => {
                if (try p.acceptOpt(.lcurly)) |_| {
                    const decls = try p.manyNode(LetDeclNode);
                    _ = try p.accept(.rcurly);
                    return Expr.init(ExprVal.ModuleDef{
                        .module = Module {
                            .decls = decls,
                        },
                    });
                }
                else {
                    return Expr.init(ExprVal.Name{
                        .name = token,
                    });
                }
            },
            .kw_import => {
                return Expr.init(ExprVal.Import {
                    .path = try p.accept(.import_path),
                });
            },
            else => unreachable,
        }
    }

    fn allocExpr(p: *Parser, expr: Expr) Error!*Expr {
        const expr_ptr = try p.arena.allocator().create(Expr);
        expr_ptr.* = expr;
        return expr_ptr;
    }

    fn acceptEof(self: *Parser) Error!void {
        if (self.tokens.next()) |token| {
            self.sourceErrorUnexpectedToken(token, "end of file");
            return Error.ParseFailed;
        }
    }


    fn astAllocator(self: *Parser) Allocator {
        return self.arena.?.allocator();
    }

    fn isAtEof(self: *Parser) bool {
        return self.tokens.lookahead.tag == .end_of_file;
    }

    fn expect(self: *Parser, comptime tag: TokenTag) Error!Token {
        return self.expectOne(&.{tag});
    }

    fn expectOpt(self: Parser, comptime tag: TokenTag) ?Token {
        return self.expectOneOpt(&.{tag});
    }

    fn expectOne(self: *Parser, comptime tags: []const TokenTag) Error!Token {
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

    fn expectOneOpt(self: Parser, comptime tags: []const TokenTag) ?Token {
        const next = self.tokens.lookahead;
        inline for (tags) |tag| {
            if (next.tag == tag) {
                return next;
            }
        }
        return null;
    }

    fn accept(self: *Parser, comptime tag: TokenTag) Error!Token {
        return self.acceptOne(&[1]TokenTag{tag});
    }

    fn acceptOpt(self: *Parser, comptime tag: TokenTag) TokenStream.Error!?Token {
        return self.acceptOneOpt(&[1]TokenTag{tag});
    }

    fn acceptOne(self: *Parser, comptime tags: []const TokenTag) Error!Token {
        const token = self.expectOne(tags);
        _ = try self.tokens.tryNext();
        return token;
    }

    fn acceptOneOpt(self: *Parser, comptime tags: []const TokenTag) TokenStream.Error!?Token {
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

    fn sourceError(self: *Parser, token: []const u8, comptime format: []const u8, args: anytype) void {
        self.error_count += 1;
        if (self.diagnostics) |diags| {
            diags.sourceErrorErrorPanic(self.tokens.source, token, format, args);
        }
    }





};