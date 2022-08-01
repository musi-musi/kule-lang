const std = @import("std");
const source = @import("../source.zig");
const lexer = @import("lexer.zig");
const diagnostics = @import("../diagnostics.zig");
const types = @import("types.zig");
const expression = @import("expression.zig");
const compilation_unit = @import("compilation_unit.zig");

const CompilationUnit = compilation_unit.CompilationUnit;

const Value = types.Value;
const Type = types.Type;
const Source = source.Source;
const SourceLocation = source.SourceLocation;
const Token = lexer.Token;
const TokenTag = lexer.TokenTag;
const TokenStream = lexer.TokenStream;

const Allocator = std.mem.Allocator;
const ArenaAllocator = std.heap.ArenaAllocator;

const Diagnostics = diagnostics.Diagnostics;


const ExprNode = expression.ExprNode;
const Expr = expression.Expr;
const Module = expression.Module;
const Decl = expression.Decl;
const Param = expression.Param;
const TypeExpr = expression.TypeExpr;
const Function = expression.Function;



pub const ParserError = Parser.Error;

pub fn parseUnit(unit: *CompilationUnit) ParserError!void {
    var parser = try Parser.init(unit);
    return try parser.parse();
}


const Parser = struct {

    unit: *CompilationUnit,
    tokens: TokenStream,

    const Error = error {
        ParseFailed,
    } || TokenStream.Error || Allocator.Error;

    pub fn init(unit: *CompilationUnit) TokenStream.Error!Parser {
        return Parser {
            .unit = unit,
            .tokens = try TokenStream.init(unit.source, &unit.diagnostics),
        };
    }

    fn parse(self: *Parser) Error!void {
        var module = Module{
            .decls = try self.manyRule(LetDeclRule),
        };
        if ((try self.acceptOpt(.end_of_file)) == null) {
            self.sourceErrorUnexpectedToken(self.tokens.lookahead, LetDeclRule.name);
        }
        self.unit.root_module = module;
    }

    fn Rule(comptime parse_fn: anytype, comptime name_: []const u8, comptime start_tags_: []const TokenTag) type {
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

    fn acceptRule(p: *Parser, comptime N: type) Error!N.Result {
        try p.expectRule(N);
        return try N.parseResult(p);
    }

    fn acceptRuleOpt(p: *Parser, comptime N: type) Error!?N.Result {
        if (p.expectRuleOpt(N)) {
            return try N.parseResult(p);
        }
        else {
            return null;
        }
    }


    fn expectRule(p: *Parser, comptime N: type) Error!void {
        if (!p.expectRuleOpt(N)) {
            p.sourceErrorUnexpectedToken(p.tokens.lookahead, N.name);
            return Error.ParseFailed;
        }
    }

    fn expectRuleOpt(p: *Parser, comptime N: type) bool {
        return p.expectOneOpt(N.start_tags) != null;
    }

    fn manyRule(p: *Parser, comptime N: type) Error![]N.Result {
        var items = std.ArrayListUnmanaged(N.Result){};
        const allocator = p.astAllocator();
        while (try p.acceptRuleOpt(N)) |item| {
            try items.append(allocator, item);
        }
        return items.toOwnedSlice(allocator);
    }

    fn commaSepRule(p: *Parser, comptime N: type) Error![]N.Result {
        var items = std.ArrayListUnmanaged(N.Result){};
        const allocator = p.astAllocator();
        while (try p.acceptRuleOpt(N)) |item| {
            try items.append(allocator, item);
            if ((try p.acceptOpt(.comma)) == null) {
                break;
            }
        }
        return items.toOwnedSlice(allocator);
    }

    const LetDeclRule = Rule(pLetDecl, "let statement", &.{.kw_pub, .kw_let});

    fn pLetDecl(p: *Parser) Error!Decl {
        const is_pub = (try p.acceptOpt(.kw_pub)) != null;
        _ = try p.accept(.kw_let);
        var decl = try p.pDecl();
        decl.is_pub = is_pub;
        decl.where_clauses = try p.manyRule(WhereClauseRule);
        return decl;
    }

    fn pDecl(p: *Parser) Error!Decl {
        const name = try p.accept(.identifier);
        const params = try p.pParams();
        const type_expr = try p.acceptRuleOpt(TypeExprRule);
        const equal = try p.accept(.equal);
        const expr = try p.acceptRule(ExprRule);
        const function: ?*Function = blk: {
            if (params.len > 0) {
                const func = try p.astAllocator().create(Function);
                func.* = Function {
                    .params = params,
                    .expr = expr,
                    .type_expr = type_expr,
                };
                break :blk func;
            }
            else {
                break :blk null;
            }
        };
        return Decl {
            .name = name,
            .equal = equal,
            .is_pub = false,
            .expr = expr,
            .type_expr = type_expr,
            .where_clauses = &.{},
            .function = function,
        };
    }

    const WhereClauseRule = Rule(pWhereClause, "where clause", &.{.kw_where});

    fn pWhereClause(p: *Parser) Error!Decl {
        _ = try p.acceptOpt(.kw_where);
        return try p.pDecl();
    }

    fn pParams(p: *Parser) Error![]Param {
        if (try p.acceptOpt(.lparen)) |_| {
            const params = try p.commaSepRule(ParamRule);
            const rparen = try p.accept(.rparen);
            if (params.len == 0) {
                p.logError(rparen.text, "parameter list cannot be empty", .{});
            }
            for (params) |*param, i| {
                param.index = i;
            }
            return params;
        }
        else {
            return &[_]Param{};
        }
    }

    const ParamRule = Rule(pParam, "param definition", &.{.identifier});

    fn pParam(p: *Parser) Error!Param {
        const name = try p.accept(.identifier);
        return Param {
            .name = name,
            .type_expr = try p.acceptRule(TypeExprRule),
        };
    }

    const TypeExprRule = Rule(pTypeExpr, "type annotation", &[_]TokenTag{ .colon });

    fn pTypeExpr(p: *Parser) Error!TypeExpr {
        const colon = try p.accept(.colon);
        const expr = try p.acceptRule(ExprRule);
        return TypeExpr {
            .colon = colon,
            .expr = try p.allocExpr(expr),
        };
    }

    const ExprRule = Rule(pExpr, "expression", expr_start);
    const ExprSumRule = Rule(pExprSum, "expression", expr_start);
    const ExprProductRule = Rule(pExprProduct, "expression", expr_start);
    const ExprPrefixRule = Rule(pExprPrefix, "expression", expr_start);
    const ExprCastRule = Rule(pExprCast, "expression", atom_start);
    const ExprEvalParamsRule = Rule(pExprEvalParams, "expression", atom_start);
    const ExprDotMemberRule = Rule(pExprDotMember, "expression", atom_start);
    const ExprAtomRule = Rule(pExprAtom, "expression", atom_start);

    const expr_start: []const TokenTag = &[_]TokenTag{.plus, .minus} ++ atom_start;

    fn pExpr(p: *Parser) Error!ExprNode {
        return try p.pExprSum();
    }


    fn pExprSum(p: *Parser) Error!ExprNode {
        const lhs = try p.acceptRule(ExprProductRule);
        if (try p.acceptOneOpt(&.{.plus, .minus})) |operator| {
            const rhs = try p.acceptRule(ExprSumRule);
            return ExprNode.init(Expr.Binary {
                .operator = operator,
                .lhs = try p.allocExpr(lhs),
                .rhs = try p.allocExpr(rhs),
            });
        }
        else {
            return lhs;
        }
    }

    fn pExprProduct(p: *Parser) Error!ExprNode {
        const lhs = try p.acceptRule(ExprPrefixRule);
        if (try p.acceptOneOpt(&.{.aster, .fslash})) |operator| {
            const rhs = try p.acceptRule(ExprProductRule);
            return ExprNode.init(Expr.Binary {
                .operator = operator,
                .lhs = try p.allocExpr(lhs),
                .rhs = try p.allocExpr(rhs),
            });
        }
        else {
            return lhs;
        }
    }

    fn pExprPrefix(p: *Parser) Error!ExprNode {
        const operator_opt = try p.acceptOneOpt(&.{.plus, .minus});
        const operand = try p.acceptRule(ExprCastRule);
        if (operator_opt) |operator| {
            return ExprNode.init(Expr.Unary {
                .operator = operator,
                .operand = try p.allocExpr(operand),
            });
        }
        else {
            return operand;
        }
    }

    fn pExprCast(p: *Parser) Error!ExprNode {
        const lhs = try p.acceptRule(ExprEvalParamsRule);
        if (try p.acceptOneOpt(&.{.kw_as})) |operator| {
            const rhs = try p.acceptRule(ExprEvalParamsRule);
            return ExprNode.init(Expr.Binary {
                .operator = operator,
                .lhs = try p.allocExpr(lhs),
                .rhs = try p.allocExpr(rhs),
            });
        }
        else {
            return lhs;
        }
    }

    fn pExprEvalParams(p: *Parser) Error!ExprNode {
        const subject = try p.acceptRule(ExprDotMemberRule);
        if (try p.acceptOpt(.lparen)) |lparen| {
            const params = try p.commaSepRule(ExprRule);
            const rparen = try p.accept(.rparen);
            return ExprNode.init(Expr.EvalParams {
                .subject = try p.allocExpr(subject),
                .params = params,
                .lparen = lparen,
                .rparen = rparen,
            });
        }
        else {
            return subject;
        }
    }

    fn pExprDotMember(p: *Parser) Error!ExprNode {
        const container = try p.acceptRule(ExprAtomRule);
        if (try p.acceptOpt(.dot)) |dot| {
            const member_name = try p.accept(.identifier);
            return ExprNode.init(Expr.DotMember{
                .container = try p.allocExpr(container),
                .member_name = member_name,
                .dot = dot,
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

    fn pExprAtom(p: *Parser) Error!ExprNode {
        const token = try p.acceptOne(atom_start);
        switch (token.tag) {
            .lparen => {
                const expr = try p.acceptRule(ExprRule);
                _ = try p.accept(.rparen);
                return expr;
            },
            .identifier => {
                return ExprNode.init(Expr.Name{
                    .name = token,
                });
            },
            .number => {
                const value = blk: {
                    if (std.fmt.parseInt(u32, token.text, 10) catch null) |unsigned| {
                        break :blk Value.init(unsigned, types.dynamicNumberType(.unsigned, .scalar));
                    }
                    else if (std.fmt.parseInt(i32, token.text, 10) catch null) |signed| {
                        break :blk Value.init(signed, types.dynamicNumberType(.signed, .scalar));
                    }
                    else if (std.fmt.parseFloat(f32, token.text) catch null) |float| {
                        break :blk Value.init(float, types.dynamicNumberType(.float, .scalar));
                    }
                    else {
                        break :blk Value.init(@as(u32, 0), types.dynamicNumberType(.unsigned, .scalar));
                    }
                };
                return ExprNode.init(Expr.NumLit {
                    .literal = token,
                    .value = value,
                });
            },
            .kw_module => {
                if (try p.acceptOpt(.lcurly)) |lcurly| {
                    const decls = try p.manyRule(LetDeclRule);
                    const rcurly = try p.accept(.rcurly);
                    return ExprNode.init(Expr.ModuleDef{
                        .module = Module {
                            .decls = decls,
                        },
                        .kw_module = token,
                        .lcurly = lcurly,
                        .rcurly = rcurly,
                    });
                }
                else {
                    return ExprNode.init(Expr.Name{
                        .name = token,
                    });
                }
            },
            .kw_import => {
                return ExprNode.init(Expr.Import {
                    .kw_import = token,
                    .path = try p.accept(.import_path),
                });
            },
            else => unreachable,
        }
    }

    fn allocExpr(p: *Parser, expr: ExprNode) Error!*ExprNode {
        const expr_ptr = try p.astAllocator().create(ExprNode);
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
        return self.unit.arena.allocator();
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
            self.logError(token.text, "expected {s}, found end of file", .{expected_desc});
        }
        else {
            self.logError(token.text, "expected {s}, found \"{s}\"", .{expected_desc, token.text});
        }
    }

    fn logError(self: *Parser, token: []const u8, comptime format: []const u8, args: anytype) void {
        self.unit.diagnostics.logError(token, format, args);
    }

};